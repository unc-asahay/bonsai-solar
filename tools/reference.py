#!/usr/bin/env python3
"""The solar model, written a second time, in a different language.

This exists to disagree with the OCaml if it can. The OCaml is the
thing that ships; this is the thing that checks it. Both print the same
keys, and the gate compares the two sets.

    python3 tools/reference.py

The model: standard solar geometry (Cooper's declination, Spencer's
equation of time, the sun's east-north-up vector) under a Meinel
clear-sky sky, transposed onto an arbitrarily tilted and oriented
surface with an isotropic sky and ground reflection.
"""

import math

GSC = 1367.0
DIFFUSE_FRACTION = 0.10
ALBEDO = 0.20
DAYS_IN_MONTH = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


def declination(day):
    return math.radians(23.45 * math.sin(math.radians((360 * (day + 284)) / 365)))


def equation_of_time(day):
    b = math.radians((360 * (day - 81)) / 365)
    return 9.87 * math.sin(2 * b) - 7.53 * math.cos(b) - 1.5 * math.sin(b)


def sun_vector(lat, lon, tz, day, minutes):
    phi = math.radians(lat)
    d = declination(day)
    solar = minutes + 4 * (15 * tz - lon) + equation_of_time(day)
    omega = math.radians(15 * (solar / 60 - 12))
    return (
        -math.cos(d) * math.sin(omega),
        math.sin(d) * math.cos(phi) - math.cos(d) * math.sin(phi) * math.cos(omega),
        math.sin(phi) * math.sin(d) + math.cos(phi) * math.cos(d) * math.cos(omega),
    )


def air_mass(elevation_rad):
    z = 90 - math.degrees(elevation_rad)
    if z >= 90:
        return math.inf
    return 1 / (math.cos(math.radians(z)) + 0.50572 * ((96.07995 - z) ** -1.6364))


def extraterrestrial(day):
    return GSC * (1 + 0.033 * math.cos((2 * math.pi * day) / 365))


def plane_of_array(lat, lon, tz, day, minutes, tilt, azimuth):
    east, north, up = sun_vector(lat, lon, tz, day, minutes)
    elevation = math.asin(max(-1.0, min(1.0, up)))
    am = air_mass(elevation)
    if not math.isfinite(am):
        return 0.0
    dni = extraterrestrial(day) * (0.7 ** (am ** 0.678))
    if dni <= 0:
        return 0.0
    cos_z = math.cos(math.pi / 2 - elevation)
    ghi = (dni * cos_z) / (1 - DIFFUSE_FRACTION)
    dhi = ghi * DIFFUSE_FRACTION
    b = math.radians(tilt)
    g = math.radians(azimuth)
    ne = math.sin(b) * math.sin(g)
    nn = math.sin(b) * math.cos(g)
    nu = math.cos(b)
    cos_incidence = east * ne + north * nn + up * nu
    direct = dni * cos_incidence if cos_incidence > 0 else 0.0
    diffuse = dhi * ((1 + math.cos(b)) / 2)
    reflected = ghi * ALBEDO * ((1 - math.cos(b)) / 2)
    return direct + diffuse + reflected


def annual_poa(lat, lon, tz, tilt, azimuth, step=10.0):
    total = 0.0
    doy = 0
    for days in DAYS_IN_MONTH:
        for day in range(1, days + 1):
            minutes = 0.0
            while minutes < 1440.0:
                total += plane_of_array(lat, lon, tz, doy + day, minutes, tilt, azimuth) * (step / 60.0)
                minutes += step
        doy += days
    return total / 1000.0


def annual_horizontal_extraterrestrial(lat, lon, tz, step=10.0):
    total = 0.0
    doy = 0
    for days in DAYS_IN_MONTH:
        for day in range(1, days + 1):
            minutes = 0.0
            while minutes < 1440.0:
                east, north, up = sun_vector(lat, lon, tz, doy + day, minutes)
                if up > 0:
                    total += extraterrestrial(doy + day) * math.cos(math.pi / 2 - math.asin(up)) * (step / 60.0)
                minutes += step
        doy += days
    return total / 1000.0


DENVER = (39.7392, -104.9903, -7.0)
MIAMI = (25.7617, -80.1918, -5.0)
SYDNEY = (-33.8688, 151.2093, 10.0)


def main():
    print(f"annual_poa_denver_tilt39 = {annual_poa(*DENVER, 39.0, 180.0):.1f}")
    print(f"annual_poa_denver_tilt0 = {annual_poa(*DENVER, 0.0, 180.0):.1f}")
    print(f"annual_poa_denver_tilt20 = {annual_poa(*DENVER, 20.0, 180.0):.1f}")
    print(f"annual_poa_denver_north = {annual_poa(*DENVER, 39.0, 0.0):.1f}")
    print(f"annual_poa_miami_tilt39 = {annual_poa(*MIAMI, 39.0, 180.0):.1f}")
    print(f"annual_poa_sydney_tilt39 = {annual_poa(*SYDNEY, 39.0, 180.0):.1f}")
    print(f"annual_horizontal_extraterrestrial_40n = {annual_horizontal_extraterrestrial(40.0, -105.0, -7.0):.1f}")
    print(f"annual_horizontal_extraterrestrial_equator = {annual_horizontal_extraterrestrial(0.0, 0.0, 0.0):.1f}")


if __name__ == "__main__":
    main()
