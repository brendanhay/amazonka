{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.NumberValidateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.NumberValidateResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a phone number.
--
-- /See:/ 'newNumberValidateResponse' smart constructor.
data NumberValidateResponse = NumberValidateResponse'
  { -- | The description of the phone type. Valid values are: MOBILE, LANDLINE,
    -- VOIP, INVALID, PREPAID, and OTHER.
    phoneType :: Prelude.Maybe Prelude.Text,
    -- | The phone number that was sent in the request body.
    originalPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The postal or ZIP code for the location where the phone number was
    -- originally registered.
    zipCode :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in
    -- the request body.
    originalCountryCodeIso2 :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region where the phone number was originally registered.
    countryCodeIso2 :: Prelude.Maybe Prelude.Text,
    -- | The name of the county where the phone number was originally registered.
    county :: Prelude.Maybe Prelude.Text,
    -- | The name of the city where the phone number was originally registered.
    city :: Prelude.Maybe Prelude.Text,
    -- | The carrier or service provider that the phone number is currently
    -- registered with. In some countries and regions, this value may be the
    -- carrier or service provider that the phone number was originally
    -- registered with.
    carrier :: Prelude.Maybe Prelude.Text,
    -- | The phone type, represented by an integer. Valid values are: 0 (mobile),
    -- 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
    phoneTypeCode :: Prelude.Maybe Prelude.Int,
    -- | The cleansed phone number, in the format for the location where the
    -- phone number was originally registered.
    cleansedPhoneNumberNational :: Prelude.Maybe Prelude.Text,
    -- | The cleansed phone number, in E.164 format, for the location where the
    -- phone number was originally registered.
    cleansedPhoneNumberE164 :: Prelude.Maybe Prelude.Text,
    -- | The numeric code for the country or region where the phone number was
    -- originally registered.
    countryCodeNumeric :: Prelude.Maybe Prelude.Text,
    -- | The time zone for the location where the phone number was originally
    -- registered.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The name of the country or region where the phone number was originally
    -- registered.
    country :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NumberValidateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneType', 'numberValidateResponse_phoneType' - The description of the phone type. Valid values are: MOBILE, LANDLINE,
-- VOIP, INVALID, PREPAID, and OTHER.
--
-- 'originalPhoneNumber', 'numberValidateResponse_originalPhoneNumber' - The phone number that was sent in the request body.
--
-- 'zipCode', 'numberValidateResponse_zipCode' - The postal or ZIP code for the location where the phone number was
-- originally registered.
--
-- 'originalCountryCodeIso2', 'numberValidateResponse_originalCountryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, that was sent in
-- the request body.
--
-- 'countryCodeIso2', 'numberValidateResponse_countryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
--
-- 'county', 'numberValidateResponse_county' - The name of the county where the phone number was originally registered.
--
-- 'city', 'numberValidateResponse_city' - The name of the city where the phone number was originally registered.
--
-- 'carrier', 'numberValidateResponse_carrier' - The carrier or service provider that the phone number is currently
-- registered with. In some countries and regions, this value may be the
-- carrier or service provider that the phone number was originally
-- registered with.
--
-- 'phoneTypeCode', 'numberValidateResponse_phoneTypeCode' - The phone type, represented by an integer. Valid values are: 0 (mobile),
-- 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
--
-- 'cleansedPhoneNumberNational', 'numberValidateResponse_cleansedPhoneNumberNational' - The cleansed phone number, in the format for the location where the
-- phone number was originally registered.
--
-- 'cleansedPhoneNumberE164', 'numberValidateResponse_cleansedPhoneNumberE164' - The cleansed phone number, in E.164 format, for the location where the
-- phone number was originally registered.
--
-- 'countryCodeNumeric', 'numberValidateResponse_countryCodeNumeric' - The numeric code for the country or region where the phone number was
-- originally registered.
--
-- 'timezone', 'numberValidateResponse_timezone' - The time zone for the location where the phone number was originally
-- registered.
--
-- 'country', 'numberValidateResponse_country' - The name of the country or region where the phone number was originally
-- registered.
newNumberValidateResponse ::
  NumberValidateResponse
newNumberValidateResponse =
  NumberValidateResponse'
    { phoneType =
        Prelude.Nothing,
      originalPhoneNumber = Prelude.Nothing,
      zipCode = Prelude.Nothing,
      originalCountryCodeIso2 = Prelude.Nothing,
      countryCodeIso2 = Prelude.Nothing,
      county = Prelude.Nothing,
      city = Prelude.Nothing,
      carrier = Prelude.Nothing,
      phoneTypeCode = Prelude.Nothing,
      cleansedPhoneNumberNational = Prelude.Nothing,
      cleansedPhoneNumberE164 = Prelude.Nothing,
      countryCodeNumeric = Prelude.Nothing,
      timezone = Prelude.Nothing,
      country = Prelude.Nothing
    }

-- | The description of the phone type. Valid values are: MOBILE, LANDLINE,
-- VOIP, INVALID, PREPAID, and OTHER.
numberValidateResponse_phoneType :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_phoneType = Lens.lens (\NumberValidateResponse' {phoneType} -> phoneType) (\s@NumberValidateResponse' {} a -> s {phoneType = a} :: NumberValidateResponse)

-- | The phone number that was sent in the request body.
numberValidateResponse_originalPhoneNumber :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_originalPhoneNumber = Lens.lens (\NumberValidateResponse' {originalPhoneNumber} -> originalPhoneNumber) (\s@NumberValidateResponse' {} a -> s {originalPhoneNumber = a} :: NumberValidateResponse)

-- | The postal or ZIP code for the location where the phone number was
-- originally registered.
numberValidateResponse_zipCode :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_zipCode = Lens.lens (\NumberValidateResponse' {zipCode} -> zipCode) (\s@NumberValidateResponse' {} a -> s {zipCode = a} :: NumberValidateResponse)

-- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in
-- the request body.
numberValidateResponse_originalCountryCodeIso2 :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_originalCountryCodeIso2 = Lens.lens (\NumberValidateResponse' {originalCountryCodeIso2} -> originalCountryCodeIso2) (\s@NumberValidateResponse' {} a -> s {originalCountryCodeIso2 = a} :: NumberValidateResponse)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
numberValidateResponse_countryCodeIso2 :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_countryCodeIso2 = Lens.lens (\NumberValidateResponse' {countryCodeIso2} -> countryCodeIso2) (\s@NumberValidateResponse' {} a -> s {countryCodeIso2 = a} :: NumberValidateResponse)

-- | The name of the county where the phone number was originally registered.
numberValidateResponse_county :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_county = Lens.lens (\NumberValidateResponse' {county} -> county) (\s@NumberValidateResponse' {} a -> s {county = a} :: NumberValidateResponse)

-- | The name of the city where the phone number was originally registered.
numberValidateResponse_city :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_city = Lens.lens (\NumberValidateResponse' {city} -> city) (\s@NumberValidateResponse' {} a -> s {city = a} :: NumberValidateResponse)

-- | The carrier or service provider that the phone number is currently
-- registered with. In some countries and regions, this value may be the
-- carrier or service provider that the phone number was originally
-- registered with.
numberValidateResponse_carrier :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_carrier = Lens.lens (\NumberValidateResponse' {carrier} -> carrier) (\s@NumberValidateResponse' {} a -> s {carrier = a} :: NumberValidateResponse)

-- | The phone type, represented by an integer. Valid values are: 0 (mobile),
-- 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
numberValidateResponse_phoneTypeCode :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Int)
numberValidateResponse_phoneTypeCode = Lens.lens (\NumberValidateResponse' {phoneTypeCode} -> phoneTypeCode) (\s@NumberValidateResponse' {} a -> s {phoneTypeCode = a} :: NumberValidateResponse)

-- | The cleansed phone number, in the format for the location where the
-- phone number was originally registered.
numberValidateResponse_cleansedPhoneNumberNational :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_cleansedPhoneNumberNational = Lens.lens (\NumberValidateResponse' {cleansedPhoneNumberNational} -> cleansedPhoneNumberNational) (\s@NumberValidateResponse' {} a -> s {cleansedPhoneNumberNational = a} :: NumberValidateResponse)

-- | The cleansed phone number, in E.164 format, for the location where the
-- phone number was originally registered.
numberValidateResponse_cleansedPhoneNumberE164 :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_cleansedPhoneNumberE164 = Lens.lens (\NumberValidateResponse' {cleansedPhoneNumberE164} -> cleansedPhoneNumberE164) (\s@NumberValidateResponse' {} a -> s {cleansedPhoneNumberE164 = a} :: NumberValidateResponse)

-- | The numeric code for the country or region where the phone number was
-- originally registered.
numberValidateResponse_countryCodeNumeric :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_countryCodeNumeric = Lens.lens (\NumberValidateResponse' {countryCodeNumeric} -> countryCodeNumeric) (\s@NumberValidateResponse' {} a -> s {countryCodeNumeric = a} :: NumberValidateResponse)

-- | The time zone for the location where the phone number was originally
-- registered.
numberValidateResponse_timezone :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_timezone = Lens.lens (\NumberValidateResponse' {timezone} -> timezone) (\s@NumberValidateResponse' {} a -> s {timezone = a} :: NumberValidateResponse)

-- | The name of the country or region where the phone number was originally
-- registered.
numberValidateResponse_country :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_country = Lens.lens (\NumberValidateResponse' {country} -> country) (\s@NumberValidateResponse' {} a -> s {country = a} :: NumberValidateResponse)

instance Prelude.FromJSON NumberValidateResponse where
  parseJSON =
    Prelude.withObject
      "NumberValidateResponse"
      ( \x ->
          NumberValidateResponse'
            Prelude.<$> (x Prelude..:? "PhoneType")
            Prelude.<*> (x Prelude..:? "OriginalPhoneNumber")
            Prelude.<*> (x Prelude..:? "ZipCode")
            Prelude.<*> (x Prelude..:? "OriginalCountryCodeIso2")
            Prelude.<*> (x Prelude..:? "CountryCodeIso2")
            Prelude.<*> (x Prelude..:? "County")
            Prelude.<*> (x Prelude..:? "City")
            Prelude.<*> (x Prelude..:? "Carrier")
            Prelude.<*> (x Prelude..:? "PhoneTypeCode")
            Prelude.<*> (x Prelude..:? "CleansedPhoneNumberNational")
            Prelude.<*> (x Prelude..:? "CleansedPhoneNumberE164")
            Prelude.<*> (x Prelude..:? "CountryCodeNumeric")
            Prelude.<*> (x Prelude..:? "Timezone")
            Prelude.<*> (x Prelude..:? "Country")
      )

instance Prelude.Hashable NumberValidateResponse

instance Prelude.NFData NumberValidateResponse
