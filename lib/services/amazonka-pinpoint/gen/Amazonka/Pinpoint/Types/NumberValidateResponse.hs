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
-- Module      : Amazonka.Pinpoint.Types.NumberValidateResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.NumberValidateResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a phone number.
--
-- /See:/ 'newNumberValidateResponse' smart constructor.
data NumberValidateResponse = NumberValidateResponse'
  { -- | The carrier or service provider that the phone number is currently
    -- registered with. In some countries and regions, this value may be the
    -- carrier or service provider that the phone number was originally
    -- registered with.
    carrier :: Prelude.Maybe Prelude.Text,
    -- | The name of the city where the phone number was originally registered.
    city :: Prelude.Maybe Prelude.Text,
    -- | The cleansed phone number, in E.164 format, for the location where the
    -- phone number was originally registered.
    cleansedPhoneNumberE164 :: Prelude.Maybe Prelude.Text,
    -- | The cleansed phone number, in the format for the location where the
    -- phone number was originally registered.
    cleansedPhoneNumberNational :: Prelude.Maybe Prelude.Text,
    -- | The name of the country or region where the phone number was originally
    -- registered.
    country :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region where the phone number was originally registered.
    countryCodeIso2 :: Prelude.Maybe Prelude.Text,
    -- | The numeric code for the country or region where the phone number was
    -- originally registered.
    countryCodeNumeric :: Prelude.Maybe Prelude.Text,
    -- | The name of the county where the phone number was originally registered.
    county :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in
    -- the request body.
    originalCountryCodeIso2 :: Prelude.Maybe Prelude.Text,
    -- | The phone number that was sent in the request body.
    originalPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The description of the phone type. Valid values are: MOBILE, LANDLINE,
    -- VOIP, INVALID, PREPAID, and OTHER.
    phoneType :: Prelude.Maybe Prelude.Text,
    -- | The phone type, represented by an integer. Valid values are: 0 (mobile),
    -- 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
    phoneTypeCode :: Prelude.Maybe Prelude.Int,
    -- | The time zone for the location where the phone number was originally
    -- registered.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The postal or ZIP code for the location where the phone number was
    -- originally registered.
    zipCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumberValidateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrier', 'numberValidateResponse_carrier' - The carrier or service provider that the phone number is currently
-- registered with. In some countries and regions, this value may be the
-- carrier or service provider that the phone number was originally
-- registered with.
--
-- 'city', 'numberValidateResponse_city' - The name of the city where the phone number was originally registered.
--
-- 'cleansedPhoneNumberE164', 'numberValidateResponse_cleansedPhoneNumberE164' - The cleansed phone number, in E.164 format, for the location where the
-- phone number was originally registered.
--
-- 'cleansedPhoneNumberNational', 'numberValidateResponse_cleansedPhoneNumberNational' - The cleansed phone number, in the format for the location where the
-- phone number was originally registered.
--
-- 'country', 'numberValidateResponse_country' - The name of the country or region where the phone number was originally
-- registered.
--
-- 'countryCodeIso2', 'numberValidateResponse_countryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
--
-- 'countryCodeNumeric', 'numberValidateResponse_countryCodeNumeric' - The numeric code for the country or region where the phone number was
-- originally registered.
--
-- 'county', 'numberValidateResponse_county' - The name of the county where the phone number was originally registered.
--
-- 'originalCountryCodeIso2', 'numberValidateResponse_originalCountryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, that was sent in
-- the request body.
--
-- 'originalPhoneNumber', 'numberValidateResponse_originalPhoneNumber' - The phone number that was sent in the request body.
--
-- 'phoneType', 'numberValidateResponse_phoneType' - The description of the phone type. Valid values are: MOBILE, LANDLINE,
-- VOIP, INVALID, PREPAID, and OTHER.
--
-- 'phoneTypeCode', 'numberValidateResponse_phoneTypeCode' - The phone type, represented by an integer. Valid values are: 0 (mobile),
-- 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
--
-- 'timezone', 'numberValidateResponse_timezone' - The time zone for the location where the phone number was originally
-- registered.
--
-- 'zipCode', 'numberValidateResponse_zipCode' - The postal or ZIP code for the location where the phone number was
-- originally registered.
newNumberValidateResponse ::
  NumberValidateResponse
newNumberValidateResponse =
  NumberValidateResponse'
    { carrier = Prelude.Nothing,
      city = Prelude.Nothing,
      cleansedPhoneNumberE164 = Prelude.Nothing,
      cleansedPhoneNumberNational = Prelude.Nothing,
      country = Prelude.Nothing,
      countryCodeIso2 = Prelude.Nothing,
      countryCodeNumeric = Prelude.Nothing,
      county = Prelude.Nothing,
      originalCountryCodeIso2 = Prelude.Nothing,
      originalPhoneNumber = Prelude.Nothing,
      phoneType = Prelude.Nothing,
      phoneTypeCode = Prelude.Nothing,
      timezone = Prelude.Nothing,
      zipCode = Prelude.Nothing
    }

-- | The carrier or service provider that the phone number is currently
-- registered with. In some countries and regions, this value may be the
-- carrier or service provider that the phone number was originally
-- registered with.
numberValidateResponse_carrier :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_carrier = Lens.lens (\NumberValidateResponse' {carrier} -> carrier) (\s@NumberValidateResponse' {} a -> s {carrier = a} :: NumberValidateResponse)

-- | The name of the city where the phone number was originally registered.
numberValidateResponse_city :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_city = Lens.lens (\NumberValidateResponse' {city} -> city) (\s@NumberValidateResponse' {} a -> s {city = a} :: NumberValidateResponse)

-- | The cleansed phone number, in E.164 format, for the location where the
-- phone number was originally registered.
numberValidateResponse_cleansedPhoneNumberE164 :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_cleansedPhoneNumberE164 = Lens.lens (\NumberValidateResponse' {cleansedPhoneNumberE164} -> cleansedPhoneNumberE164) (\s@NumberValidateResponse' {} a -> s {cleansedPhoneNumberE164 = a} :: NumberValidateResponse)

-- | The cleansed phone number, in the format for the location where the
-- phone number was originally registered.
numberValidateResponse_cleansedPhoneNumberNational :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_cleansedPhoneNumberNational = Lens.lens (\NumberValidateResponse' {cleansedPhoneNumberNational} -> cleansedPhoneNumberNational) (\s@NumberValidateResponse' {} a -> s {cleansedPhoneNumberNational = a} :: NumberValidateResponse)

-- | The name of the country or region where the phone number was originally
-- registered.
numberValidateResponse_country :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_country = Lens.lens (\NumberValidateResponse' {country} -> country) (\s@NumberValidateResponse' {} a -> s {country = a} :: NumberValidateResponse)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the phone number was originally registered.
numberValidateResponse_countryCodeIso2 :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_countryCodeIso2 = Lens.lens (\NumberValidateResponse' {countryCodeIso2} -> countryCodeIso2) (\s@NumberValidateResponse' {} a -> s {countryCodeIso2 = a} :: NumberValidateResponse)

-- | The numeric code for the country or region where the phone number was
-- originally registered.
numberValidateResponse_countryCodeNumeric :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_countryCodeNumeric = Lens.lens (\NumberValidateResponse' {countryCodeNumeric} -> countryCodeNumeric) (\s@NumberValidateResponse' {} a -> s {countryCodeNumeric = a} :: NumberValidateResponse)

-- | The name of the county where the phone number was originally registered.
numberValidateResponse_county :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_county = Lens.lens (\NumberValidateResponse' {county} -> county) (\s@NumberValidateResponse' {} a -> s {county = a} :: NumberValidateResponse)

-- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in
-- the request body.
numberValidateResponse_originalCountryCodeIso2 :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_originalCountryCodeIso2 = Lens.lens (\NumberValidateResponse' {originalCountryCodeIso2} -> originalCountryCodeIso2) (\s@NumberValidateResponse' {} a -> s {originalCountryCodeIso2 = a} :: NumberValidateResponse)

-- | The phone number that was sent in the request body.
numberValidateResponse_originalPhoneNumber :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_originalPhoneNumber = Lens.lens (\NumberValidateResponse' {originalPhoneNumber} -> originalPhoneNumber) (\s@NumberValidateResponse' {} a -> s {originalPhoneNumber = a} :: NumberValidateResponse)

-- | The description of the phone type. Valid values are: MOBILE, LANDLINE,
-- VOIP, INVALID, PREPAID, and OTHER.
numberValidateResponse_phoneType :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_phoneType = Lens.lens (\NumberValidateResponse' {phoneType} -> phoneType) (\s@NumberValidateResponse' {} a -> s {phoneType = a} :: NumberValidateResponse)

-- | The phone type, represented by an integer. Valid values are: 0 (mobile),
-- 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
numberValidateResponse_phoneTypeCode :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Int)
numberValidateResponse_phoneTypeCode = Lens.lens (\NumberValidateResponse' {phoneTypeCode} -> phoneTypeCode) (\s@NumberValidateResponse' {} a -> s {phoneTypeCode = a} :: NumberValidateResponse)

-- | The time zone for the location where the phone number was originally
-- registered.
numberValidateResponse_timezone :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_timezone = Lens.lens (\NumberValidateResponse' {timezone} -> timezone) (\s@NumberValidateResponse' {} a -> s {timezone = a} :: NumberValidateResponse)

-- | The postal or ZIP code for the location where the phone number was
-- originally registered.
numberValidateResponse_zipCode :: Lens.Lens' NumberValidateResponse (Prelude.Maybe Prelude.Text)
numberValidateResponse_zipCode = Lens.lens (\NumberValidateResponse' {zipCode} -> zipCode) (\s@NumberValidateResponse' {} a -> s {zipCode = a} :: NumberValidateResponse)

instance Data.FromJSON NumberValidateResponse where
  parseJSON =
    Data.withObject
      "NumberValidateResponse"
      ( \x ->
          NumberValidateResponse'
            Prelude.<$> (x Data..:? "Carrier")
            Prelude.<*> (x Data..:? "City")
            Prelude.<*> (x Data..:? "CleansedPhoneNumberE164")
            Prelude.<*> (x Data..:? "CleansedPhoneNumberNational")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "CountryCodeIso2")
            Prelude.<*> (x Data..:? "CountryCodeNumeric")
            Prelude.<*> (x Data..:? "County")
            Prelude.<*> (x Data..:? "OriginalCountryCodeIso2")
            Prelude.<*> (x Data..:? "OriginalPhoneNumber")
            Prelude.<*> (x Data..:? "PhoneType")
            Prelude.<*> (x Data..:? "PhoneTypeCode")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> (x Data..:? "ZipCode")
      )

instance Prelude.Hashable NumberValidateResponse where
  hashWithSalt _salt NumberValidateResponse' {..} =
    _salt
      `Prelude.hashWithSalt` carrier
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` cleansedPhoneNumberE164
      `Prelude.hashWithSalt` cleansedPhoneNumberNational
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` countryCodeIso2
      `Prelude.hashWithSalt` countryCodeNumeric
      `Prelude.hashWithSalt` county
      `Prelude.hashWithSalt` originalCountryCodeIso2
      `Prelude.hashWithSalt` originalPhoneNumber
      `Prelude.hashWithSalt` phoneType
      `Prelude.hashWithSalt` phoneTypeCode
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` zipCode

instance Prelude.NFData NumberValidateResponse where
  rnf NumberValidateResponse' {..} =
    Prelude.rnf carrier
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf cleansedPhoneNumberE164
      `Prelude.seq` Prelude.rnf cleansedPhoneNumberNational
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf countryCodeIso2
      `Prelude.seq` Prelude.rnf countryCodeNumeric
      `Prelude.seq` Prelude.rnf county
      `Prelude.seq` Prelude.rnf originalCountryCodeIso2
      `Prelude.seq` Prelude.rnf originalPhoneNumber
      `Prelude.seq` Prelude.rnf phoneType
      `Prelude.seq` Prelude.rnf phoneTypeCode
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf zipCode
