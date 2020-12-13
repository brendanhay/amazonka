{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.NumberValidateResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.NumberValidateResponse
  ( NumberValidateResponse (..),

    -- * Smart constructor
    mkNumberValidateResponse,

    -- * Lenses
    nvCarrier,
    nvCounty,
    nvCountry,
    nvCountryCodeNumeric,
    nvZipCode,
    nvOriginalPhoneNumber,
    nvPhoneTypeCode,
    nvPhoneType,
    nvCity,
    nvCountryCodeIso2,
    nvTimezone,
    nvOriginalCountryCodeIso2,
    nvCleansedPhoneNumberNational,
    nvCleansedPhoneNumberE164,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a phone number.
--
-- /See:/ 'mkNumberValidateResponse' smart constructor.
data NumberValidateResponse = NumberValidateResponse'
  { -- | The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
    carrier :: Lude.Maybe Lude.Text,
    -- | The name of the county where the phone number was originally registered.
    county :: Lude.Maybe Lude.Text,
    -- | The name of the country or region where the phone number was originally registered.
    country :: Lude.Maybe Lude.Text,
    -- | The numeric code for the country or region where the phone number was originally registered.
    countryCodeNumeric :: Lude.Maybe Lude.Text,
    -- | The postal or ZIP code for the location where the phone number was originally registered.
    zipCode :: Lude.Maybe Lude.Text,
    -- | The phone number that was sent in the request body.
    originalPhoneNumber :: Lude.Maybe Lude.Text,
    -- | The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
    phoneTypeCode :: Lude.Maybe Lude.Int,
    -- | The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,
    --
    --                   INVALID, PREPAID, and OTHER.
    phoneType :: Lude.Maybe Lude.Text,
    -- | The name of the city where the phone number was originally registered.
    city :: Lude.Maybe Lude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
    countryCodeIso2 :: Lude.Maybe Lude.Text,
    -- | The time zone for the location where the phone number was originally registered.
    timezone :: Lude.Maybe Lude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
    originalCountryCodeIso2 :: Lude.Maybe Lude.Text,
    -- | The cleansed phone number, in the format for the location where the phone number was originally registered.
    cleansedPhoneNumberNational :: Lude.Maybe Lude.Text,
    -- | The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
    cleansedPhoneNumberE164 :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NumberValidateResponse' with the minimum fields required to make a request.
--
-- * 'carrier' - The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
-- * 'county' - The name of the county where the phone number was originally registered.
-- * 'country' - The name of the country or region where the phone number was originally registered.
-- * 'countryCodeNumeric' - The numeric code for the country or region where the phone number was originally registered.
-- * 'zipCode' - The postal or ZIP code for the location where the phone number was originally registered.
-- * 'originalPhoneNumber' - The phone number that was sent in the request body.
-- * 'phoneTypeCode' - The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
-- * 'phoneType' - The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,
--
--                   INVALID, PREPAID, and OTHER.
-- * 'city' - The name of the city where the phone number was originally registered.
-- * 'countryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
-- * 'timezone' - The time zone for the location where the phone number was originally registered.
-- * 'originalCountryCodeIso2' - The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
-- * 'cleansedPhoneNumberNational' - The cleansed phone number, in the format for the location where the phone number was originally registered.
-- * 'cleansedPhoneNumberE164' - The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
mkNumberValidateResponse ::
  NumberValidateResponse
mkNumberValidateResponse =
  NumberValidateResponse'
    { carrier = Lude.Nothing,
      county = Lude.Nothing,
      country = Lude.Nothing,
      countryCodeNumeric = Lude.Nothing,
      zipCode = Lude.Nothing,
      originalPhoneNumber = Lude.Nothing,
      phoneTypeCode = Lude.Nothing,
      phoneType = Lude.Nothing,
      city = Lude.Nothing,
      countryCodeIso2 = Lude.Nothing,
      timezone = Lude.Nothing,
      originalCountryCodeIso2 = Lude.Nothing,
      cleansedPhoneNumberNational = Lude.Nothing,
      cleansedPhoneNumberE164 = Lude.Nothing
    }

-- | The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCarrier :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCarrier = Lens.lens (carrier :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {carrier = a} :: NumberValidateResponse)
{-# DEPRECATED nvCarrier "Use generic-lens or generic-optics with 'carrier' instead." #-}

-- | The name of the county where the phone number was originally registered.
--
-- /Note:/ Consider using 'county' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCounty :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCounty = Lens.lens (county :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {county = a} :: NumberValidateResponse)
{-# DEPRECATED nvCounty "Use generic-lens or generic-optics with 'county' instead." #-}

-- | The name of the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCountry :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCountry = Lens.lens (country :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: NumberValidateResponse)
{-# DEPRECATED nvCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The numeric code for the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'countryCodeNumeric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCountryCodeNumeric :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCountryCodeNumeric = Lens.lens (countryCodeNumeric :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {countryCodeNumeric = a} :: NumberValidateResponse)
{-# DEPRECATED nvCountryCodeNumeric "Use generic-lens or generic-optics with 'countryCodeNumeric' instead." #-}

-- | The postal or ZIP code for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'zipCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvZipCode :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvZipCode = Lens.lens (zipCode :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {zipCode = a} :: NumberValidateResponse)
{-# DEPRECATED nvZipCode "Use generic-lens or generic-optics with 'zipCode' instead." #-}

-- | The phone number that was sent in the request body.
--
-- /Note:/ Consider using 'originalPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvOriginalPhoneNumber :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvOriginalPhoneNumber = Lens.lens (originalPhoneNumber :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {originalPhoneNumber = a} :: NumberValidateResponse)
{-# DEPRECATED nvOriginalPhoneNumber "Use generic-lens or generic-optics with 'originalPhoneNumber' instead." #-}

-- | The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
--
-- /Note:/ Consider using 'phoneTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvPhoneTypeCode :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Int)
nvPhoneTypeCode = Lens.lens (phoneTypeCode :: NumberValidateResponse -> Lude.Maybe Lude.Int) (\s a -> s {phoneTypeCode = a} :: NumberValidateResponse)
{-# DEPRECATED nvPhoneTypeCode "Use generic-lens or generic-optics with 'phoneTypeCode' instead." #-}

-- | The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,
--
--                   INVALID, PREPAID, and OTHER.
--
-- /Note:/ Consider using 'phoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvPhoneType :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvPhoneType = Lens.lens (phoneType :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {phoneType = a} :: NumberValidateResponse)
{-# DEPRECATED nvPhoneType "Use generic-lens or generic-optics with 'phoneType' instead." #-}

-- | The name of the city where the phone number was originally registered.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCity :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCity = Lens.lens (city :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {city = a} :: NumberValidateResponse)
{-# DEPRECATED nvCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'countryCodeIso2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCountryCodeIso2 :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCountryCodeIso2 = Lens.lens (countryCodeIso2 :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {countryCodeIso2 = a} :: NumberValidateResponse)
{-# DEPRECATED nvCountryCodeIso2 "Use generic-lens or generic-optics with 'countryCodeIso2' instead." #-}

-- | The time zone for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvTimezone :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvTimezone = Lens.lens (timezone :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: NumberValidateResponse)
{-# DEPRECATED nvTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
--
-- /Note:/ Consider using 'originalCountryCodeIso2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvOriginalCountryCodeIso2 :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvOriginalCountryCodeIso2 = Lens.lens (originalCountryCodeIso2 :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {originalCountryCodeIso2 = a} :: NumberValidateResponse)
{-# DEPRECATED nvOriginalCountryCodeIso2 "Use generic-lens or generic-optics with 'originalCountryCodeIso2' instead." #-}

-- | The cleansed phone number, in the format for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'cleansedPhoneNumberNational' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCleansedPhoneNumberNational :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCleansedPhoneNumberNational = Lens.lens (cleansedPhoneNumberNational :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {cleansedPhoneNumberNational = a} :: NumberValidateResponse)
{-# DEPRECATED nvCleansedPhoneNumberNational "Use generic-lens or generic-optics with 'cleansedPhoneNumberNational' instead." #-}

-- | The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'cleansedPhoneNumberE164' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvCleansedPhoneNumberE164 :: Lens.Lens' NumberValidateResponse (Lude.Maybe Lude.Text)
nvCleansedPhoneNumberE164 = Lens.lens (cleansedPhoneNumberE164 :: NumberValidateResponse -> Lude.Maybe Lude.Text) (\s a -> s {cleansedPhoneNumberE164 = a} :: NumberValidateResponse)
{-# DEPRECATED nvCleansedPhoneNumberE164 "Use generic-lens or generic-optics with 'cleansedPhoneNumberE164' instead." #-}

instance Lude.FromJSON NumberValidateResponse where
  parseJSON =
    Lude.withObject
      "NumberValidateResponse"
      ( \x ->
          NumberValidateResponse'
            Lude.<$> (x Lude..:? "Carrier")
            Lude.<*> (x Lude..:? "County")
            Lude.<*> (x Lude..:? "Country")
            Lude.<*> (x Lude..:? "CountryCodeNumeric")
            Lude.<*> (x Lude..:? "ZipCode")
            Lude.<*> (x Lude..:? "OriginalPhoneNumber")
            Lude.<*> (x Lude..:? "PhoneTypeCode")
            Lude.<*> (x Lude..:? "PhoneType")
            Lude.<*> (x Lude..:? "City")
            Lude.<*> (x Lude..:? "CountryCodeIso2")
            Lude.<*> (x Lude..:? "Timezone")
            Lude.<*> (x Lude..:? "OriginalCountryCodeIso2")
            Lude.<*> (x Lude..:? "CleansedPhoneNumberNational")
            Lude.<*> (x Lude..:? "CleansedPhoneNumberE164")
      )
