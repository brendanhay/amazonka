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
    nvrCarrier,
    nvrCity,
    nvrCleansedPhoneNumberE164,
    nvrCleansedPhoneNumberNational,
    nvrCountry,
    nvrCountryCodeIso2,
    nvrCountryCodeNumeric,
    nvrCounty,
    nvrOriginalCountryCodeIso2,
    nvrOriginalPhoneNumber,
    nvrPhoneType,
    nvrPhoneTypeCode,
    nvrTimezone,
    nvrZipCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a phone number.
--
-- /See:/ 'mkNumberValidateResponse' smart constructor.
data NumberValidateResponse = NumberValidateResponse'
  { -- | The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
    carrier :: Core.Maybe Core.Text,
    -- | The name of the city where the phone number was originally registered.
    city :: Core.Maybe Core.Text,
    -- | The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
    cleansedPhoneNumberE164 :: Core.Maybe Core.Text,
    -- | The cleansed phone number, in the format for the location where the phone number was originally registered.
    cleansedPhoneNumberNational :: Core.Maybe Core.Text,
    -- | The name of the country or region where the phone number was originally registered.
    country :: Core.Maybe Core.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
    countryCodeIso2 :: Core.Maybe Core.Text,
    -- | The numeric code for the country or region where the phone number was originally registered.
    countryCodeNumeric :: Core.Maybe Core.Text,
    -- | The name of the county where the phone number was originally registered.
    county :: Core.Maybe Core.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
    originalCountryCodeIso2 :: Core.Maybe Core.Text,
    -- | The phone number that was sent in the request body.
    originalPhoneNumber :: Core.Maybe Core.Text,
    -- | The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,
    --
    --                   INVALID, PREPAID, and OTHER.
    phoneType :: Core.Maybe Core.Text,
    -- | The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
    phoneTypeCode :: Core.Maybe Core.Int,
    -- | The time zone for the location where the phone number was originally registered.
    timezone :: Core.Maybe Core.Text,
    -- | The postal or ZIP code for the location where the phone number was originally registered.
    zipCode :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NumberValidateResponse' value with any optional fields omitted.
mkNumberValidateResponse ::
  NumberValidateResponse
mkNumberValidateResponse =
  NumberValidateResponse'
    { carrier = Core.Nothing,
      city = Core.Nothing,
      cleansedPhoneNumberE164 = Core.Nothing,
      cleansedPhoneNumberNational = Core.Nothing,
      country = Core.Nothing,
      countryCodeIso2 = Core.Nothing,
      countryCodeNumeric = Core.Nothing,
      county = Core.Nothing,
      originalCountryCodeIso2 = Core.Nothing,
      originalPhoneNumber = Core.Nothing,
      phoneType = Core.Nothing,
      phoneTypeCode = Core.Nothing,
      timezone = Core.Nothing,
      zipCode = Core.Nothing
    }

-- | The carrier or service provider that the phone number is currently registered with. In some countries and regions, this value may be the carrier or service provider that the phone number was originally registered with.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCarrier :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCarrier = Lens.field @"carrier"
{-# DEPRECATED nvrCarrier "Use generic-lens or generic-optics with 'carrier' instead." #-}

-- | The name of the city where the phone number was originally registered.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCity :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCity = Lens.field @"city"
{-# DEPRECATED nvrCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The cleansed phone number, in E.164 format, for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'cleansedPhoneNumberE164' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCleansedPhoneNumberE164 :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCleansedPhoneNumberE164 = Lens.field @"cleansedPhoneNumberE164"
{-# DEPRECATED nvrCleansedPhoneNumberE164 "Use generic-lens or generic-optics with 'cleansedPhoneNumberE164' instead." #-}

-- | The cleansed phone number, in the format for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'cleansedPhoneNumberNational' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCleansedPhoneNumberNational :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCleansedPhoneNumberNational = Lens.field @"cleansedPhoneNumberNational"
{-# DEPRECATED nvrCleansedPhoneNumberNational "Use generic-lens or generic-optics with 'cleansedPhoneNumberNational' instead." #-}

-- | The name of the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCountry :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCountry = Lens.field @"country"
{-# DEPRECATED nvrCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'countryCodeIso2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCountryCodeIso2 :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCountryCodeIso2 = Lens.field @"countryCodeIso2"
{-# DEPRECATED nvrCountryCodeIso2 "Use generic-lens or generic-optics with 'countryCodeIso2' instead." #-}

-- | The numeric code for the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'countryCodeNumeric' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCountryCodeNumeric :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCountryCodeNumeric = Lens.field @"countryCodeNumeric"
{-# DEPRECATED nvrCountryCodeNumeric "Use generic-lens or generic-optics with 'countryCodeNumeric' instead." #-}

-- | The name of the county where the phone number was originally registered.
--
-- /Note:/ Consider using 'county' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrCounty :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrCounty = Lens.field @"county"
{-# DEPRECATED nvrCounty "Use generic-lens or generic-optics with 'county' instead." #-}

-- | The two-character code, in ISO 3166-1 alpha-2 format, that was sent in the request body.
--
-- /Note:/ Consider using 'originalCountryCodeIso2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrOriginalCountryCodeIso2 :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrOriginalCountryCodeIso2 = Lens.field @"originalCountryCodeIso2"
{-# DEPRECATED nvrOriginalCountryCodeIso2 "Use generic-lens or generic-optics with 'originalCountryCodeIso2' instead." #-}

-- | The phone number that was sent in the request body.
--
-- /Note:/ Consider using 'originalPhoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrOriginalPhoneNumber :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrOriginalPhoneNumber = Lens.field @"originalPhoneNumber"
{-# DEPRECATED nvrOriginalPhoneNumber "Use generic-lens or generic-optics with 'originalPhoneNumber' instead." #-}

-- | The description of the phone type. Valid values are: MOBILE, LANDLINE, VOIP,
--
--                   INVALID, PREPAID, and OTHER.
--
-- /Note:/ Consider using 'phoneType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrPhoneType :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrPhoneType = Lens.field @"phoneType"
{-# DEPRECATED nvrPhoneType "Use generic-lens or generic-optics with 'phoneType' instead." #-}

-- | The phone type, represented by an integer. Valid values are: 0 (mobile), 1 (landline), 2 (VoIP), 3 (invalid), 4 (other), and 5 (prepaid).
--
-- /Note:/ Consider using 'phoneTypeCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrPhoneTypeCode :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Int)
nvrPhoneTypeCode = Lens.field @"phoneTypeCode"
{-# DEPRECATED nvrPhoneTypeCode "Use generic-lens or generic-optics with 'phoneTypeCode' instead." #-}

-- | The time zone for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrTimezone :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrTimezone = Lens.field @"timezone"
{-# DEPRECATED nvrTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The postal or ZIP code for the location where the phone number was originally registered.
--
-- /Note:/ Consider using 'zipCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrZipCode :: Lens.Lens' NumberValidateResponse (Core.Maybe Core.Text)
nvrZipCode = Lens.field @"zipCode"
{-# DEPRECATED nvrZipCode "Use generic-lens or generic-optics with 'zipCode' instead." #-}

instance Core.FromJSON NumberValidateResponse where
  parseJSON =
    Core.withObject "NumberValidateResponse" Core.$
      \x ->
        NumberValidateResponse'
          Core.<$> (x Core..:? "Carrier")
          Core.<*> (x Core..:? "City")
          Core.<*> (x Core..:? "CleansedPhoneNumberE164")
          Core.<*> (x Core..:? "CleansedPhoneNumberNational")
          Core.<*> (x Core..:? "Country")
          Core.<*> (x Core..:? "CountryCodeIso2")
          Core.<*> (x Core..:? "CountryCodeNumeric")
          Core.<*> (x Core..:? "County")
          Core.<*> (x Core..:? "OriginalCountryCodeIso2")
          Core.<*> (x Core..:? "OriginalPhoneNumber")
          Core.<*> (x Core..:? "PhoneType")
          Core.<*> (x Core..:? "PhoneTypeCode")
          Core.<*> (x Core..:? "Timezone")
          Core.<*> (x Core..:? "ZipCode")
