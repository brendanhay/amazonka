{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Address
  ( Address (..),

    -- * Smart constructor
    mkAddress,

    -- * Lenses
    aAddressId,
    aCity,
    aCompany,
    aCountry,
    aIsRestricted,
    aLandmark,
    aName,
    aPhoneNumber,
    aPostalCode,
    aPrefectureOrDistrict,
    aStateOrProvince,
    aStreet1,
    aStreet2,
    aStreet3,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.AddressId as Types
import qualified Network.AWS.Snowball.Types.String as Types

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the @Address@ are required, if the address is invalid or unsupported, then an exception is thrown.
--
-- /See:/ 'mkAddress' smart constructor.
data Address = Address'
  { -- | The unique ID for an address.
    addressId :: Core.Maybe Types.AddressId,
    -- | The city in an address that a Snow device is to be delivered to.
    city :: Core.Maybe Types.String,
    -- | The name of the company to receive a Snow device at an address.
    company :: Core.Maybe Types.String,
    -- | The country in an address that a Snow device is to be delivered to.
    country :: Core.Maybe Types.String,
    -- | If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
    isRestricted :: Core.Maybe Core.Bool,
    -- | This field is no longer used and the value is ignored.
    landmark :: Core.Maybe Types.String,
    -- | The name of a person to receive a Snow device at an address.
    name :: Core.Maybe Types.String,
    -- | The phone number associated with an address that a Snow device is to be delivered to.
    phoneNumber :: Core.Maybe Types.String,
    -- | The postal code in an address that a Snow device is to be delivered to.
    postalCode :: Core.Maybe Types.String,
    -- | This field is no longer used and the value is ignored.
    prefectureOrDistrict :: Core.Maybe Types.String,
    -- | The state or province in an address that a Snow device is to be delivered to.
    stateOrProvince :: Core.Maybe Types.String,
    -- | The first line in a street address that a Snow device is to be delivered to.
    street1 :: Core.Maybe Types.String,
    -- | The second line in a street address that a Snow device is to be delivered to.
    street2 :: Core.Maybe Types.String,
    -- | The third line in a street address that a Snow device is to be delivered to.
    street3 :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Address' value with any optional fields omitted.
mkAddress ::
  Address
mkAddress =
  Address'
    { addressId = Core.Nothing,
      city = Core.Nothing,
      company = Core.Nothing,
      country = Core.Nothing,
      isRestricted = Core.Nothing,
      landmark = Core.Nothing,
      name = Core.Nothing,
      phoneNumber = Core.Nothing,
      postalCode = Core.Nothing,
      prefectureOrDistrict = Core.Nothing,
      stateOrProvince = Core.Nothing,
      street1 = Core.Nothing,
      street2 = Core.Nothing,
      street3 = Core.Nothing
    }

-- | The unique ID for an address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAddressId :: Lens.Lens' Address (Core.Maybe Types.AddressId)
aAddressId = Lens.field @"addressId"
{-# DEPRECATED aAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The city in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCity :: Lens.Lens' Address (Core.Maybe Types.String)
aCity = Lens.field @"city"
{-# DEPRECATED aCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The name of the company to receive a Snow device at an address.
--
-- /Note:/ Consider using 'company' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCompany :: Lens.Lens' Address (Core.Maybe Types.String)
aCompany = Lens.field @"company"
{-# DEPRECATED aCompany "Use generic-lens or generic-optics with 'company' instead." #-}

-- | The country in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCountry :: Lens.Lens' Address (Core.Maybe Types.String)
aCountry = Lens.field @"country"
{-# DEPRECATED aCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
--
-- /Note:/ Consider using 'isRestricted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIsRestricted :: Lens.Lens' Address (Core.Maybe Core.Bool)
aIsRestricted = Lens.field @"isRestricted"
{-# DEPRECATED aIsRestricted "Use generic-lens or generic-optics with 'isRestricted' instead." #-}

-- | This field is no longer used and the value is ignored.
--
-- /Note:/ Consider using 'landmark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLandmark :: Lens.Lens' Address (Core.Maybe Types.String)
aLandmark = Lens.field @"landmark"
{-# DEPRECATED aLandmark "Use generic-lens or generic-optics with 'landmark' instead." #-}

-- | The name of a person to receive a Snow device at an address.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Address (Core.Maybe Types.String)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The phone number associated with an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPhoneNumber :: Lens.Lens' Address (Core.Maybe Types.String)
aPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED aPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The postal code in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPostalCode :: Lens.Lens' Address (Core.Maybe Types.String)
aPostalCode = Lens.field @"postalCode"
{-# DEPRECATED aPostalCode "Use generic-lens or generic-optics with 'postalCode' instead." #-}

-- | This field is no longer used and the value is ignored.
--
-- /Note:/ Consider using 'prefectureOrDistrict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrefectureOrDistrict :: Lens.Lens' Address (Core.Maybe Types.String)
aPrefectureOrDistrict = Lens.field @"prefectureOrDistrict"
{-# DEPRECATED aPrefectureOrDistrict "Use generic-lens or generic-optics with 'prefectureOrDistrict' instead." #-}

-- | The state or province in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'stateOrProvince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStateOrProvince :: Lens.Lens' Address (Core.Maybe Types.String)
aStateOrProvince = Lens.field @"stateOrProvince"
{-# DEPRECATED aStateOrProvince "Use generic-lens or generic-optics with 'stateOrProvince' instead." #-}

-- | The first line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet1 :: Lens.Lens' Address (Core.Maybe Types.String)
aStreet1 = Lens.field @"street1"
{-# DEPRECATED aStreet1 "Use generic-lens or generic-optics with 'street1' instead." #-}

-- | The second line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet2 :: Lens.Lens' Address (Core.Maybe Types.String)
aStreet2 = Lens.field @"street2"
{-# DEPRECATED aStreet2 "Use generic-lens or generic-optics with 'street2' instead." #-}

-- | The third line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet3 :: Lens.Lens' Address (Core.Maybe Types.String)
aStreet3 = Lens.field @"street3"
{-# DEPRECATED aStreet3 "Use generic-lens or generic-optics with 'street3' instead." #-}

instance Core.FromJSON Address where
  toJSON Address {..} =
    Core.object
      ( Core.catMaybes
          [ ("AddressId" Core..=) Core.<$> addressId,
            ("City" Core..=) Core.<$> city,
            ("Company" Core..=) Core.<$> company,
            ("Country" Core..=) Core.<$> country,
            ("IsRestricted" Core..=) Core.<$> isRestricted,
            ("Landmark" Core..=) Core.<$> landmark,
            ("Name" Core..=) Core.<$> name,
            ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("PostalCode" Core..=) Core.<$> postalCode,
            ("PrefectureOrDistrict" Core..=) Core.<$> prefectureOrDistrict,
            ("StateOrProvince" Core..=) Core.<$> stateOrProvince,
            ("Street1" Core..=) Core.<$> street1,
            ("Street2" Core..=) Core.<$> street2,
            ("Street3" Core..=) Core.<$> street3
          ]
      )

instance Core.FromJSON Address where
  parseJSON =
    Core.withObject "Address" Core.$
      \x ->
        Address'
          Core.<$> (x Core..:? "AddressId")
          Core.<*> (x Core..:? "City")
          Core.<*> (x Core..:? "Company")
          Core.<*> (x Core..:? "Country")
          Core.<*> (x Core..:? "IsRestricted")
          Core.<*> (x Core..:? "Landmark")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "PhoneNumber")
          Core.<*> (x Core..:? "PostalCode")
          Core.<*> (x Core..:? "PrefectureOrDistrict")
          Core.<*> (x Core..:? "StateOrProvince")
          Core.<*> (x Core..:? "Street1")
          Core.<*> (x Core..:? "Street2")
          Core.<*> (x Core..:? "Street3")
