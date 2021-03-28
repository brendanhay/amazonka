{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.Address
  ( Address (..)
  -- * Smart constructor
  , mkAddress
  -- * Lenses
  , aAddressId
  , aCity
  , aCompany
  , aCountry
  , aIsRestricted
  , aLandmark
  , aName
  , aPhoneNumber
  , aPostalCode
  , aPrefectureOrDistrict
  , aStateOrProvince
  , aStreet1
  , aStreet2
  , aStreet3
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.AddressId as Types

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the @Address@ are required, if the address is invalid or unsupported, then an exception is thrown.
--
-- /See:/ 'mkAddress' smart constructor.
data Address = Address'
  { addressId :: Core.Maybe Types.AddressId
    -- ^ The unique ID for an address.
  , city :: Core.Maybe Core.Text
    -- ^ The city in an address that a Snow device is to be delivered to.
  , company :: Core.Maybe Core.Text
    -- ^ The name of the company to receive a Snow device at an address.
  , country :: Core.Maybe Core.Text
    -- ^ The country in an address that a Snow device is to be delivered to.
  , isRestricted :: Core.Maybe Core.Bool
    -- ^ If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
  , landmark :: Core.Maybe Core.Text
    -- ^ This field is no longer used and the value is ignored.
  , name :: Core.Maybe Core.Text
    -- ^ The name of a person to receive a Snow device at an address.
  , phoneNumber :: Core.Maybe Core.Text
    -- ^ The phone number associated with an address that a Snow device is to be delivered to.
  , postalCode :: Core.Maybe Core.Text
    -- ^ The postal code in an address that a Snow device is to be delivered to.
  , prefectureOrDistrict :: Core.Maybe Core.Text
    -- ^ This field is no longer used and the value is ignored.
  , stateOrProvince :: Core.Maybe Core.Text
    -- ^ The state or province in an address that a Snow device is to be delivered to.
  , street1 :: Core.Maybe Core.Text
    -- ^ The first line in a street address that a Snow device is to be delivered to.
  , street2 :: Core.Maybe Core.Text
    -- ^ The second line in a street address that a Snow device is to be delivered to.
  , street3 :: Core.Maybe Core.Text
    -- ^ The third line in a street address that a Snow device is to be delivered to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Address' value with any optional fields omitted.
mkAddress
    :: Address
mkAddress
  = Address'{addressId = Core.Nothing, city = Core.Nothing,
             company = Core.Nothing, country = Core.Nothing,
             isRestricted = Core.Nothing, landmark = Core.Nothing,
             name = Core.Nothing, phoneNumber = Core.Nothing,
             postalCode = Core.Nothing, prefectureOrDistrict = Core.Nothing,
             stateOrProvince = Core.Nothing, street1 = Core.Nothing,
             street2 = Core.Nothing, street3 = Core.Nothing}

-- | The unique ID for an address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAddressId :: Lens.Lens' Address (Core.Maybe Types.AddressId)
aAddressId = Lens.field @"addressId"
{-# INLINEABLE aAddressId #-}
{-# DEPRECATED addressId "Use generic-lens or generic-optics with 'addressId' instead"  #-}

-- | The city in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCity :: Lens.Lens' Address (Core.Maybe Core.Text)
aCity = Lens.field @"city"
{-# INLINEABLE aCity #-}
{-# DEPRECATED city "Use generic-lens or generic-optics with 'city' instead"  #-}

-- | The name of the company to receive a Snow device at an address.
--
-- /Note:/ Consider using 'company' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCompany :: Lens.Lens' Address (Core.Maybe Core.Text)
aCompany = Lens.field @"company"
{-# INLINEABLE aCompany #-}
{-# DEPRECATED company "Use generic-lens or generic-optics with 'company' instead"  #-}

-- | The country in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCountry :: Lens.Lens' Address (Core.Maybe Core.Text)
aCountry = Lens.field @"country"
{-# INLINEABLE aCountry #-}
{-# DEPRECATED country "Use generic-lens or generic-optics with 'country' instead"  #-}

-- | If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
--
-- /Note:/ Consider using 'isRestricted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIsRestricted :: Lens.Lens' Address (Core.Maybe Core.Bool)
aIsRestricted = Lens.field @"isRestricted"
{-# INLINEABLE aIsRestricted #-}
{-# DEPRECATED isRestricted "Use generic-lens or generic-optics with 'isRestricted' instead"  #-}

-- | This field is no longer used and the value is ignored.
--
-- /Note:/ Consider using 'landmark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLandmark :: Lens.Lens' Address (Core.Maybe Core.Text)
aLandmark = Lens.field @"landmark"
{-# INLINEABLE aLandmark #-}
{-# DEPRECATED landmark "Use generic-lens or generic-optics with 'landmark' instead"  #-}

-- | The name of a person to receive a Snow device at an address.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Address (Core.Maybe Core.Text)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The phone number associated with an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPhoneNumber :: Lens.Lens' Address (Core.Maybe Core.Text)
aPhoneNumber = Lens.field @"phoneNumber"
{-# INLINEABLE aPhoneNumber #-}
{-# DEPRECATED phoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead"  #-}

-- | The postal code in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPostalCode :: Lens.Lens' Address (Core.Maybe Core.Text)
aPostalCode = Lens.field @"postalCode"
{-# INLINEABLE aPostalCode #-}
{-# DEPRECATED postalCode "Use generic-lens or generic-optics with 'postalCode' instead"  #-}

-- | This field is no longer used and the value is ignored.
--
-- /Note:/ Consider using 'prefectureOrDistrict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrefectureOrDistrict :: Lens.Lens' Address (Core.Maybe Core.Text)
aPrefectureOrDistrict = Lens.field @"prefectureOrDistrict"
{-# INLINEABLE aPrefectureOrDistrict #-}
{-# DEPRECATED prefectureOrDistrict "Use generic-lens or generic-optics with 'prefectureOrDistrict' instead"  #-}

-- | The state or province in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'stateOrProvince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStateOrProvince :: Lens.Lens' Address (Core.Maybe Core.Text)
aStateOrProvince = Lens.field @"stateOrProvince"
{-# INLINEABLE aStateOrProvince #-}
{-# DEPRECATED stateOrProvince "Use generic-lens or generic-optics with 'stateOrProvince' instead"  #-}

-- | The first line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet1 :: Lens.Lens' Address (Core.Maybe Core.Text)
aStreet1 = Lens.field @"street1"
{-# INLINEABLE aStreet1 #-}
{-# DEPRECATED street1 "Use generic-lens or generic-optics with 'street1' instead"  #-}

-- | The second line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet2 :: Lens.Lens' Address (Core.Maybe Core.Text)
aStreet2 = Lens.field @"street2"
{-# INLINEABLE aStreet2 #-}
{-# DEPRECATED street2 "Use generic-lens or generic-optics with 'street2' instead"  #-}

-- | The third line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet3 :: Lens.Lens' Address (Core.Maybe Core.Text)
aStreet3 = Lens.field @"street3"
{-# INLINEABLE aStreet3 #-}
{-# DEPRECATED street3 "Use generic-lens or generic-optics with 'street3' instead"  #-}

instance Core.FromJSON Address where
        toJSON Address{..}
          = Core.object
              (Core.catMaybes
                 [("AddressId" Core..=) Core.<$> addressId,
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
                  ("Street3" Core..=) Core.<$> street3])

instance Core.FromJSON Address where
        parseJSON
          = Core.withObject "Address" Core.$
              \ x ->
                Address' Core.<$>
                  (x Core..:? "AddressId") Core.<*> x Core..:? "City" Core.<*>
                    x Core..:? "Company"
                    Core.<*> x Core..:? "Country"
                    Core.<*> x Core..:? "IsRestricted"
                    Core.<*> x Core..:? "Landmark"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "PhoneNumber"
                    Core.<*> x Core..:? "PostalCode"
                    Core.<*> x Core..:? "PrefectureOrDistrict"
                    Core.<*> x Core..:? "StateOrProvince"
                    Core.<*> x Core..:? "Street1"
                    Core.<*> x Core..:? "Street2"
                    Core.<*> x Core..:? "Street3"
