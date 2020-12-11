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
    aIsRestricted,
    aStreet3,
    aLandmark,
    aPostalCode,
    aCountry,
    aStateOrProvince,
    aStreet2,
    aAddressId,
    aCity,
    aPhoneNumber,
    aCompany,
    aName,
    aPrefectureOrDistrict,
    aStreet1,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The address that you want the Snow device(s) associated with a specific job to be shipped to. Addresses are validated at the time of creation. The address you provide must be located within the serviceable area of your region. Although no individual elements of the @Address@ are required, if the address is invalid or unsupported, then an exception is thrown.
--
-- /See:/ 'mkAddress' smart constructor.
data Address = Address'
  { isRestricted :: Lude.Maybe Lude.Bool,
    street3 :: Lude.Maybe Lude.Text,
    landmark :: Lude.Maybe Lude.Text,
    postalCode :: Lude.Maybe Lude.Text,
    country :: Lude.Maybe Lude.Text,
    stateOrProvince :: Lude.Maybe Lude.Text,
    street2 :: Lude.Maybe Lude.Text,
    addressId :: Lude.Maybe Lude.Text,
    city :: Lude.Maybe Lude.Text,
    phoneNumber :: Lude.Maybe Lude.Text,
    company :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    prefectureOrDistrict :: Lude.Maybe Lude.Text,
    street1 :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Address' with the minimum fields required to make a request.
--
-- * 'addressId' - The unique ID for an address.
-- * 'city' - The city in an address that a Snow device is to be delivered to.
-- * 'company' - The name of the company to receive a Snow device at an address.
-- * 'country' - The country in an address that a Snow device is to be delivered to.
-- * 'isRestricted' - If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
-- * 'landmark' - This field is no longer used and the value is ignored.
-- * 'name' - The name of a person to receive a Snow device at an address.
-- * 'phoneNumber' - The phone number associated with an address that a Snow device is to be delivered to.
-- * 'postalCode' - The postal code in an address that a Snow device is to be delivered to.
-- * 'prefectureOrDistrict' - This field is no longer used and the value is ignored.
-- * 'stateOrProvince' - The state or province in an address that a Snow device is to be delivered to.
-- * 'street1' - The first line in a street address that a Snow device is to be delivered to.
-- * 'street2' - The second line in a street address that a Snow device is to be delivered to.
-- * 'street3' - The third line in a street address that a Snow device is to be delivered to.
mkAddress ::
  Address
mkAddress =
  Address'
    { isRestricted = Lude.Nothing,
      street3 = Lude.Nothing,
      landmark = Lude.Nothing,
      postalCode = Lude.Nothing,
      country = Lude.Nothing,
      stateOrProvince = Lude.Nothing,
      street2 = Lude.Nothing,
      addressId = Lude.Nothing,
      city = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      company = Lude.Nothing,
      name = Lude.Nothing,
      prefectureOrDistrict = Lude.Nothing,
      street1 = Lude.Nothing
    }

-- | If the address you are creating is a primary address, then set this option to true. This field is not supported in most regions.
--
-- /Note:/ Consider using 'isRestricted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIsRestricted :: Lens.Lens' Address (Lude.Maybe Lude.Bool)
aIsRestricted = Lens.lens (isRestricted :: Address -> Lude.Maybe Lude.Bool) (\s a -> s {isRestricted = a} :: Address)
{-# DEPRECATED aIsRestricted "Use generic-lens or generic-optics with 'isRestricted' instead." #-}

-- | The third line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street3' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet3 :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aStreet3 = Lens.lens (street3 :: Address -> Lude.Maybe Lude.Text) (\s a -> s {street3 = a} :: Address)
{-# DEPRECATED aStreet3 "Use generic-lens or generic-optics with 'street3' instead." #-}

-- | This field is no longer used and the value is ignored.
--
-- /Note:/ Consider using 'landmark' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLandmark :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aLandmark = Lens.lens (landmark :: Address -> Lude.Maybe Lude.Text) (\s a -> s {landmark = a} :: Address)
{-# DEPRECATED aLandmark "Use generic-lens or generic-optics with 'landmark' instead." #-}

-- | The postal code in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPostalCode :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aPostalCode = Lens.lens (postalCode :: Address -> Lude.Maybe Lude.Text) (\s a -> s {postalCode = a} :: Address)
{-# DEPRECATED aPostalCode "Use generic-lens or generic-optics with 'postalCode' instead." #-}

-- | The country in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCountry :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aCountry = Lens.lens (country :: Address -> Lude.Maybe Lude.Text) (\s a -> s {country = a} :: Address)
{-# DEPRECATED aCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The state or province in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'stateOrProvince' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStateOrProvince :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aStateOrProvince = Lens.lens (stateOrProvince :: Address -> Lude.Maybe Lude.Text) (\s a -> s {stateOrProvince = a} :: Address)
{-# DEPRECATED aStateOrProvince "Use generic-lens or generic-optics with 'stateOrProvince' instead." #-}

-- | The second line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street2' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet2 :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aStreet2 = Lens.lens (street2 :: Address -> Lude.Maybe Lude.Text) (\s a -> s {street2 = a} :: Address)
{-# DEPRECATED aStreet2 "Use generic-lens or generic-optics with 'street2' instead." #-}

-- | The unique ID for an address.
--
-- /Note:/ Consider using 'addressId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAddressId :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aAddressId = Lens.lens (addressId :: Address -> Lude.Maybe Lude.Text) (\s a -> s {addressId = a} :: Address)
{-# DEPRECATED aAddressId "Use generic-lens or generic-optics with 'addressId' instead." #-}

-- | The city in an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCity :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aCity = Lens.lens (city :: Address -> Lude.Maybe Lude.Text) (\s a -> s {city = a} :: Address)
{-# DEPRECATED aCity "Use generic-lens or generic-optics with 'city' instead." #-}

-- | The phone number associated with an address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPhoneNumber :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aPhoneNumber = Lens.lens (phoneNumber :: Address -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: Address)
{-# DEPRECATED aPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The name of the company to receive a Snow device at an address.
--
-- /Note:/ Consider using 'company' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCompany :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aCompany = Lens.lens (company :: Address -> Lude.Maybe Lude.Text) (\s a -> s {company = a} :: Address)
{-# DEPRECATED aCompany "Use generic-lens or generic-optics with 'company' instead." #-}

-- | The name of a person to receive a Snow device at an address.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Address -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Address)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | This field is no longer used and the value is ignored.
--
-- /Note:/ Consider using 'prefectureOrDistrict' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrefectureOrDistrict :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aPrefectureOrDistrict = Lens.lens (prefectureOrDistrict :: Address -> Lude.Maybe Lude.Text) (\s a -> s {prefectureOrDistrict = a} :: Address)
{-# DEPRECATED aPrefectureOrDistrict "Use generic-lens or generic-optics with 'prefectureOrDistrict' instead." #-}

-- | The first line in a street address that a Snow device is to be delivered to.
--
-- /Note:/ Consider using 'street1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aStreet1 :: Lens.Lens' Address (Lude.Maybe Lude.Text)
aStreet1 = Lens.lens (street1 :: Address -> Lude.Maybe Lude.Text) (\s a -> s {street1 = a} :: Address)
{-# DEPRECATED aStreet1 "Use generic-lens or generic-optics with 'street1' instead." #-}

instance Lude.FromJSON Address where
  parseJSON =
    Lude.withObject
      "Address"
      ( \x ->
          Address'
            Lude.<$> (x Lude..:? "IsRestricted")
            Lude.<*> (x Lude..:? "Street3")
            Lude.<*> (x Lude..:? "Landmark")
            Lude.<*> (x Lude..:? "PostalCode")
            Lude.<*> (x Lude..:? "Country")
            Lude.<*> (x Lude..:? "StateOrProvince")
            Lude.<*> (x Lude..:? "Street2")
            Lude.<*> (x Lude..:? "AddressId")
            Lude.<*> (x Lude..:? "City")
            Lude.<*> (x Lude..:? "PhoneNumber")
            Lude.<*> (x Lude..:? "Company")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "PrefectureOrDistrict")
            Lude.<*> (x Lude..:? "Street1")
      )

instance Lude.ToJSON Address where
  toJSON Address' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IsRestricted" Lude..=) Lude.<$> isRestricted,
            ("Street3" Lude..=) Lude.<$> street3,
            ("Landmark" Lude..=) Lude.<$> landmark,
            ("PostalCode" Lude..=) Lude.<$> postalCode,
            ("Country" Lude..=) Lude.<$> country,
            ("StateOrProvince" Lude..=) Lude.<$> stateOrProvince,
            ("Street2" Lude..=) Lude.<$> street2,
            ("AddressId" Lude..=) Lude.<$> addressId,
            ("City" Lude..=) Lude.<$> city,
            ("PhoneNumber" Lude..=) Lude.<$> phoneNumber,
            ("Company" Lude..=) Lude.<$> company,
            ("Name" Lude..=) Lude.<$> name,
            ("PrefectureOrDistrict" Lude..=) Lude.<$> prefectureOrDistrict,
            ("Street1" Lude..=) Lude.<$> street1
          ]
      )
