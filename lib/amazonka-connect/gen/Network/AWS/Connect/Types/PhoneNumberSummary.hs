{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberSummary
  ( PhoneNumberSummary (..),

    -- * Smart constructor
    mkPhoneNumberSummary,

    -- * Lenses
    pnsPhoneNumberType,
    pnsARN,
    pnsPhoneNumber,
    pnsPhoneNumberCountryCode,
    pnsId,
  )
where

import Network.AWS.Connect.Types.PhoneNumberCountryCode
import Network.AWS.Connect.Types.PhoneNumberType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains summary information about a phone number for a contact center.
--
-- /See:/ 'mkPhoneNumberSummary' smart constructor.
data PhoneNumberSummary = PhoneNumberSummary'
  { -- | The type of phone number.
    phoneNumberType :: Lude.Maybe PhoneNumberType,
    -- | The Amazon Resource Name (ARN) of the phone number.
    arn :: Lude.Maybe Lude.Text,
    -- | The phone number.
    phoneNumber :: Lude.Maybe Lude.Text,
    -- | The ISO country code.
    phoneNumberCountryCode :: Lude.Maybe PhoneNumberCountryCode,
    -- | The identifier of the phone number.
    id :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PhoneNumberSummary' with the minimum fields required to make a request.
--
-- * 'phoneNumberType' - The type of phone number.
-- * 'arn' - The Amazon Resource Name (ARN) of the phone number.
-- * 'phoneNumber' - The phone number.
-- * 'phoneNumberCountryCode' - The ISO country code.
-- * 'id' - The identifier of the phone number.
mkPhoneNumberSummary ::
  PhoneNumberSummary
mkPhoneNumberSummary =
  PhoneNumberSummary'
    { phoneNumberType = Lude.Nothing,
      arn = Lude.Nothing,
      phoneNumber = Lude.Nothing,
      phoneNumberCountryCode = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The type of phone number.
--
-- /Note:/ Consider using 'phoneNumberType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPhoneNumberType :: Lens.Lens' PhoneNumberSummary (Lude.Maybe PhoneNumberType)
pnsPhoneNumberType = Lens.lens (phoneNumberType :: PhoneNumberSummary -> Lude.Maybe PhoneNumberType) (\s a -> s {phoneNumberType = a} :: PhoneNumberSummary)
{-# DEPRECATED pnsPhoneNumberType "Use generic-lens or generic-optics with 'phoneNumberType' instead." #-}

-- | The Amazon Resource Name (ARN) of the phone number.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsARN :: Lens.Lens' PhoneNumberSummary (Lude.Maybe Lude.Text)
pnsARN = Lens.lens (arn :: PhoneNumberSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PhoneNumberSummary)
{-# DEPRECATED pnsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The phone number.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPhoneNumber :: Lens.Lens' PhoneNumberSummary (Lude.Maybe Lude.Text)
pnsPhoneNumber = Lens.lens (phoneNumber :: PhoneNumberSummary -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: PhoneNumberSummary)
{-# DEPRECATED pnsPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

-- | The ISO country code.
--
-- /Note:/ Consider using 'phoneNumberCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPhoneNumberCountryCode :: Lens.Lens' PhoneNumberSummary (Lude.Maybe PhoneNumberCountryCode)
pnsPhoneNumberCountryCode = Lens.lens (phoneNumberCountryCode :: PhoneNumberSummary -> Lude.Maybe PhoneNumberCountryCode) (\s a -> s {phoneNumberCountryCode = a} :: PhoneNumberSummary)
{-# DEPRECATED pnsPhoneNumberCountryCode "Use generic-lens or generic-optics with 'phoneNumberCountryCode' instead." #-}

-- | The identifier of the phone number.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsId :: Lens.Lens' PhoneNumberSummary (Lude.Maybe Lude.Text)
pnsId = Lens.lens (id :: PhoneNumberSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PhoneNumberSummary)
{-# DEPRECATED pnsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON PhoneNumberSummary where
  parseJSON =
    Lude.withObject
      "PhoneNumberSummary"
      ( \x ->
          PhoneNumberSummary'
            Lude.<$> (x Lude..:? "PhoneNumberType")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "PhoneNumber")
            Lude.<*> (x Lude..:? "PhoneNumberCountryCode")
            Lude.<*> (x Lude..:? "Id")
      )
