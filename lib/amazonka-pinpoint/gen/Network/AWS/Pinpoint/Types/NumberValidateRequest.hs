-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.NumberValidateRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.NumberValidateRequest
  ( NumberValidateRequest (..),

    -- * Smart constructor
    mkNumberValidateRequest,

    -- * Lenses
    nvrIsoCountryCode,
    nvrPhoneNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a phone number to validate and retrieve information about.
--
-- /See:/ 'mkNumberValidateRequest' smart constructor.
data NumberValidateRequest = NumberValidateRequest'
  { isoCountryCode ::
      Lude.Maybe Lude.Text,
    phoneNumber :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NumberValidateRequest' with the minimum fields required to make a request.
--
-- * 'isoCountryCode' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
-- * 'phoneNumber' - The phone number to retrieve information about. The phone number that you provide should include a valid numeric country code. Otherwise, the operation might result in an error.
mkNumberValidateRequest ::
  NumberValidateRequest
mkNumberValidateRequest =
  NumberValidateRequest'
    { isoCountryCode = Lude.Nothing,
      phoneNumber = Lude.Nothing
    }

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'isoCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrIsoCountryCode :: Lens.Lens' NumberValidateRequest (Lude.Maybe Lude.Text)
nvrIsoCountryCode = Lens.lens (isoCountryCode :: NumberValidateRequest -> Lude.Maybe Lude.Text) (\s a -> s {isoCountryCode = a} :: NumberValidateRequest)
{-# DEPRECATED nvrIsoCountryCode "Use generic-lens or generic-optics with 'isoCountryCode' instead." #-}

-- | The phone number to retrieve information about. The phone number that you provide should include a valid numeric country code. Otherwise, the operation might result in an error.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrPhoneNumber :: Lens.Lens' NumberValidateRequest (Lude.Maybe Lude.Text)
nvrPhoneNumber = Lens.lens (phoneNumber :: NumberValidateRequest -> Lude.Maybe Lude.Text) (\s a -> s {phoneNumber = a} :: NumberValidateRequest)
{-# DEPRECATED nvrPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Lude.ToJSON NumberValidateRequest where
  toJSON NumberValidateRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IsoCountryCode" Lude..=) Lude.<$> isoCountryCode,
            ("PhoneNumber" Lude..=) Lude.<$> phoneNumber
          ]
      )
