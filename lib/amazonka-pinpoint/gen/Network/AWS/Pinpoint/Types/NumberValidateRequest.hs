{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | Specifies a phone number to validate and retrieve information about.
--
-- /See:/ 'mkNumberValidateRequest' smart constructor.
data NumberValidateRequest = NumberValidateRequest'
  { -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
    isoCountryCode :: Core.Maybe Core.Text,
    -- | The phone number to retrieve information about. The phone number that you provide should include a valid numeric country code. Otherwise, the operation might result in an error.
    phoneNumber :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NumberValidateRequest' value with any optional fields omitted.
mkNumberValidateRequest ::
  NumberValidateRequest
mkNumberValidateRequest =
  NumberValidateRequest'
    { isoCountryCode = Core.Nothing,
      phoneNumber = Core.Nothing
    }

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the phone number was originally registered.
--
-- /Note:/ Consider using 'isoCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrIsoCountryCode :: Lens.Lens' NumberValidateRequest (Core.Maybe Core.Text)
nvrIsoCountryCode = Lens.field @"isoCountryCode"
{-# DEPRECATED nvrIsoCountryCode "Use generic-lens or generic-optics with 'isoCountryCode' instead." #-}

-- | The phone number to retrieve information about. The phone number that you provide should include a valid numeric country code. Otherwise, the operation might result in an error.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nvrPhoneNumber :: Lens.Lens' NumberValidateRequest (Core.Maybe Core.Text)
nvrPhoneNumber = Lens.field @"phoneNumber"
{-# DEPRECATED nvrPhoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead." #-}

instance Core.FromJSON NumberValidateRequest where
  toJSON NumberValidateRequest {..} =
    Core.object
      ( Core.catMaybes
          [ ("IsoCountryCode" Core..=) Core.<$> isoCountryCode,
            ("PhoneNumber" Core..=) Core.<$> phoneNumber
          ]
      )
