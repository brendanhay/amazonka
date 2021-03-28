{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.PhoneNumberSummary
  ( PhoneNumberSummary (..)
  -- * Smart constructor
  , mkPhoneNumberSummary
  -- * Lenses
  , pnsArn
  , pnsId
  , pnsPhoneNumber
  , pnsPhoneNumberCountryCode
  , pnsPhoneNumberType
  ) where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.PhoneNumber as Types
import qualified Network.AWS.Connect.Types.PhoneNumberCountryCode as Types
import qualified Network.AWS.Connect.Types.PhoneNumberId as Types
import qualified Network.AWS.Connect.Types.PhoneNumberType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a phone number for a contact center.
--
-- /See:/ 'mkPhoneNumberSummary' smart constructor.
data PhoneNumberSummary = PhoneNumberSummary'
  { arn :: Core.Maybe Types.ARN
    -- ^ The Amazon Resource Name (ARN) of the phone number.
  , id :: Core.Maybe Types.PhoneNumberId
    -- ^ The identifier of the phone number.
  , phoneNumber :: Core.Maybe Types.PhoneNumber
    -- ^ The phone number.
  , phoneNumberCountryCode :: Core.Maybe Types.PhoneNumberCountryCode
    -- ^ The ISO country code.
  , phoneNumberType :: Core.Maybe Types.PhoneNumberType
    -- ^ The type of phone number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PhoneNumberSummary' value with any optional fields omitted.
mkPhoneNumberSummary
    :: PhoneNumberSummary
mkPhoneNumberSummary
  = PhoneNumberSummary'{arn = Core.Nothing, id = Core.Nothing,
                        phoneNumber = Core.Nothing, phoneNumberCountryCode = Core.Nothing,
                        phoneNumberType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the phone number.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsArn :: Lens.Lens' PhoneNumberSummary (Core.Maybe Types.ARN)
pnsArn = Lens.field @"arn"
{-# INLINEABLE pnsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier of the phone number.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsId :: Lens.Lens' PhoneNumberSummary (Core.Maybe Types.PhoneNumberId)
pnsId = Lens.field @"id"
{-# INLINEABLE pnsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The phone number.
--
-- /Note:/ Consider using 'phoneNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPhoneNumber :: Lens.Lens' PhoneNumberSummary (Core.Maybe Types.PhoneNumber)
pnsPhoneNumber = Lens.field @"phoneNumber"
{-# INLINEABLE pnsPhoneNumber #-}
{-# DEPRECATED phoneNumber "Use generic-lens or generic-optics with 'phoneNumber' instead"  #-}

-- | The ISO country code.
--
-- /Note:/ Consider using 'phoneNumberCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPhoneNumberCountryCode :: Lens.Lens' PhoneNumberSummary (Core.Maybe Types.PhoneNumberCountryCode)
pnsPhoneNumberCountryCode = Lens.field @"phoneNumberCountryCode"
{-# INLINEABLE pnsPhoneNumberCountryCode #-}
{-# DEPRECATED phoneNumberCountryCode "Use generic-lens or generic-optics with 'phoneNumberCountryCode' instead"  #-}

-- | The type of phone number.
--
-- /Note:/ Consider using 'phoneNumberType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnsPhoneNumberType :: Lens.Lens' PhoneNumberSummary (Core.Maybe Types.PhoneNumberType)
pnsPhoneNumberType = Lens.field @"phoneNumberType"
{-# INLINEABLE pnsPhoneNumberType #-}
{-# DEPRECATED phoneNumberType "Use generic-lens or generic-optics with 'phoneNumberType' instead"  #-}

instance Core.FromJSON PhoneNumberSummary where
        parseJSON
          = Core.withObject "PhoneNumberSummary" Core.$
              \ x ->
                PhoneNumberSummary' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "PhoneNumber"
                    Core.<*> x Core..:? "PhoneNumberCountryCode"
                    Core.<*> x Core..:? "PhoneNumberType"
