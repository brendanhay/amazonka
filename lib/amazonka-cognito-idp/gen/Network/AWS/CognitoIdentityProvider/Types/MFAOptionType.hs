{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
  ( MFAOptionType (..)
  -- * Smart constructor
  , mkMFAOptionType
  -- * Lenses
  , mfaotAttributeName
  , mfaotDeliveryMedium
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AttributeNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /This data type is no longer supported./ You can use it only for SMS MFA configurations. You can't use it for TOTP software token MFA configurations.
--
-- /See:/ 'mkMFAOptionType' smart constructor.
data MFAOptionType = MFAOptionType'
  { attributeName :: Core.Maybe Types.AttributeNameType
    -- ^ The attribute name of the MFA option type. The only valid value is @phone_number@ .
  , deliveryMedium :: Core.Maybe Types.DeliveryMediumType
    -- ^ The delivery medium to send the MFA code. You can use this parameter to set only the @SMS@ delivery medium value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MFAOptionType' value with any optional fields omitted.
mkMFAOptionType
    :: MFAOptionType
mkMFAOptionType
  = MFAOptionType'{attributeName = Core.Nothing,
                   deliveryMedium = Core.Nothing}

-- | The attribute name of the MFA option type. The only valid value is @phone_number@ .
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfaotAttributeName :: Lens.Lens' MFAOptionType (Core.Maybe Types.AttributeNameType)
mfaotAttributeName = Lens.field @"attributeName"
{-# INLINEABLE mfaotAttributeName #-}
{-# DEPRECATED attributeName "Use generic-lens or generic-optics with 'attributeName' instead"  #-}

-- | The delivery medium to send the MFA code. You can use this parameter to set only the @SMS@ delivery medium value.
--
-- /Note:/ Consider using 'deliveryMedium' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfaotDeliveryMedium :: Lens.Lens' MFAOptionType (Core.Maybe Types.DeliveryMediumType)
mfaotDeliveryMedium = Lens.field @"deliveryMedium"
{-# INLINEABLE mfaotDeliveryMedium #-}
{-# DEPRECATED deliveryMedium "Use generic-lens or generic-optics with 'deliveryMedium' instead"  #-}

instance Core.FromJSON MFAOptionType where
        toJSON MFAOptionType{..}
          = Core.object
              (Core.catMaybes
                 [("AttributeName" Core..=) Core.<$> attributeName,
                  ("DeliveryMedium" Core..=) Core.<$> deliveryMedium])

instance Core.FromJSON MFAOptionType where
        parseJSON
          = Core.withObject "MFAOptionType" Core.$
              \ x ->
                MFAOptionType' Core.<$>
                  (x Core..:? "AttributeName") Core.<*> x Core..:? "DeliveryMedium"
