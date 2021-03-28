{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
  ( UserPoolAddOnsType (..)
  -- * Smart constructor
  , mkUserPoolAddOnsType
  -- * Lenses
  , upaotAdvancedSecurityMode
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The user pool add-ons type.
--
-- /See:/ 'mkUserPoolAddOnsType' smart constructor.
newtype UserPoolAddOnsType = UserPoolAddOnsType'
  { advancedSecurityMode :: Types.AdvancedSecurityModeType
    -- ^ The advanced security mode.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UserPoolAddOnsType' value with any optional fields omitted.
mkUserPoolAddOnsType
    :: Types.AdvancedSecurityModeType -- ^ 'advancedSecurityMode'
    -> UserPoolAddOnsType
mkUserPoolAddOnsType advancedSecurityMode
  = UserPoolAddOnsType'{advancedSecurityMode}

-- | The advanced security mode.
--
-- /Note:/ Consider using 'advancedSecurityMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upaotAdvancedSecurityMode :: Lens.Lens' UserPoolAddOnsType Types.AdvancedSecurityModeType
upaotAdvancedSecurityMode = Lens.field @"advancedSecurityMode"
{-# INLINEABLE upaotAdvancedSecurityMode #-}
{-# DEPRECATED advancedSecurityMode "Use generic-lens or generic-optics with 'advancedSecurityMode' instead"  #-}

instance Core.FromJSON UserPoolAddOnsType where
        toJSON UserPoolAddOnsType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AdvancedSecurityMode" Core..= advancedSecurityMode)])

instance Core.FromJSON UserPoolAddOnsType where
        parseJSON
          = Core.withObject "UserPoolAddOnsType" Core.$
              \ x ->
                UserPoolAddOnsType' Core.<$> (x Core..: "AdvancedSecurityMode")
