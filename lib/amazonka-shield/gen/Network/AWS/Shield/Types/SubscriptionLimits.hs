{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SubscriptionLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.SubscriptionLimits
  ( SubscriptionLimits (..)
  -- * Smart constructor
  , mkSubscriptionLimits
  -- * Lenses
  , slProtectionLimits
  , slProtectionGroupLimits
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Shield.Types.ProtectionGroupLimits as Types
import qualified Network.AWS.Shield.Types.ProtectionLimits as Types

-- | Limits settings for your subscription. 
--
-- /See:/ 'mkSubscriptionLimits' smart constructor.
data SubscriptionLimits = SubscriptionLimits'
  { protectionLimits :: Types.ProtectionLimits
    -- ^ Limits settings on protections for your subscription. 
  , protectionGroupLimits :: Types.ProtectionGroupLimits
    -- ^ Limits settings on protection groups for your subscription. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubscriptionLimits' value with any optional fields omitted.
mkSubscriptionLimits
    :: Types.ProtectionLimits -- ^ 'protectionLimits'
    -> Types.ProtectionGroupLimits -- ^ 'protectionGroupLimits'
    -> SubscriptionLimits
mkSubscriptionLimits protectionLimits protectionGroupLimits
  = SubscriptionLimits'{protectionLimits, protectionGroupLimits}

-- | Limits settings on protections for your subscription. 
--
-- /Note:/ Consider using 'protectionLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slProtectionLimits :: Lens.Lens' SubscriptionLimits Types.ProtectionLimits
slProtectionLimits = Lens.field @"protectionLimits"
{-# INLINEABLE slProtectionLimits #-}
{-# DEPRECATED protectionLimits "Use generic-lens or generic-optics with 'protectionLimits' instead"  #-}

-- | Limits settings on protection groups for your subscription. 
--
-- /Note:/ Consider using 'protectionGroupLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slProtectionGroupLimits :: Lens.Lens' SubscriptionLimits Types.ProtectionGroupLimits
slProtectionGroupLimits = Lens.field @"protectionGroupLimits"
{-# INLINEABLE slProtectionGroupLimits #-}
{-# DEPRECATED protectionGroupLimits "Use generic-lens or generic-optics with 'protectionGroupLimits' instead"  #-}

instance Core.FromJSON SubscriptionLimits where
        parseJSON
          = Core.withObject "SubscriptionLimits" Core.$
              \ x ->
                SubscriptionLimits' Core.<$>
                  (x Core..: "ProtectionLimits") Core.<*>
                    x Core..: "ProtectionGroupLimits"
