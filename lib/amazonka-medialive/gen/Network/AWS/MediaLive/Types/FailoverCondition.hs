{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FailoverCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.FailoverCondition
  ( FailoverCondition (..)
  -- * Smart constructor
  , mkFailoverCondition
  -- * Lenses
  , fcFailoverConditionSettings
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.FailoverConditionSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Failover Condition settings. There can be multiple failover conditions inside AutomaticInputFailoverSettings.
--
-- /See:/ 'mkFailoverCondition' smart constructor.
newtype FailoverCondition = FailoverCondition'
  { failoverConditionSettings :: Core.Maybe Types.FailoverConditionSettings
    -- ^ Failover condition type-specific settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverCondition' value with any optional fields omitted.
mkFailoverCondition
    :: FailoverCondition
mkFailoverCondition
  = FailoverCondition'{failoverConditionSettings = Core.Nothing}

-- | Failover condition type-specific settings.
--
-- /Note:/ Consider using 'failoverConditionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcFailoverConditionSettings :: Lens.Lens' FailoverCondition (Core.Maybe Types.FailoverConditionSettings)
fcFailoverConditionSettings = Lens.field @"failoverConditionSettings"
{-# INLINEABLE fcFailoverConditionSettings #-}
{-# DEPRECATED failoverConditionSettings "Use generic-lens or generic-optics with 'failoverConditionSettings' instead"  #-}

instance Core.FromJSON FailoverCondition where
        toJSON FailoverCondition{..}
          = Core.object
              (Core.catMaybes
                 [("failoverConditionSettings" Core..=) Core.<$>
                    failoverConditionSettings])

instance Core.FromJSON FailoverCondition where
        parseJSON
          = Core.withObject "FailoverCondition" Core.$
              \ x ->
                FailoverCondition' Core.<$>
                  (x Core..:? "failoverConditionSettings")
