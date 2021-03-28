{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.ReplayDestination
  ( ReplayDestination (..)
  -- * Smart constructor
  , mkReplayDestination
  -- * Lenses
  , rdArn
  , rdFilterArns
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A @ReplayDestination@ object that contains details about a replay.
--
-- /See:/ 'mkReplayDestination' smart constructor.
data ReplayDestination = ReplayDestination'
  { arn :: Types.Arn
    -- ^ The ARN of the event bus to replay event to. You can replay events only to the event bus specified to create the archive.
  , filterArns :: Core.Maybe [Types.Arn]
    -- ^ A list of ARNs for rules to replay events to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplayDestination' value with any optional fields omitted.
mkReplayDestination
    :: Types.Arn -- ^ 'arn'
    -> ReplayDestination
mkReplayDestination arn
  = ReplayDestination'{arn, filterArns = Core.Nothing}

-- | The ARN of the event bus to replay event to. You can replay events only to the event bus specified to create the archive.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdArn :: Lens.Lens' ReplayDestination Types.Arn
rdArn = Lens.field @"arn"
{-# INLINEABLE rdArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A list of ARNs for rules to replay events to.
--
-- /Note:/ Consider using 'filterArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdFilterArns :: Lens.Lens' ReplayDestination (Core.Maybe [Types.Arn])
rdFilterArns = Lens.field @"filterArns"
{-# INLINEABLE rdFilterArns #-}
{-# DEPRECATED filterArns "Use generic-lens or generic-optics with 'filterArns' instead"  #-}

instance Core.FromJSON ReplayDestination where
        toJSON ReplayDestination{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Arn" Core..= arn),
                  ("FilterArns" Core..=) Core.<$> filterArns])

instance Core.FromJSON ReplayDestination where
        parseJSON
          = Core.withObject "ReplayDestination" Core.$
              \ x ->
                ReplayDestination' Core.<$>
                  (x Core..: "Arn") Core.<*> x Core..:? "FilterArns"
