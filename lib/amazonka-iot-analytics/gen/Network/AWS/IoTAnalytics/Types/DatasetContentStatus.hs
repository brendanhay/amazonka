{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.DatasetContentStatus
  ( DatasetContentStatus (..)
  -- * Smart constructor
  , mkDatasetContentStatus
  -- * Lenses
  , dcsReason
  , dcsState
  ) where

import qualified Network.AWS.IoTAnalytics.Types.DatasetContentState as Types
import qualified Network.AWS.IoTAnalytics.Types.Reason as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The state of the data set contents and the reason they are in this state.
--
-- /See:/ 'mkDatasetContentStatus' smart constructor.
data DatasetContentStatus = DatasetContentStatus'
  { reason :: Core.Maybe Types.Reason
    -- ^ The reason the data set contents are in this state.
  , state :: Core.Maybe Types.DatasetContentState
    -- ^ The state of the data set contents. Can be one of READY, CREATING, SUCCEEDED, or FAILED.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DatasetContentStatus' value with any optional fields omitted.
mkDatasetContentStatus
    :: DatasetContentStatus
mkDatasetContentStatus
  = DatasetContentStatus'{reason = Core.Nothing,
                          state = Core.Nothing}

-- | The reason the data set contents are in this state.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsReason :: Lens.Lens' DatasetContentStatus (Core.Maybe Types.Reason)
dcsReason = Lens.field @"reason"
{-# INLINEABLE dcsReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

-- | The state of the data set contents. Can be one of READY, CREATING, SUCCEEDED, or FAILED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsState :: Lens.Lens' DatasetContentStatus (Core.Maybe Types.DatasetContentState)
dcsState = Lens.field @"state"
{-# INLINEABLE dcsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON DatasetContentStatus where
        parseJSON
          = Core.withObject "DatasetContentStatus" Core.$
              \ x ->
                DatasetContentStatus' Core.<$>
                  (x Core..:? "reason") Core.<*> x Core..:? "state"
