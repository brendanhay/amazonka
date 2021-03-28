{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunStageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.ReplicationRunStageDetails
  ( ReplicationRunStageDetails (..)
  -- * Smart constructor
  , mkReplicationRunStageDetails
  -- * Lenses
  , rrsdStage
  , rrsdStageProgress
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ReplicationRunStageProgress as Types
import qualified Network.AWS.SMS.Types.Stage as Types

-- | Details of the current stage of a replication run.
--
-- /See:/ 'mkReplicationRunStageDetails' smart constructor.
data ReplicationRunStageDetails = ReplicationRunStageDetails'
  { stage :: Core.Maybe Types.Stage
    -- ^ The current stage of a replication run.
  , stageProgress :: Core.Maybe Types.ReplicationRunStageProgress
    -- ^ The progress of the current stage of a replication run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReplicationRunStageDetails' value with any optional fields omitted.
mkReplicationRunStageDetails
    :: ReplicationRunStageDetails
mkReplicationRunStageDetails
  = ReplicationRunStageDetails'{stage = Core.Nothing,
                                stageProgress = Core.Nothing}

-- | The current stage of a replication run.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsdStage :: Lens.Lens' ReplicationRunStageDetails (Core.Maybe Types.Stage)
rrsdStage = Lens.field @"stage"
{-# INLINEABLE rrsdStage #-}
{-# DEPRECATED stage "Use generic-lens or generic-optics with 'stage' instead"  #-}

-- | The progress of the current stage of a replication run.
--
-- /Note:/ Consider using 'stageProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsdStageProgress :: Lens.Lens' ReplicationRunStageDetails (Core.Maybe Types.ReplicationRunStageProgress)
rrsdStageProgress = Lens.field @"stageProgress"
{-# INLINEABLE rrsdStageProgress #-}
{-# DEPRECATED stageProgress "Use generic-lens or generic-optics with 'stageProgress' instead"  #-}

instance Core.FromJSON ReplicationRunStageDetails where
        parseJSON
          = Core.withObject "ReplicationRunStageDetails" Core.$
              \ x ->
                ReplicationRunStageDetails' Core.<$>
                  (x Core..:? "stage") Core.<*> x Core..:? "stageProgress"
