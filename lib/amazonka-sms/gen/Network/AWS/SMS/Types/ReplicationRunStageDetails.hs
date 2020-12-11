-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunStageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRunStageDetails
  ( ReplicationRunStageDetails (..),

    -- * Smart constructor
    mkReplicationRunStageDetails,

    -- * Lenses
    rrsdStage,
    rrsdStageProgress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details of the current stage of a replication run.
--
-- /See:/ 'mkReplicationRunStageDetails' smart constructor.
data ReplicationRunStageDetails = ReplicationRunStageDetails'
  { stage ::
      Lude.Maybe Lude.Text,
    stageProgress :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplicationRunStageDetails' with the minimum fields required to make a request.
--
-- * 'stage' - The current stage of a replication run.
-- * 'stageProgress' - The progress of the current stage of a replication run.
mkReplicationRunStageDetails ::
  ReplicationRunStageDetails
mkReplicationRunStageDetails =
  ReplicationRunStageDetails'
    { stage = Lude.Nothing,
      stageProgress = Lude.Nothing
    }

-- | The current stage of a replication run.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsdStage :: Lens.Lens' ReplicationRunStageDetails (Lude.Maybe Lude.Text)
rrsdStage = Lens.lens (stage :: ReplicationRunStageDetails -> Lude.Maybe Lude.Text) (\s a -> s {stage = a} :: ReplicationRunStageDetails)
{-# DEPRECATED rrsdStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | The progress of the current stage of a replication run.
--
-- /Note:/ Consider using 'stageProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsdStageProgress :: Lens.Lens' ReplicationRunStageDetails (Lude.Maybe Lude.Text)
rrsdStageProgress = Lens.lens (stageProgress :: ReplicationRunStageDetails -> Lude.Maybe Lude.Text) (\s a -> s {stageProgress = a} :: ReplicationRunStageDetails)
{-# DEPRECATED rrsdStageProgress "Use generic-lens or generic-optics with 'stageProgress' instead." #-}

instance Lude.FromJSON ReplicationRunStageDetails where
  parseJSON =
    Lude.withObject
      "ReplicationRunStageDetails"
      ( \x ->
          ReplicationRunStageDetails'
            Lude.<$> (x Lude..:? "stage") Lude.<*> (x Lude..:? "stageProgress")
      )
