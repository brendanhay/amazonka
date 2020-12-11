-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.Pipeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Pipeline
  ( Pipeline (..),

    -- * Smart constructor
    mkPipeline,

    -- * Lenses
    pCreationTime,
    pArn,
    pActivities,
    pName,
    pReprocessingSummaries,
    pLastUpdateTime,
  )
where

import Network.AWS.IoTAnalytics.Types.PipelineActivity
import Network.AWS.IoTAnalytics.Types.ReprocessingSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a pipeline.
--
-- /See:/ 'mkPipeline' smart constructor.
data Pipeline = Pipeline'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    activities :: Lude.Maybe (Lude.NonEmpty PipelineActivity),
    name :: Lude.Maybe Lude.Text,
    reprocessingSummaries :: Lude.Maybe [ReprocessingSummary],
    lastUpdateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Pipeline' with the minimum fields required to make a request.
--
-- * 'activities' - The activities that perform transformations on the messages.
-- * 'arn' - The ARN of the pipeline.
-- * 'creationTime' - When the pipeline was created.
-- * 'lastUpdateTime' - The last time the pipeline was updated.
-- * 'name' - The name of the pipeline.
-- * 'reprocessingSummaries' - A summary of information about the pipeline reprocessing.
mkPipeline ::
  Pipeline
mkPipeline =
  Pipeline'
    { creationTime = Lude.Nothing,
      arn = Lude.Nothing,
      activities = Lude.Nothing,
      name = Lude.Nothing,
      reprocessingSummaries = Lude.Nothing,
      lastUpdateTime = Lude.Nothing
    }

-- | When the pipeline was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreationTime :: Lens.Lens' Pipeline (Lude.Maybe Lude.Timestamp)
pCreationTime = Lens.lens (creationTime :: Pipeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: Pipeline)
{-# DEPRECATED pCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The ARN of the pipeline.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pArn = Lens.lens (arn :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Pipeline)
{-# DEPRECATED pArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The activities that perform transformations on the messages.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActivities :: Lens.Lens' Pipeline (Lude.Maybe (Lude.NonEmpty PipelineActivity))
pActivities = Lens.lens (activities :: Pipeline -> Lude.Maybe (Lude.NonEmpty PipelineActivity)) (\s a -> s {activities = a} :: Pipeline)
{-# DEPRECATED pActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Pipeline (Lude.Maybe Lude.Text)
pName = Lens.lens (name :: Pipeline -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Pipeline)
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A summary of information about the pipeline reprocessing.
--
-- /Note:/ Consider using 'reprocessingSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pReprocessingSummaries :: Lens.Lens' Pipeline (Lude.Maybe [ReprocessingSummary])
pReprocessingSummaries = Lens.lens (reprocessingSummaries :: Pipeline -> Lude.Maybe [ReprocessingSummary]) (\s a -> s {reprocessingSummaries = a} :: Pipeline)
{-# DEPRECATED pReprocessingSummaries "Use generic-lens or generic-optics with 'reprocessingSummaries' instead." #-}

-- | The last time the pipeline was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastUpdateTime :: Lens.Lens' Pipeline (Lude.Maybe Lude.Timestamp)
pLastUpdateTime = Lens.lens (lastUpdateTime :: Pipeline -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdateTime = a} :: Pipeline)
{-# DEPRECATED pLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

instance Lude.FromJSON Pipeline where
  parseJSON =
    Lude.withObject
      "Pipeline"
      ( \x ->
          Pipeline'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "activities")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "reprocessingSummaries" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastUpdateTime")
      )
