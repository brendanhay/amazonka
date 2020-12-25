{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    pActivities,
    pArn,
    pCreationTime,
    pLastUpdateTime,
    pName,
    pReprocessingSummaries,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.PipelineActivity as Types
import qualified Network.AWS.IoTAnalytics.Types.PipelineArn as Types
import qualified Network.AWS.IoTAnalytics.Types.PipelineName as Types
import qualified Network.AWS.IoTAnalytics.Types.ReprocessingSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a pipeline.
--
-- /See:/ 'mkPipeline' smart constructor.
data Pipeline = Pipeline'
  { -- | The activities that perform transformations on the messages.
    activities :: Core.Maybe (Core.NonEmpty Types.PipelineActivity),
    -- | The ARN of the pipeline.
    arn :: Core.Maybe Types.PipelineArn,
    -- | When the pipeline was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The last time the pipeline was updated.
    lastUpdateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the pipeline.
    name :: Core.Maybe Types.PipelineName,
    -- | A summary of information about the pipeline reprocessing.
    reprocessingSummaries :: Core.Maybe [Types.ReprocessingSummary]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Pipeline' value with any optional fields omitted.
mkPipeline ::
  Pipeline
mkPipeline =
  Pipeline'
    { activities = Core.Nothing,
      arn = Core.Nothing,
      creationTime = Core.Nothing,
      lastUpdateTime = Core.Nothing,
      name = Core.Nothing,
      reprocessingSummaries = Core.Nothing
    }

-- | The activities that perform transformations on the messages.
--
-- /Note:/ Consider using 'activities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pActivities :: Lens.Lens' Pipeline (Core.Maybe (Core.NonEmpty Types.PipelineActivity))
pActivities = Lens.field @"activities"
{-# DEPRECATED pActivities "Use generic-lens or generic-optics with 'activities' instead." #-}

-- | The ARN of the pipeline.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pArn :: Lens.Lens' Pipeline (Core.Maybe Types.PipelineArn)
pArn = Lens.field @"arn"
{-# DEPRECATED pArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | When the pipeline was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pCreationTime :: Lens.Lens' Pipeline (Core.Maybe Core.NominalDiffTime)
pCreationTime = Lens.field @"creationTime"
{-# DEPRECATED pCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The last time the pipeline was updated.
--
-- /Note:/ Consider using 'lastUpdateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pLastUpdateTime :: Lens.Lens' Pipeline (Core.Maybe Core.NominalDiffTime)
pLastUpdateTime = Lens.field @"lastUpdateTime"
{-# DEPRECATED pLastUpdateTime "Use generic-lens or generic-optics with 'lastUpdateTime' instead." #-}

-- | The name of the pipeline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Pipeline (Core.Maybe Types.PipelineName)
pName = Lens.field @"name"
{-# DEPRECATED pName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A summary of information about the pipeline reprocessing.
--
-- /Note:/ Consider using 'reprocessingSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pReprocessingSummaries :: Lens.Lens' Pipeline (Core.Maybe [Types.ReprocessingSummary])
pReprocessingSummaries = Lens.field @"reprocessingSummaries"
{-# DEPRECATED pReprocessingSummaries "Use generic-lens or generic-optics with 'reprocessingSummaries' instead." #-}

instance Core.FromJSON Pipeline where
  parseJSON =
    Core.withObject "Pipeline" Core.$
      \x ->
        Pipeline'
          Core.<$> (x Core..:? "activities")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "lastUpdateTime")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "reprocessingSummaries")
