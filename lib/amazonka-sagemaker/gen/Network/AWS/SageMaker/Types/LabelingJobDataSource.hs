{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobDataSource
  ( LabelingJobDataSource (..),

    -- * Smart constructor
    mkLabelingJobDataSource,

    -- * Lenses
    ljdsS3DataSource,
    ljdsSnsDataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.LabelingJobS3DataSource as Types
import qualified Network.AWS.SageMaker.Types.LabelingJobSnsDataSource as Types

-- | Provides information about the location of input data.
--
-- You must specify at least one of the following: @S3DataSource@ or @SnsDataSource@ .
-- Use @SnsDataSource@ to specify an SNS input topic for a streaming labeling job. If you do not specify and SNS input topic ARN, Ground Truth will create a one-time labeling job.
-- Use @S3DataSource@ to specify an input manifest file for both streaming and one-time labeling jobs. Adding an @S3DataSource@ is optional if you use @SnsDataSource@ to create a streaming labeling job.
--
-- /See:/ 'mkLabelingJobDataSource' smart constructor.
data LabelingJobDataSource = LabelingJobDataSource'
  { -- | The Amazon S3 location of the input data objects.
    s3DataSource :: Core.Maybe Types.LabelingJobS3DataSource,
    -- | An Amazon SNS data source used for streaming labeling jobs.
    snsDataSource :: Core.Maybe Types.LabelingJobSnsDataSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobDataSource' value with any optional fields omitted.
mkLabelingJobDataSource ::
  LabelingJobDataSource
mkLabelingJobDataSource =
  LabelingJobDataSource'
    { s3DataSource = Core.Nothing,
      snsDataSource = Core.Nothing
    }

-- | The Amazon S3 location of the input data objects.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljdsS3DataSource :: Lens.Lens' LabelingJobDataSource (Core.Maybe Types.LabelingJobS3DataSource)
ljdsS3DataSource = Lens.field @"s3DataSource"
{-# DEPRECATED ljdsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

-- | An Amazon SNS data source used for streaming labeling jobs.
--
-- /Note:/ Consider using 'snsDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljdsSnsDataSource :: Lens.Lens' LabelingJobDataSource (Core.Maybe Types.LabelingJobSnsDataSource)
ljdsSnsDataSource = Lens.field @"snsDataSource"
{-# DEPRECATED ljdsSnsDataSource "Use generic-lens or generic-optics with 'snsDataSource' instead." #-}

instance Core.FromJSON LabelingJobDataSource where
  toJSON LabelingJobDataSource {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3DataSource" Core..=) Core.<$> s3DataSource,
            ("SnsDataSource" Core..=) Core.<$> snsDataSource
          ]
      )

instance Core.FromJSON LabelingJobDataSource where
  parseJSON =
    Core.withObject "LabelingJobDataSource" Core.$
      \x ->
        LabelingJobDataSource'
          Core.<$> (x Core..:? "S3DataSource") Core.<*> (x Core..:? "SnsDataSource")
