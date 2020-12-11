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
    ljdsSNSDataSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource
import Network.AWS.SageMaker.Types.LabelingJobSNSDataSource

-- | Provides information about the location of input data.
--
-- You must specify at least one of the following: @S3DataSource@ or @SnsDataSource@ .
-- Use @SnsDataSource@ to specify an SNS input topic for a streaming labeling job. If you do not specify and SNS input topic ARN, Ground Truth will create a one-time labeling job.
-- Use @S3DataSource@ to specify an input manifest file for both streaming and one-time labeling jobs. Adding an @S3DataSource@ is optional if you use @SnsDataSource@ to create a streaming labeling job.
--
-- /See:/ 'mkLabelingJobDataSource' smart constructor.
data LabelingJobDataSource = LabelingJobDataSource'
  { s3DataSource ::
      Lude.Maybe LabelingJobS3DataSource,
    snsDataSource ::
      Lude.Maybe LabelingJobSNSDataSource
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobDataSource' with the minimum fields required to make a request.
--
-- * 's3DataSource' - The Amazon S3 location of the input data objects.
-- * 'snsDataSource' - An Amazon SNS data source used for streaming labeling jobs.
mkLabelingJobDataSource ::
  LabelingJobDataSource
mkLabelingJobDataSource =
  LabelingJobDataSource'
    { s3DataSource = Lude.Nothing,
      snsDataSource = Lude.Nothing
    }

-- | The Amazon S3 location of the input data objects.
--
-- /Note:/ Consider using 's3DataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljdsS3DataSource :: Lens.Lens' LabelingJobDataSource (Lude.Maybe LabelingJobS3DataSource)
ljdsS3DataSource = Lens.lens (s3DataSource :: LabelingJobDataSource -> Lude.Maybe LabelingJobS3DataSource) (\s a -> s {s3DataSource = a} :: LabelingJobDataSource)
{-# DEPRECATED ljdsS3DataSource "Use generic-lens or generic-optics with 's3DataSource' instead." #-}

-- | An Amazon SNS data source used for streaming labeling jobs.
--
-- /Note:/ Consider using 'snsDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljdsSNSDataSource :: Lens.Lens' LabelingJobDataSource (Lude.Maybe LabelingJobSNSDataSource)
ljdsSNSDataSource = Lens.lens (snsDataSource :: LabelingJobDataSource -> Lude.Maybe LabelingJobSNSDataSource) (\s a -> s {snsDataSource = a} :: LabelingJobDataSource)
{-# DEPRECATED ljdsSNSDataSource "Use generic-lens or generic-optics with 'snsDataSource' instead." #-}

instance Lude.FromJSON LabelingJobDataSource where
  parseJSON =
    Lude.withObject
      "LabelingJobDataSource"
      ( \x ->
          LabelingJobDataSource'
            Lude.<$> (x Lude..:? "S3DataSource") Lude.<*> (x Lude..:? "SnsDataSource")
      )

instance Lude.ToJSON LabelingJobDataSource where
  toJSON LabelingJobDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3DataSource" Lude..=) Lude.<$> s3DataSource,
            ("SnsDataSource" Lude..=) Lude.<$> snsDataSource
          ]
      )
