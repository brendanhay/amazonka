{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSNSDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobSNSDataSource
  ( LabelingJobSNSDataSource (..),

    -- * Smart constructor
    mkLabelingJobSNSDataSource,

    -- * Lenses
    ljsdsSNSTopicARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An Amazon SNS data source used for streaming labeling jobs.
--
-- /See:/ 'mkLabelingJobSNSDataSource' smart constructor.
newtype LabelingJobSNSDataSource = LabelingJobSNSDataSource'
  { -- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job.
    --
    -- If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
    snsTopicARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobSNSDataSource' with the minimum fields required to make a request.
--
-- * 'snsTopicARN' - The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job.
--
-- If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
mkLabelingJobSNSDataSource ::
  -- | 'snsTopicARN'
  Lude.Text ->
  LabelingJobSNSDataSource
mkLabelingJobSNSDataSource pSNSTopicARN_ =
  LabelingJobSNSDataSource' {snsTopicARN = pSNSTopicARN_}

-- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job.
--
-- If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsdsSNSTopicARN :: Lens.Lens' LabelingJobSNSDataSource Lude.Text
ljsdsSNSTopicARN = Lens.lens (snsTopicARN :: LabelingJobSNSDataSource -> Lude.Text) (\s a -> s {snsTopicARN = a} :: LabelingJobSNSDataSource)
{-# DEPRECATED ljsdsSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

instance Lude.FromJSON LabelingJobSNSDataSource where
  parseJSON =
    Lude.withObject
      "LabelingJobSNSDataSource"
      ( \x ->
          LabelingJobSNSDataSource' Lude.<$> (x Lude..: "SnsTopicArn")
      )

instance Lude.ToJSON LabelingJobSNSDataSource where
  toJSON LabelingJobSNSDataSource' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SnsTopicArn" Lude..= snsTopicARN)])
