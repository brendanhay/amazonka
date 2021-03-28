{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobSnsDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.LabelingJobSnsDataSource
  ( LabelingJobSnsDataSource (..)
  -- * Smart constructor
  , mkLabelingJobSnsDataSource
  -- * Lenses
  , ljsdsSnsTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SnsTopicArn as Types

-- | An Amazon SNS data source used for streaming labeling jobs.
--
-- /See:/ 'mkLabelingJobSnsDataSource' smart constructor.
newtype LabelingJobSnsDataSource = LabelingJobSnsDataSource'
  { snsTopicArn :: Types.SnsTopicArn
    -- ^ The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job.
--
-- If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobSnsDataSource' value with any optional fields omitted.
mkLabelingJobSnsDataSource
    :: Types.SnsTopicArn -- ^ 'snsTopicArn'
    -> LabelingJobSnsDataSource
mkLabelingJobSnsDataSource snsTopicArn
  = LabelingJobSnsDataSource'{snsTopicArn}

-- | The Amazon SNS input topic Amazon Resource Name (ARN). Specify the ARN of the input topic you will use to send new data objects to a streaming labeling job.
--
-- If you specify an input topic for @SnsTopicArn@ in @InputConfig@ , you must specify a value for @SnsTopicArn@ in @OutputConfig@ .
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljsdsSnsTopicArn :: Lens.Lens' LabelingJobSnsDataSource Types.SnsTopicArn
ljsdsSnsTopicArn = Lens.field @"snsTopicArn"
{-# INLINEABLE ljsdsSnsTopicArn #-}
{-# DEPRECATED snsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead"  #-}

instance Core.FromJSON LabelingJobSnsDataSource where
        toJSON LabelingJobSnsDataSource{..}
          = Core.object
              (Core.catMaybes [Core.Just ("SnsTopicArn" Core..= snsTopicArn)])

instance Core.FromJSON LabelingJobSnsDataSource where
        parseJSON
          = Core.withObject "LabelingJobSnsDataSource" Core.$
              \ x -> LabelingJobSnsDataSource' Core.<$> (x Core..: "SnsTopicArn")
