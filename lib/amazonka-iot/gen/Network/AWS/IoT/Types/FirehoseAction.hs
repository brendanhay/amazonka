{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.FirehoseAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.FirehoseAction
  ( FirehoseAction (..)
  -- * Smart constructor
  , mkFirehoseAction
  -- * Lenses
  , faRoleArn
  , faDeliveryStreamName
  , faBatchMode
  , faSeparator
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.DeliveryStreamName as Types
import qualified Network.AWS.IoT.Types.FirehoseSeparator as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action that writes data to an Amazon Kinesis Firehose stream.
--
-- /See:/ 'mkFirehoseAction' smart constructor.
data FirehoseAction = FirehoseAction'
  { roleArn :: Types.AwsArn
    -- ^ The IAM role that grants access to the Amazon Kinesis Firehose stream.
  , deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The delivery stream name.
  , batchMode :: Core.Maybe Core.Bool
    -- ^ Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
  , separator :: Core.Maybe Types.FirehoseSeparator
    -- ^ A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FirehoseAction' value with any optional fields omitted.
mkFirehoseAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> FirehoseAction
mkFirehoseAction roleArn deliveryStreamName
  = FirehoseAction'{roleArn, deliveryStreamName,
                    batchMode = Core.Nothing, separator = Core.Nothing}

-- | The IAM role that grants access to the Amazon Kinesis Firehose stream.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faRoleArn :: Lens.Lens' FirehoseAction Types.AwsArn
faRoleArn = Lens.field @"roleArn"
{-# INLINEABLE faRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The delivery stream name.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faDeliveryStreamName :: Lens.Lens' FirehoseAction Types.DeliveryStreamName
faDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE faDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | Whether to deliver the Kinesis Data Firehose stream as a batch by using <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > . The default value is @false@ .
--
-- When @batchMode@ is @true@ and the rule's SQL statement evaluates to an Array, each Array element forms one record in the <https://docs.aws.amazon.com/firehose/latest/APIReference/API_PutRecordBatch.html @PutRecordBatch@ > request. The resulting array can't have more than 500 records.
--
-- /Note:/ Consider using 'batchMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faBatchMode :: Lens.Lens' FirehoseAction (Core.Maybe Core.Bool)
faBatchMode = Lens.field @"batchMode"
{-# INLINEABLE faBatchMode #-}
{-# DEPRECATED batchMode "Use generic-lens or generic-optics with 'batchMode' instead"  #-}

-- | A character separator that will be used to separate records written to the Firehose stream. Valid values are: '\n' (newline), '\t' (tab), '\r\n' (Windows newline), ',' (comma).
--
-- /Note:/ Consider using 'separator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faSeparator :: Lens.Lens' FirehoseAction (Core.Maybe Types.FirehoseSeparator)
faSeparator = Lens.field @"separator"
{-# INLINEABLE faSeparator #-}
{-# DEPRECATED separator "Use generic-lens or generic-optics with 'separator' instead"  #-}

instance Core.FromJSON FirehoseAction where
        toJSON FirehoseAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("deliveryStreamName" Core..= deliveryStreamName),
                  ("batchMode" Core..=) Core.<$> batchMode,
                  ("separator" Core..=) Core.<$> separator])

instance Core.FromJSON FirehoseAction where
        parseJSON
          = Core.withObject "FirehoseAction" Core.$
              \ x ->
                FirehoseAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "deliveryStreamName"
                    Core.<*> x Core..:? "batchMode"
                    Core.<*> x Core..:? "separator"
