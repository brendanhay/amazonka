{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Firehose.Types.DeliveryStreamDescription
  ( DeliveryStreamDescription (..)
  -- * Smart constructor
  , mkDeliveryStreamDescription
  -- * Lenses
  , dsdDeliveryStreamName
  , dsdDeliveryStreamARN
  , dsdDeliveryStreamStatus
  , dsdDeliveryStreamType
  , dsdVersionId
  , dsdDestinations
  , dsdHasMoreDestinations
  , dsdCreateTimestamp
  , dsdDeliveryStreamEncryptionConfiguration
  , dsdFailureDescription
  , dsdLastUpdateTimestamp
  , dsdSource
  ) where

import qualified Network.AWS.Firehose.Types.DeliveryStreamARN as Types
import qualified Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration as Types
import qualified Network.AWS.Firehose.Types.DeliveryStreamName as Types
import qualified Network.AWS.Firehose.Types.DeliveryStreamStatus as Types
import qualified Network.AWS.Firehose.Types.DeliveryStreamType as Types
import qualified Network.AWS.Firehose.Types.DeliveryStreamVersionId as Types
import qualified Network.AWS.Firehose.Types.DestinationDescription as Types
import qualified Network.AWS.Firehose.Types.FailureDescription as Types
import qualified Network.AWS.Firehose.Types.SourceDescription as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a delivery stream.
--
-- /See:/ 'mkDeliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
  { deliveryStreamName :: Types.DeliveryStreamName
    -- ^ The name of the delivery stream.
  , deliveryStreamARN :: Types.DeliveryStreamARN
    -- ^ The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  , deliveryStreamStatus :: Types.DeliveryStreamStatus
    -- ^ The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
  , deliveryStreamType :: Types.DeliveryStreamType
    -- ^ The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
  , versionId :: Types.DeliveryStreamVersionId
    -- ^ Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
  , destinations :: [Types.DestinationDescription]
    -- ^ The destinations.
  , hasMoreDestinations :: Core.Bool
    -- ^ Indicates whether there are more destinations available to list.
  , createTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the delivery stream was created.
  , deliveryStreamEncryptionConfiguration :: Core.Maybe Types.DeliveryStreamEncryptionConfiguration
    -- ^ Indicates the server-side encryption (SSE) status for the delivery stream.
  , failureDescription :: Core.Maybe Types.FailureDescription
    -- ^ Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
  , lastUpdateTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the delivery stream was last updated.
  , source :: Core.Maybe Types.SourceDescription
    -- ^ If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeliveryStreamDescription' value with any optional fields omitted.
mkDeliveryStreamDescription
    :: Types.DeliveryStreamName -- ^ 'deliveryStreamName'
    -> Types.DeliveryStreamARN -- ^ 'deliveryStreamARN'
    -> Types.DeliveryStreamStatus -- ^ 'deliveryStreamStatus'
    -> Types.DeliveryStreamType -- ^ 'deliveryStreamType'
    -> Types.DeliveryStreamVersionId -- ^ 'versionId'
    -> Core.Bool -- ^ 'hasMoreDestinations'
    -> DeliveryStreamDescription
mkDeliveryStreamDescription deliveryStreamName deliveryStreamARN
  deliveryStreamStatus deliveryStreamType versionId
  hasMoreDestinations
  = DeliveryStreamDescription'{deliveryStreamName, deliveryStreamARN,
                               deliveryStreamStatus, deliveryStreamType, versionId,
                               destinations = Core.mempty, hasMoreDestinations,
                               createTimestamp = Core.Nothing,
                               deliveryStreamEncryptionConfiguration = Core.Nothing,
                               failureDescription = Core.Nothing,
                               lastUpdateTimestamp = Core.Nothing, source = Core.Nothing}

-- | The name of the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamName :: Lens.Lens' DeliveryStreamDescription Types.DeliveryStreamName
dsdDeliveryStreamName = Lens.field @"deliveryStreamName"
{-# INLINEABLE dsdDeliveryStreamName #-}
{-# DEPRECATED deliveryStreamName "Use generic-lens or generic-optics with 'deliveryStreamName' instead"  #-}

-- | The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'deliveryStreamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamARN :: Lens.Lens' DeliveryStreamDescription Types.DeliveryStreamARN
dsdDeliveryStreamARN = Lens.field @"deliveryStreamARN"
{-# INLINEABLE dsdDeliveryStreamARN #-}
{-# DEPRECATED deliveryStreamARN "Use generic-lens or generic-optics with 'deliveryStreamARN' instead"  #-}

-- | The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
--
-- /Note:/ Consider using 'deliveryStreamStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamStatus :: Lens.Lens' DeliveryStreamDescription Types.DeliveryStreamStatus
dsdDeliveryStreamStatus = Lens.field @"deliveryStreamStatus"
{-# INLINEABLE dsdDeliveryStreamStatus #-}
{-# DEPRECATED deliveryStreamStatus "Use generic-lens or generic-optics with 'deliveryStreamStatus' instead"  #-}

-- | The delivery stream type. This can be one of the following values:
--
--
--     * @DirectPut@ : Provider applications access the delivery stream directly.
--
--
--     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
--
--
-- /Note:/ Consider using 'deliveryStreamType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamType :: Lens.Lens' DeliveryStreamDescription Types.DeliveryStreamType
dsdDeliveryStreamType = Lens.field @"deliveryStreamType"
{-# INLINEABLE dsdDeliveryStreamType #-}
{-# DEPRECATED deliveryStreamType "Use generic-lens or generic-optics with 'deliveryStreamType' instead"  #-}

-- | Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdVersionId :: Lens.Lens' DeliveryStreamDescription Types.DeliveryStreamVersionId
dsdVersionId = Lens.field @"versionId"
{-# INLINEABLE dsdVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The destinations.
--
-- /Note:/ Consider using 'destinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDestinations :: Lens.Lens' DeliveryStreamDescription [Types.DestinationDescription]
dsdDestinations = Lens.field @"destinations"
{-# INLINEABLE dsdDestinations #-}
{-# DEPRECATED destinations "Use generic-lens or generic-optics with 'destinations' instead"  #-}

-- | Indicates whether there are more destinations available to list.
--
-- /Note:/ Consider using 'hasMoreDestinations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdHasMoreDestinations :: Lens.Lens' DeliveryStreamDescription Core.Bool
dsdHasMoreDestinations = Lens.field @"hasMoreDestinations"
{-# INLINEABLE dsdHasMoreDestinations #-}
{-# DEPRECATED hasMoreDestinations "Use generic-lens or generic-optics with 'hasMoreDestinations' instead"  #-}

-- | The date and time that the delivery stream was created.
--
-- /Note:/ Consider using 'createTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdCreateTimestamp :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Core.NominalDiffTime)
dsdCreateTimestamp = Lens.field @"createTimestamp"
{-# INLINEABLE dsdCreateTimestamp #-}
{-# DEPRECATED createTimestamp "Use generic-lens or generic-optics with 'createTimestamp' instead"  #-}

-- | Indicates the server-side encryption (SSE) status for the delivery stream.
--
-- /Note:/ Consider using 'deliveryStreamEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdDeliveryStreamEncryptionConfiguration :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Types.DeliveryStreamEncryptionConfiguration)
dsdDeliveryStreamEncryptionConfiguration = Lens.field @"deliveryStreamEncryptionConfiguration"
{-# INLINEABLE dsdDeliveryStreamEncryptionConfiguration #-}
{-# DEPRECATED deliveryStreamEncryptionConfiguration "Use generic-lens or generic-optics with 'deliveryStreamEncryptionConfiguration' instead"  #-}

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- /Note:/ Consider using 'failureDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdFailureDescription :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Types.FailureDescription)
dsdFailureDescription = Lens.field @"failureDescription"
{-# INLINEABLE dsdFailureDescription #-}
{-# DEPRECATED failureDescription "Use generic-lens or generic-optics with 'failureDescription' instead"  #-}

-- | The date and time that the delivery stream was last updated.
--
-- /Note:/ Consider using 'lastUpdateTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdLastUpdateTimestamp :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Core.NominalDiffTime)
dsdLastUpdateTimestamp = Lens.field @"lastUpdateTimestamp"
{-# INLINEABLE dsdLastUpdateTimestamp #-}
{-# DEPRECATED lastUpdateTimestamp "Use generic-lens or generic-optics with 'lastUpdateTimestamp' instead"  #-}

-- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsdSource :: Lens.Lens' DeliveryStreamDescription (Core.Maybe Types.SourceDescription)
dsdSource = Lens.field @"source"
{-# INLINEABLE dsdSource #-}
{-# DEPRECATED source "Use generic-lens or generic-optics with 'source' instead"  #-}

instance Core.FromJSON DeliveryStreamDescription where
        parseJSON
          = Core.withObject "DeliveryStreamDescription" Core.$
              \ x ->
                DeliveryStreamDescription' Core.<$>
                  (x Core..: "DeliveryStreamName") Core.<*>
                    x Core..: "DeliveryStreamARN"
                    Core.<*> x Core..: "DeliveryStreamStatus"
                    Core.<*> x Core..: "DeliveryStreamType"
                    Core.<*> x Core..: "VersionId"
                    Core.<*> x Core..:? "Destinations" Core..!= Core.mempty
                    Core.<*> x Core..: "HasMoreDestinations"
                    Core.<*> x Core..:? "CreateTimestamp"
                    Core.<*> x Core..:? "DeliveryStreamEncryptionConfiguration"
                    Core.<*> x Core..:? "FailureDescription"
                    Core.<*> x Core..:? "LastUpdateTimestamp"
                    Core.<*> x Core..:? "Source"
