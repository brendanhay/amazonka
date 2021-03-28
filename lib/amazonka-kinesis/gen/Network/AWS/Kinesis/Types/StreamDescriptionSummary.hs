{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamDescriptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.StreamDescriptionSummary
  ( StreamDescriptionSummary (..)
  -- * Smart constructor
  , mkStreamDescriptionSummary
  -- * Lenses
  , sdsStreamName
  , sdsStreamARN
  , sdsStreamStatus
  , sdsRetentionPeriodHours
  , sdsStreamCreationTimestamp
  , sdsEnhancedMonitoring
  , sdsOpenShardCount
  , sdsConsumerCount
  , sdsEncryptionType
  , sdsKeyId
  ) where

import qualified Network.AWS.Kinesis.Types.EncryptionType as Types
import qualified Network.AWS.Kinesis.Types.EnhancedMetrics as Types
import qualified Network.AWS.Kinesis.Types.KeyId as Types
import qualified Network.AWS.Kinesis.Types.StreamARN as Types
import qualified Network.AWS.Kinesis.Types.StreamName as Types
import qualified Network.AWS.Kinesis.Types.StreamStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output for 'DescribeStreamSummary' 
--
-- /See:/ 'mkStreamDescriptionSummary' smart constructor.
data StreamDescriptionSummary = StreamDescriptionSummary'
  { streamName :: Types.StreamName
    -- ^ The name of the stream being described.
  , streamARN :: Types.StreamARN
    -- ^ The Amazon Resource Name (ARN) for the stream being described.
  , streamStatus :: Types.StreamStatus
    -- ^ The current status of the stream being described. The stream status is one of the following states:
--
--
--     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .
--
--
--     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.
--
--
--     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.
--
--
--     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
--
  , retentionPeriodHours :: Core.Int
    -- ^ The current retention period, in hours.
  , streamCreationTimestamp :: Core.NominalDiffTime
    -- ^ The approximate time that the stream was created.
  , enhancedMonitoring :: [Types.EnhancedMetrics]
    -- ^ Represents the current enhanced monitoring settings of the stream.
  , openShardCount :: Core.Natural
    -- ^ The number of open shards in the stream.
  , consumerCount :: Core.Maybe Core.Natural
    -- ^ The number of enhanced fan-out consumers registered with the stream.
  , encryptionType :: Core.Maybe Types.EncryptionType
    -- ^ The encryption type used. This value is one of the following:
--
--
--     * @KMS@ 
--
--
--     * @NONE@ 
--
--
  , keyId :: Core.Maybe Types.KeyId
    -- ^ The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .
--
--
--     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@ 
--
--
--     * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@ 
--
--
--     * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@ 
--
--
--     * Alias name example: @alias/MyAliasName@ 
--
--
--     * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@ 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StreamDescriptionSummary' value with any optional fields omitted.
mkStreamDescriptionSummary
    :: Types.StreamName -- ^ 'streamName'
    -> Types.StreamARN -- ^ 'streamARN'
    -> Types.StreamStatus -- ^ 'streamStatus'
    -> Core.Int -- ^ 'retentionPeriodHours'
    -> Core.NominalDiffTime -- ^ 'streamCreationTimestamp'
    -> Core.Natural -- ^ 'openShardCount'
    -> StreamDescriptionSummary
mkStreamDescriptionSummary streamName streamARN streamStatus
  retentionPeriodHours streamCreationTimestamp openShardCount
  = StreamDescriptionSummary'{streamName, streamARN, streamStatus,
                              retentionPeriodHours, streamCreationTimestamp,
                              enhancedMonitoring = Core.mempty, openShardCount,
                              consumerCount = Core.Nothing, encryptionType = Core.Nothing,
                              keyId = Core.Nothing}

-- | The name of the stream being described.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamName :: Lens.Lens' StreamDescriptionSummary Types.StreamName
sdsStreamName = Lens.field @"streamName"
{-# INLINEABLE sdsStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | The Amazon Resource Name (ARN) for the stream being described.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamARN :: Lens.Lens' StreamDescriptionSummary Types.StreamARN
sdsStreamARN = Lens.field @"streamARN"
{-# INLINEABLE sdsStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

-- | The current status of the stream being described. The stream status is one of the following states:
--
--
--     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .
--
--
--     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.
--
--
--     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.
--
--
--     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
--
--
-- /Note:/ Consider using 'streamStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamStatus :: Lens.Lens' StreamDescriptionSummary Types.StreamStatus
sdsStreamStatus = Lens.field @"streamStatus"
{-# INLINEABLE sdsStreamStatus #-}
{-# DEPRECATED streamStatus "Use generic-lens or generic-optics with 'streamStatus' instead"  #-}

-- | The current retention period, in hours.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsRetentionPeriodHours :: Lens.Lens' StreamDescriptionSummary Core.Int
sdsRetentionPeriodHours = Lens.field @"retentionPeriodHours"
{-# INLINEABLE sdsRetentionPeriodHours #-}
{-# DEPRECATED retentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead"  #-}

-- | The approximate time that the stream was created.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamCreationTimestamp :: Lens.Lens' StreamDescriptionSummary Core.NominalDiffTime
sdsStreamCreationTimestamp = Lens.field @"streamCreationTimestamp"
{-# INLINEABLE sdsStreamCreationTimestamp #-}
{-# DEPRECATED streamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead"  #-}

-- | Represents the current enhanced monitoring settings of the stream.
--
-- /Note:/ Consider using 'enhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsEnhancedMonitoring :: Lens.Lens' StreamDescriptionSummary [Types.EnhancedMetrics]
sdsEnhancedMonitoring = Lens.field @"enhancedMonitoring"
{-# INLINEABLE sdsEnhancedMonitoring #-}
{-# DEPRECATED enhancedMonitoring "Use generic-lens or generic-optics with 'enhancedMonitoring' instead"  #-}

-- | The number of open shards in the stream.
--
-- /Note:/ Consider using 'openShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsOpenShardCount :: Lens.Lens' StreamDescriptionSummary Core.Natural
sdsOpenShardCount = Lens.field @"openShardCount"
{-# INLINEABLE sdsOpenShardCount #-}
{-# DEPRECATED openShardCount "Use generic-lens or generic-optics with 'openShardCount' instead"  #-}

-- | The number of enhanced fan-out consumers registered with the stream.
--
-- /Note:/ Consider using 'consumerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsConsumerCount :: Lens.Lens' StreamDescriptionSummary (Core.Maybe Core.Natural)
sdsConsumerCount = Lens.field @"consumerCount"
{-# INLINEABLE sdsConsumerCount #-}
{-# DEPRECATED consumerCount "Use generic-lens or generic-optics with 'consumerCount' instead"  #-}

-- | The encryption type used. This value is one of the following:
--
--
--     * @KMS@ 
--
--
--     * @NONE@ 
--
--
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsEncryptionType :: Lens.Lens' StreamDescriptionSummary (Core.Maybe Types.EncryptionType)
sdsEncryptionType = Lens.field @"encryptionType"
{-# INLINEABLE sdsEncryptionType #-}
{-# DEPRECATED encryptionType "Use generic-lens or generic-optics with 'encryptionType' instead"  #-}

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .
--
--
--     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@ 
--
--
--     * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@ 
--
--
--     * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@ 
--
--
--     * Alias name example: @alias/MyAliasName@ 
--
--
--     * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@ 
--
--
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsKeyId :: Lens.Lens' StreamDescriptionSummary (Core.Maybe Types.KeyId)
sdsKeyId = Lens.field @"keyId"
{-# INLINEABLE sdsKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

instance Core.FromJSON StreamDescriptionSummary where
        parseJSON
          = Core.withObject "StreamDescriptionSummary" Core.$
              \ x ->
                StreamDescriptionSummary' Core.<$>
                  (x Core..: "StreamName") Core.<*> x Core..: "StreamARN" Core.<*>
                    x Core..: "StreamStatus"
                    Core.<*> x Core..: "RetentionPeriodHours"
                    Core.<*> x Core..: "StreamCreationTimestamp"
                    Core.<*> x Core..:? "EnhancedMonitoring" Core..!= Core.mempty
                    Core.<*> x Core..: "OpenShardCount"
                    Core.<*> x Core..:? "ConsumerCount"
                    Core.<*> x Core..:? "EncryptionType"
                    Core.<*> x Core..:? "KeyId"
