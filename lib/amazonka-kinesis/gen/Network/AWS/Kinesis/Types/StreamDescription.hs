{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StreamDescription
  ( StreamDescription (..),

    -- * Smart constructor
    mkStreamDescription,

    -- * Lenses
    sdStreamName,
    sdStreamARN,
    sdStreamStatus,
    sdShards,
    sdHasMoreShards,
    sdRetentionPeriodHours,
    sdStreamCreationTimestamp,
    sdEnhancedMonitoring,
    sdEncryptionType,
    sdKeyId,
  )
where

import qualified Network.AWS.Kinesis.Types.EncryptionType as Types
import qualified Network.AWS.Kinesis.Types.EnhancedMetrics as Types
import qualified Network.AWS.Kinesis.Types.KeyId as Types
import qualified Network.AWS.Kinesis.Types.Shard as Types
import qualified Network.AWS.Kinesis.Types.StreamARN as Types
import qualified Network.AWS.Kinesis.Types.StreamName as Types
import qualified Network.AWS.Kinesis.Types.StreamStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output for 'DescribeStream' .
--
-- /See:/ 'mkStreamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { -- | The name of the stream being described.
    streamName :: Types.StreamName,
    -- | The Amazon Resource Name (ARN) for the stream being described.
    streamARN :: Types.StreamARN,
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
    streamStatus :: Types.StreamStatus,
    -- | The shards that comprise the stream.
    shards :: [Types.Shard],
    -- | If set to @true@ , more shards in the stream are available to describe.
    hasMoreShards :: Core.Bool,
    -- | The current retention period, in hours. Minimum value of 24. Maximum value of 168.
    retentionPeriodHours :: Core.Int,
    -- | The approximate time that the stream was created.
    streamCreationTimestamp :: Core.NominalDiffTime,
    -- | Represents the current enhanced monitoring settings of the stream.
    enhancedMonitoring :: [Types.EnhancedMetrics],
    -- | The server-side encryption type used on the stream. This parameter can be one of the following values:
    --
    --
    --     * @NONE@ : Do not encrypt the records in the stream.
    --
    --
    --     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
    encryptionType :: Core.Maybe Types.EncryptionType,
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
    keyId :: Core.Maybe Types.KeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StreamDescription' value with any optional fields omitted.
mkStreamDescription ::
  -- | 'streamName'
  Types.StreamName ->
  -- | 'streamARN'
  Types.StreamARN ->
  -- | 'streamStatus'
  Types.StreamStatus ->
  -- | 'hasMoreShards'
  Core.Bool ->
  -- | 'retentionPeriodHours'
  Core.Int ->
  -- | 'streamCreationTimestamp'
  Core.NominalDiffTime ->
  StreamDescription
mkStreamDescription
  streamName
  streamARN
  streamStatus
  hasMoreShards
  retentionPeriodHours
  streamCreationTimestamp =
    StreamDescription'
      { streamName,
        streamARN,
        streamStatus,
        shards = Core.mempty,
        hasMoreShards,
        retentionPeriodHours,
        streamCreationTimestamp,
        enhancedMonitoring = Core.mempty,
        encryptionType = Core.Nothing,
        keyId = Core.Nothing
      }

-- | The name of the stream being described.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamName :: Lens.Lens' StreamDescription Types.StreamName
sdStreamName = Lens.field @"streamName"
{-# DEPRECATED sdStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream being described.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamARN :: Lens.Lens' StreamDescription Types.StreamARN
sdStreamARN = Lens.field @"streamARN"
{-# DEPRECATED sdStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

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
sdStreamStatus :: Lens.Lens' StreamDescription Types.StreamStatus
sdStreamStatus = Lens.field @"streamStatus"
{-# DEPRECATED sdStreamStatus "Use generic-lens or generic-optics with 'streamStatus' instead." #-}

-- | The shards that comprise the stream.
--
-- /Note:/ Consider using 'shards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShards :: Lens.Lens' StreamDescription [Types.Shard]
sdShards = Lens.field @"shards"
{-# DEPRECATED sdShards "Use generic-lens or generic-optics with 'shards' instead." #-}

-- | If set to @true@ , more shards in the stream are available to describe.
--
-- /Note:/ Consider using 'hasMoreShards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdHasMoreShards :: Lens.Lens' StreamDescription Core.Bool
sdHasMoreShards = Lens.field @"hasMoreShards"
{-# DEPRECATED sdHasMoreShards "Use generic-lens or generic-optics with 'hasMoreShards' instead." #-}

-- | The current retention period, in hours. Minimum value of 24. Maximum value of 168.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdRetentionPeriodHours :: Lens.Lens' StreamDescription Core.Int
sdRetentionPeriodHours = Lens.field @"retentionPeriodHours"
{-# DEPRECATED sdRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

-- | The approximate time that the stream was created.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamCreationTimestamp :: Lens.Lens' StreamDescription Core.NominalDiffTime
sdStreamCreationTimestamp = Lens.field @"streamCreationTimestamp"
{-# DEPRECATED sdStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

-- | Represents the current enhanced monitoring settings of the stream.
--
-- /Note:/ Consider using 'enhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEnhancedMonitoring :: Lens.Lens' StreamDescription [Types.EnhancedMetrics]
sdEnhancedMonitoring = Lens.field @"enhancedMonitoring"
{-# DEPRECATED sdEnhancedMonitoring "Use generic-lens or generic-optics with 'enhancedMonitoring' instead." #-}

-- | The server-side encryption type used on the stream. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records in the stream.
--
--
--     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
--
--
-- /Note:/ Consider using 'encryptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEncryptionType :: Lens.Lens' StreamDescription (Core.Maybe Types.EncryptionType)
sdEncryptionType = Lens.field @"encryptionType"
{-# DEPRECATED sdEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

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
sdKeyId :: Lens.Lens' StreamDescription (Core.Maybe Types.KeyId)
sdKeyId = Lens.field @"keyId"
{-# DEPRECATED sdKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

instance Core.FromJSON StreamDescription where
  parseJSON =
    Core.withObject "StreamDescription" Core.$
      \x ->
        StreamDescription'
          Core.<$> (x Core..: "StreamName")
          Core.<*> (x Core..: "StreamARN")
          Core.<*> (x Core..: "StreamStatus")
          Core.<*> (x Core..:? "Shards" Core..!= Core.mempty)
          Core.<*> (x Core..: "HasMoreShards")
          Core.<*> (x Core..: "RetentionPeriodHours")
          Core.<*> (x Core..: "StreamCreationTimestamp")
          Core.<*> (x Core..:? "EnhancedMonitoring" Core..!= Core.mempty)
          Core.<*> (x Core..:? "EncryptionType")
          Core.<*> (x Core..:? "KeyId")
