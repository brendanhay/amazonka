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
    sdEncryptionType,
    sdKeyId,
    sdStreamName,
    sdStreamARN,
    sdStreamStatus,
    sdShards,
    sdHasMoreShards,
    sdRetentionPeriodHours,
    sdStreamCreationTimestamp,
    sdEnhancedMonitoring,
  )
where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.StreamStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output for 'DescribeStream' .
--
-- /See:/ 'mkStreamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { encryptionType ::
      Lude.Maybe EncryptionType,
    keyId :: Lude.Maybe Lude.Text,
    streamName :: Lude.Text,
    streamARN :: Lude.Text,
    streamStatus :: StreamStatus,
    shards :: [Shard],
    hasMoreShards :: Lude.Bool,
    retentionPeriodHours :: Lude.Int,
    streamCreationTimestamp :: Lude.Timestamp,
    enhancedMonitoring :: [EnhancedMetrics]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamDescription' with the minimum fields required to make a request.
--
-- * 'encryptionType' - The server-side encryption type used on the stream. This parameter can be one of the following values:
--
--
--     * @NONE@ : Do not encrypt the records in the stream.
--
--
--     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
--
-- * 'enhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
-- * 'hasMoreShards' - If set to @true@ , more shards in the stream are available to describe.
-- * 'keyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .
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
-- * 'retentionPeriodHours' - The current retention period, in hours. Minimum value of 24. Maximum value of 168.
-- * 'shards' - The shards that comprise the stream.
-- * 'streamARN' - The Amazon Resource Name (ARN) for the stream being described.
-- * 'streamCreationTimestamp' - The approximate time that the stream was created.
-- * 'streamName' - The name of the stream being described.
-- * 'streamStatus' - The current status of the stream being described. The stream status is one of the following states:
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
mkStreamDescription ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'streamARN'
  Lude.Text ->
  -- | 'streamStatus'
  StreamStatus ->
  -- | 'hasMoreShards'
  Lude.Bool ->
  -- | 'retentionPeriodHours'
  Lude.Int ->
  -- | 'streamCreationTimestamp'
  Lude.Timestamp ->
  StreamDescription
mkStreamDescription
  pStreamName_
  pStreamARN_
  pStreamStatus_
  pHasMoreShards_
  pRetentionPeriodHours_
  pStreamCreationTimestamp_ =
    StreamDescription'
      { encryptionType = Lude.Nothing,
        keyId = Lude.Nothing,
        streamName = pStreamName_,
        streamARN = pStreamARN_,
        streamStatus = pStreamStatus_,
        shards = Lude.mempty,
        hasMoreShards = pHasMoreShards_,
        retentionPeriodHours = pRetentionPeriodHours_,
        streamCreationTimestamp = pStreamCreationTimestamp_,
        enhancedMonitoring = Lude.mempty
      }

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
sdEncryptionType :: Lens.Lens' StreamDescription (Lude.Maybe EncryptionType)
sdEncryptionType = Lens.lens (encryptionType :: StreamDescription -> Lude.Maybe EncryptionType) (\s a -> s {encryptionType = a} :: StreamDescription)
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
sdKeyId :: Lens.Lens' StreamDescription (Lude.Maybe Lude.Text)
sdKeyId = Lens.lens (keyId :: StreamDescription -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: StreamDescription)
{-# DEPRECATED sdKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The name of the stream being described.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamName :: Lens.Lens' StreamDescription Lude.Text
sdStreamName = Lens.lens (streamName :: StreamDescription -> Lude.Text) (\s a -> s {streamName = a} :: StreamDescription)
{-# DEPRECATED sdStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream being described.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamARN :: Lens.Lens' StreamDescription Lude.Text
sdStreamARN = Lens.lens (streamARN :: StreamDescription -> Lude.Text) (\s a -> s {streamARN = a} :: StreamDescription)
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
sdStreamStatus :: Lens.Lens' StreamDescription StreamStatus
sdStreamStatus = Lens.lens (streamStatus :: StreamDescription -> StreamStatus) (\s a -> s {streamStatus = a} :: StreamDescription)
{-# DEPRECATED sdStreamStatus "Use generic-lens or generic-optics with 'streamStatus' instead." #-}

-- | The shards that comprise the stream.
--
-- /Note:/ Consider using 'shards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShards :: Lens.Lens' StreamDescription [Shard]
sdShards = Lens.lens (shards :: StreamDescription -> [Shard]) (\s a -> s {shards = a} :: StreamDescription)
{-# DEPRECATED sdShards "Use generic-lens or generic-optics with 'shards' instead." #-}

-- | If set to @true@ , more shards in the stream are available to describe.
--
-- /Note:/ Consider using 'hasMoreShards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdHasMoreShards :: Lens.Lens' StreamDescription Lude.Bool
sdHasMoreShards = Lens.lens (hasMoreShards :: StreamDescription -> Lude.Bool) (\s a -> s {hasMoreShards = a} :: StreamDescription)
{-# DEPRECATED sdHasMoreShards "Use generic-lens or generic-optics with 'hasMoreShards' instead." #-}

-- | The current retention period, in hours. Minimum value of 24. Maximum value of 168.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdRetentionPeriodHours :: Lens.Lens' StreamDescription Lude.Int
sdRetentionPeriodHours = Lens.lens (retentionPeriodHours :: StreamDescription -> Lude.Int) (\s a -> s {retentionPeriodHours = a} :: StreamDescription)
{-# DEPRECATED sdRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

-- | The approximate time that the stream was created.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamCreationTimestamp :: Lens.Lens' StreamDescription Lude.Timestamp
sdStreamCreationTimestamp = Lens.lens (streamCreationTimestamp :: StreamDescription -> Lude.Timestamp) (\s a -> s {streamCreationTimestamp = a} :: StreamDescription)
{-# DEPRECATED sdStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

-- | Represents the current enhanced monitoring settings of the stream.
--
-- /Note:/ Consider using 'enhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEnhancedMonitoring :: Lens.Lens' StreamDescription [EnhancedMetrics]
sdEnhancedMonitoring = Lens.lens (enhancedMonitoring :: StreamDescription -> [EnhancedMetrics]) (\s a -> s {enhancedMonitoring = a} :: StreamDescription)
{-# DEPRECATED sdEnhancedMonitoring "Use generic-lens or generic-optics with 'enhancedMonitoring' instead." #-}

instance Lude.FromJSON StreamDescription where
  parseJSON =
    Lude.withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            Lude.<$> (x Lude..:? "EncryptionType")
            Lude.<*> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..: "StreamName")
            Lude.<*> (x Lude..: "StreamARN")
            Lude.<*> (x Lude..: "StreamStatus")
            Lude.<*> (x Lude..:? "Shards" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "HasMoreShards")
            Lude.<*> (x Lude..: "RetentionPeriodHours")
            Lude.<*> (x Lude..: "StreamCreationTimestamp")
            Lude.<*> (x Lude..:? "EnhancedMonitoring" Lude..!= Lude.mempty)
      )
