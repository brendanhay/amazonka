-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamDescriptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StreamDescriptionSummary
  ( StreamDescriptionSummary (..),

    -- * Smart constructor
    mkStreamDescriptionSummary,

    -- * Lenses
    sdsEncryptionType,
    sdsKeyId,
    sdsConsumerCount,
    sdsStreamName,
    sdsStreamARN,
    sdsStreamStatus,
    sdsRetentionPeriodHours,
    sdsStreamCreationTimestamp,
    sdsEnhancedMonitoring,
    sdsOpenShardCount,
  )
where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.StreamStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output for 'DescribeStreamSummary'
--
-- /See:/ 'mkStreamDescriptionSummary' smart constructor.
data StreamDescriptionSummary = StreamDescriptionSummary'
  { encryptionType ::
      Lude.Maybe EncryptionType,
    keyId :: Lude.Maybe Lude.Text,
    consumerCount :: Lude.Maybe Lude.Natural,
    streamName :: Lude.Text,
    streamARN :: Lude.Text,
    streamStatus :: StreamStatus,
    retentionPeriodHours :: Lude.Int,
    streamCreationTimestamp :: Lude.Timestamp,
    enhancedMonitoring :: [EnhancedMetrics],
    openShardCount :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamDescriptionSummary' with the minimum fields required to make a request.
--
-- * 'consumerCount' - The number of enhanced fan-out consumers registered with the stream.
-- * 'encryptionType' - The encryption type used. This value is one of the following:
--
--
--     * @KMS@
--
--
--     * @NONE@
--
--
-- * 'enhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
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
-- * 'openShardCount' - The number of open shards in the stream.
-- * 'retentionPeriodHours' - The current retention period, in hours.
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
mkStreamDescriptionSummary ::
  -- | 'streamName'
  Lude.Text ->
  -- | 'streamARN'
  Lude.Text ->
  -- | 'streamStatus'
  StreamStatus ->
  -- | 'retentionPeriodHours'
  Lude.Int ->
  -- | 'streamCreationTimestamp'
  Lude.Timestamp ->
  -- | 'openShardCount'
  Lude.Natural ->
  StreamDescriptionSummary
mkStreamDescriptionSummary
  pStreamName_
  pStreamARN_
  pStreamStatus_
  pRetentionPeriodHours_
  pStreamCreationTimestamp_
  pOpenShardCount_ =
    StreamDescriptionSummary'
      { encryptionType = Lude.Nothing,
        keyId = Lude.Nothing,
        consumerCount = Lude.Nothing,
        streamName = pStreamName_,
        streamARN = pStreamARN_,
        streamStatus = pStreamStatus_,
        retentionPeriodHours = pRetentionPeriodHours_,
        streamCreationTimestamp = pStreamCreationTimestamp_,
        enhancedMonitoring = Lude.mempty,
        openShardCount = pOpenShardCount_
      }

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
sdsEncryptionType :: Lens.Lens' StreamDescriptionSummary (Lude.Maybe EncryptionType)
sdsEncryptionType = Lens.lens (encryptionType :: StreamDescriptionSummary -> Lude.Maybe EncryptionType) (\s a -> s {encryptionType = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsEncryptionType "Use generic-lens or generic-optics with 'encryptionType' instead." #-}

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
sdsKeyId :: Lens.Lens' StreamDescriptionSummary (Lude.Maybe Lude.Text)
sdsKeyId = Lens.lens (keyId :: StreamDescriptionSummary -> Lude.Maybe Lude.Text) (\s a -> s {keyId = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsKeyId "Use generic-lens or generic-optics with 'keyId' instead." #-}

-- | The number of enhanced fan-out consumers registered with the stream.
--
-- /Note:/ Consider using 'consumerCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsConsumerCount :: Lens.Lens' StreamDescriptionSummary (Lude.Maybe Lude.Natural)
sdsConsumerCount = Lens.lens (consumerCount :: StreamDescriptionSummary -> Lude.Maybe Lude.Natural) (\s a -> s {consumerCount = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsConsumerCount "Use generic-lens or generic-optics with 'consumerCount' instead." #-}

-- | The name of the stream being described.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamName :: Lens.Lens' StreamDescriptionSummary Lude.Text
sdsStreamName = Lens.lens (streamName :: StreamDescriptionSummary -> Lude.Text) (\s a -> s {streamName = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream being described.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamARN :: Lens.Lens' StreamDescriptionSummary Lude.Text
sdsStreamARN = Lens.lens (streamARN :: StreamDescriptionSummary -> Lude.Text) (\s a -> s {streamARN = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

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
sdsStreamStatus :: Lens.Lens' StreamDescriptionSummary StreamStatus
sdsStreamStatus = Lens.lens (streamStatus :: StreamDescriptionSummary -> StreamStatus) (\s a -> s {streamStatus = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsStreamStatus "Use generic-lens or generic-optics with 'streamStatus' instead." #-}

-- | The current retention period, in hours.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsRetentionPeriodHours :: Lens.Lens' StreamDescriptionSummary Lude.Int
sdsRetentionPeriodHours = Lens.lens (retentionPeriodHours :: StreamDescriptionSummary -> Lude.Int) (\s a -> s {retentionPeriodHours = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

-- | The approximate time that the stream was created.
--
-- /Note:/ Consider using 'streamCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsStreamCreationTimestamp :: Lens.Lens' StreamDescriptionSummary Lude.Timestamp
sdsStreamCreationTimestamp = Lens.lens (streamCreationTimestamp :: StreamDescriptionSummary -> Lude.Timestamp) (\s a -> s {streamCreationTimestamp = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsStreamCreationTimestamp "Use generic-lens or generic-optics with 'streamCreationTimestamp' instead." #-}

-- | Represents the current enhanced monitoring settings of the stream.
--
-- /Note:/ Consider using 'enhancedMonitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsEnhancedMonitoring :: Lens.Lens' StreamDescriptionSummary [EnhancedMetrics]
sdsEnhancedMonitoring = Lens.lens (enhancedMonitoring :: StreamDescriptionSummary -> [EnhancedMetrics]) (\s a -> s {enhancedMonitoring = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsEnhancedMonitoring "Use generic-lens or generic-optics with 'enhancedMonitoring' instead." #-}

-- | The number of open shards in the stream.
--
-- /Note:/ Consider using 'openShardCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsOpenShardCount :: Lens.Lens' StreamDescriptionSummary Lude.Natural
sdsOpenShardCount = Lens.lens (openShardCount :: StreamDescriptionSummary -> Lude.Natural) (\s a -> s {openShardCount = a} :: StreamDescriptionSummary)
{-# DEPRECATED sdsOpenShardCount "Use generic-lens or generic-optics with 'openShardCount' instead." #-}

instance Lude.FromJSON StreamDescriptionSummary where
  parseJSON =
    Lude.withObject
      "StreamDescriptionSummary"
      ( \x ->
          StreamDescriptionSummary'
            Lude.<$> (x Lude..:? "EncryptionType")
            Lude.<*> (x Lude..:? "KeyId")
            Lude.<*> (x Lude..:? "ConsumerCount")
            Lude.<*> (x Lude..: "StreamName")
            Lude.<*> (x Lude..: "StreamARN")
            Lude.<*> (x Lude..: "StreamStatus")
            Lude.<*> (x Lude..: "RetentionPeriodHours")
            Lude.<*> (x Lude..: "StreamCreationTimestamp")
            Lude.<*> (x Lude..:? "EnhancedMonitoring" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "OpenShardCount")
      )
