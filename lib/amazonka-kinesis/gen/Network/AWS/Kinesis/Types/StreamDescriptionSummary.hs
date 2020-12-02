{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamDescriptionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StreamDescriptionSummary where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.StreamStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output for 'DescribeStreamSummary'
--
--
--
-- /See:/ 'streamDescriptionSummary' smart constructor.
data StreamDescriptionSummary = StreamDescriptionSummary'
  { _sdsEncryptionType ::
      !(Maybe EncryptionType),
    _sdsKeyId :: !(Maybe Text),
    _sdsConsumerCount :: !(Maybe Nat),
    _sdsStreamName :: !Text,
    _sdsStreamARN :: !Text,
    _sdsStreamStatus :: !StreamStatus,
    _sdsRetentionPeriodHours :: !Int,
    _sdsStreamCreationTimestamp :: !POSIX,
    _sdsEnhancedMonitoring ::
      ![EnhancedMetrics],
    _sdsOpenShardCount :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamDescriptionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdsEncryptionType' - The encryption type used. This value is one of the following:     * @KMS@      * @NONE@
--
-- * 'sdsKeyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
--
-- * 'sdsConsumerCount' - The number of enhanced fan-out consumers registered with the stream.
--
-- * 'sdsStreamName' - The name of the stream being described.
--
-- * 'sdsStreamARN' - The Amazon Resource Name (ARN) for the stream being described.
--
-- * 'sdsStreamStatus' - The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
-- * 'sdsRetentionPeriodHours' - The current retention period, in hours.
--
-- * 'sdsStreamCreationTimestamp' - The approximate time that the stream was created.
--
-- * 'sdsEnhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
--
-- * 'sdsOpenShardCount' - The number of open shards in the stream.
streamDescriptionSummary ::
  -- | 'sdsStreamName'
  Text ->
  -- | 'sdsStreamARN'
  Text ->
  -- | 'sdsStreamStatus'
  StreamStatus ->
  -- | 'sdsRetentionPeriodHours'
  Int ->
  -- | 'sdsStreamCreationTimestamp'
  UTCTime ->
  -- | 'sdsOpenShardCount'
  Natural ->
  StreamDescriptionSummary
streamDescriptionSummary
  pStreamName_
  pStreamARN_
  pStreamStatus_
  pRetentionPeriodHours_
  pStreamCreationTimestamp_
  pOpenShardCount_ =
    StreamDescriptionSummary'
      { _sdsEncryptionType = Nothing,
        _sdsKeyId = Nothing,
        _sdsConsumerCount = Nothing,
        _sdsStreamName = pStreamName_,
        _sdsStreamARN = pStreamARN_,
        _sdsStreamStatus = pStreamStatus_,
        _sdsRetentionPeriodHours = pRetentionPeriodHours_,
        _sdsStreamCreationTimestamp = _Time # pStreamCreationTimestamp_,
        _sdsEnhancedMonitoring = mempty,
        _sdsOpenShardCount = _Nat # pOpenShardCount_
      }

-- | The encryption type used. This value is one of the following:     * @KMS@      * @NONE@
sdsEncryptionType :: Lens' StreamDescriptionSummary (Maybe EncryptionType)
sdsEncryptionType = lens _sdsEncryptionType (\s a -> s {_sdsEncryptionType = a})

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
sdsKeyId :: Lens' StreamDescriptionSummary (Maybe Text)
sdsKeyId = lens _sdsKeyId (\s a -> s {_sdsKeyId = a})

-- | The number of enhanced fan-out consumers registered with the stream.
sdsConsumerCount :: Lens' StreamDescriptionSummary (Maybe Natural)
sdsConsumerCount = lens _sdsConsumerCount (\s a -> s {_sdsConsumerCount = a}) . mapping _Nat

-- | The name of the stream being described.
sdsStreamName :: Lens' StreamDescriptionSummary Text
sdsStreamName = lens _sdsStreamName (\s a -> s {_sdsStreamName = a})

-- | The Amazon Resource Name (ARN) for the stream being described.
sdsStreamARN :: Lens' StreamDescriptionSummary Text
sdsStreamARN = lens _sdsStreamARN (\s a -> s {_sdsStreamARN = a})

-- | The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
sdsStreamStatus :: Lens' StreamDescriptionSummary StreamStatus
sdsStreamStatus = lens _sdsStreamStatus (\s a -> s {_sdsStreamStatus = a})

-- | The current retention period, in hours.
sdsRetentionPeriodHours :: Lens' StreamDescriptionSummary Int
sdsRetentionPeriodHours = lens _sdsRetentionPeriodHours (\s a -> s {_sdsRetentionPeriodHours = a})

-- | The approximate time that the stream was created.
sdsStreamCreationTimestamp :: Lens' StreamDescriptionSummary UTCTime
sdsStreamCreationTimestamp = lens _sdsStreamCreationTimestamp (\s a -> s {_sdsStreamCreationTimestamp = a}) . _Time

-- | Represents the current enhanced monitoring settings of the stream.
sdsEnhancedMonitoring :: Lens' StreamDescriptionSummary [EnhancedMetrics]
sdsEnhancedMonitoring = lens _sdsEnhancedMonitoring (\s a -> s {_sdsEnhancedMonitoring = a}) . _Coerce

-- | The number of open shards in the stream.
sdsOpenShardCount :: Lens' StreamDescriptionSummary Natural
sdsOpenShardCount = lens _sdsOpenShardCount (\s a -> s {_sdsOpenShardCount = a}) . _Nat

instance FromJSON StreamDescriptionSummary where
  parseJSON =
    withObject
      "StreamDescriptionSummary"
      ( \x ->
          StreamDescriptionSummary'
            <$> (x .:? "EncryptionType")
            <*> (x .:? "KeyId")
            <*> (x .:? "ConsumerCount")
            <*> (x .: "StreamName")
            <*> (x .: "StreamARN")
            <*> (x .: "StreamStatus")
            <*> (x .: "RetentionPeriodHours")
            <*> (x .: "StreamCreationTimestamp")
            <*> (x .:? "EnhancedMonitoring" .!= mempty)
            <*> (x .: "OpenShardCount")
      )

instance Hashable StreamDescriptionSummary

instance NFData StreamDescriptionSummary
