{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StreamDescription where

import Network.AWS.Kinesis.Types.EncryptionType
import Network.AWS.Kinesis.Types.EnhancedMetrics
import Network.AWS.Kinesis.Types.Shard
import Network.AWS.Kinesis.Types.StreamStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output for 'DescribeStream' .
--
--
--
-- /See:/ 'streamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { _sdEncryptionType ::
      !(Maybe EncryptionType),
    _sdKeyId :: !(Maybe Text),
    _sdStreamName :: !Text,
    _sdStreamARN :: !Text,
    _sdStreamStatus :: !StreamStatus,
    _sdShards :: ![Shard],
    _sdHasMoreShards :: !Bool,
    _sdRetentionPeriodHours :: !Int,
    _sdStreamCreationTimestamp :: !POSIX,
    _sdEnhancedMonitoring :: ![EnhancedMetrics]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdEncryptionType' - The server-side encryption type used on the stream. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
--
-- * 'sdKeyId' - The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
--
-- * 'sdStreamName' - The name of the stream being described.
--
-- * 'sdStreamARN' - The Amazon Resource Name (ARN) for the stream being described.
--
-- * 'sdStreamStatus' - The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
--
-- * 'sdShards' - The shards that comprise the stream.
--
-- * 'sdHasMoreShards' - If set to @true@ , more shards in the stream are available to describe.
--
-- * 'sdRetentionPeriodHours' - The current retention period, in hours. Minimum value of 24. Maximum value of 168.
--
-- * 'sdStreamCreationTimestamp' - The approximate time that the stream was created.
--
-- * 'sdEnhancedMonitoring' - Represents the current enhanced monitoring settings of the stream.
streamDescription ::
  -- | 'sdStreamName'
  Text ->
  -- | 'sdStreamARN'
  Text ->
  -- | 'sdStreamStatus'
  StreamStatus ->
  -- | 'sdHasMoreShards'
  Bool ->
  -- | 'sdRetentionPeriodHours'
  Int ->
  -- | 'sdStreamCreationTimestamp'
  UTCTime ->
  StreamDescription
streamDescription
  pStreamName_
  pStreamARN_
  pStreamStatus_
  pHasMoreShards_
  pRetentionPeriodHours_
  pStreamCreationTimestamp_ =
    StreamDescription'
      { _sdEncryptionType = Nothing,
        _sdKeyId = Nothing,
        _sdStreamName = pStreamName_,
        _sdStreamARN = pStreamARN_,
        _sdStreamStatus = pStreamStatus_,
        _sdShards = mempty,
        _sdHasMoreShards = pHasMoreShards_,
        _sdRetentionPeriodHours = pRetentionPeriodHours_,
        _sdStreamCreationTimestamp = _Time # pStreamCreationTimestamp_,
        _sdEnhancedMonitoring = mempty
      }

-- | The server-side encryption type used on the stream. This parameter can be one of the following values:     * @NONE@ : Do not encrypt the records in the stream.     * @KMS@ : Use server-side encryption on the records in the stream using a customer-managed AWS KMS key.
sdEncryptionType :: Lens' StreamDescription (Maybe EncryptionType)
sdEncryptionType = lens _sdEncryptionType (\s a -> s {_sdEncryptionType = a})

-- | The GUID for the customer-managed AWS KMS key to use for encryption. This value can be a globally unique identifier, a fully specified ARN to either an alias or a key, or an alias name prefixed by "alias/".You can also use a master key owned by Kinesis Data Streams by specifying the alias @aws/kinesis@ .     * Key ARN example: @arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012@      * Alias ARN example: @arn:aws:kms:us-east-1:123456789012:alias/MyAliasName@      * Globally unique key ID example: @12345678-1234-1234-1234-123456789012@      * Alias name example: @alias/MyAliasName@      * Master key owned by Kinesis Data Streams: @alias/aws/kinesis@
sdKeyId :: Lens' StreamDescription (Maybe Text)
sdKeyId = lens _sdKeyId (\s a -> s {_sdKeyId = a})

-- | The name of the stream being described.
sdStreamName :: Lens' StreamDescription Text
sdStreamName = lens _sdStreamName (\s a -> s {_sdStreamName = a})

-- | The Amazon Resource Name (ARN) for the stream being described.
sdStreamARN :: Lens' StreamDescription Text
sdStreamARN = lens _sdStreamARN (\s a -> s {_sdStreamARN = a})

-- | The current status of the stream being described. The stream status is one of the following states:     * @CREATING@ - The stream is being created. Kinesis Data Streams immediately returns and sets @StreamStatus@ to @CREATING@ .     * @DELETING@ - The stream is being deleted. The specified stream is in the @DELETING@ state until Kinesis Data Streams completes the deletion.     * @ACTIVE@ - The stream exists and is ready for read and write operations or deletion. You should perform read and write operations only on an @ACTIVE@ stream.     * @UPDATING@ - Shards in the stream are being merged or split. Read and write operations continue to work while the stream is in the @UPDATING@ state.
sdStreamStatus :: Lens' StreamDescription StreamStatus
sdStreamStatus = lens _sdStreamStatus (\s a -> s {_sdStreamStatus = a})

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\s a -> s {_sdShards = a}) . _Coerce

-- | If set to @true@ , more shards in the stream are available to describe.
sdHasMoreShards :: Lens' StreamDescription Bool
sdHasMoreShards = lens _sdHasMoreShards (\s a -> s {_sdHasMoreShards = a})

-- | The current retention period, in hours. Minimum value of 24. Maximum value of 168.
sdRetentionPeriodHours :: Lens' StreamDescription Int
sdRetentionPeriodHours = lens _sdRetentionPeriodHours (\s a -> s {_sdRetentionPeriodHours = a})

-- | The approximate time that the stream was created.
sdStreamCreationTimestamp :: Lens' StreamDescription UTCTime
sdStreamCreationTimestamp = lens _sdStreamCreationTimestamp (\s a -> s {_sdStreamCreationTimestamp = a}) . _Time

-- | Represents the current enhanced monitoring settings of the stream.
sdEnhancedMonitoring :: Lens' StreamDescription [EnhancedMetrics]
sdEnhancedMonitoring = lens _sdEnhancedMonitoring (\s a -> s {_sdEnhancedMonitoring = a}) . _Coerce

instance FromJSON StreamDescription where
  parseJSON =
    withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            <$> (x .:? "EncryptionType")
            <*> (x .:? "KeyId")
            <*> (x .: "StreamName")
            <*> (x .: "StreamARN")
            <*> (x .: "StreamStatus")
            <*> (x .:? "Shards" .!= mempty)
            <*> (x .: "HasMoreShards")
            <*> (x .: "RetentionPeriodHours")
            <*> (x .: "StreamCreationTimestamp")
            <*> (x .:? "EnhancedMonitoring" .!= mempty)
      )

instance Hashable StreamDescription

instance NFData StreamDescription
