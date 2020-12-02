{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamDescription where

import Network.AWS.DynamoDBStreams.Types.KeySchemaElement
import Network.AWS.DynamoDBStreams.Types.Shard
import Network.AWS.DynamoDBStreams.Types.StreamStatus
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents all of the data describing a particular stream.
--
--
--
-- /See:/ 'streamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { _sdLastEvaluatedShardId ::
      !(Maybe Text),
    _sdStreamLabel :: !(Maybe Text),
    _sdStreamStatus :: !(Maybe StreamStatus),
    _sdKeySchema :: !(Maybe (List1 KeySchemaElement)),
    _sdStreamViewType :: !(Maybe StreamViewType),
    _sdStreamARN :: !(Maybe Text),
    _sdShards :: !(Maybe [Shard]),
    _sdTableName :: !(Maybe Text),
    _sdCreationRequestDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdLastEvaluatedShardId' - The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request. If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved. If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
--
-- * 'sdStreamLabel' - A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
--
-- * 'sdStreamStatus' - Indicates the current status of the stream:     * @ENABLING@ - Streams is currently being enabled on the DynamoDB table.     * @ENABLED@ - the stream is enabled.     * @DISABLING@ - Streams is currently being disabled on the DynamoDB table.     * @DISABLED@ - the stream is disabled.
--
-- * 'sdKeySchema' - The key attribute(s) of the stream's DynamoDB table.
--
-- * 'sdStreamViewType' - Indicates the format of the records within this stream:     * @KEYS_ONLY@ - only the key attributes of items that were modified in the DynamoDB table.     * @NEW_IMAGE@ - entire items from the table, as they appeared after they were modified.     * @OLD_IMAGE@ - entire items from the table, as they appeared before they were modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items from the table.
--
-- * 'sdStreamARN' - The Amazon Resource Name (ARN) for the stream.
--
-- * 'sdShards' - The shards that comprise the stream.
--
-- * 'sdTableName' - The DynamoDB table with which the stream is associated.
--
-- * 'sdCreationRequestDateTime' - The date and time when the request to create this stream was issued.
streamDescription ::
  StreamDescription
streamDescription =
  StreamDescription'
    { _sdLastEvaluatedShardId = Nothing,
      _sdStreamLabel = Nothing,
      _sdStreamStatus = Nothing,
      _sdKeySchema = Nothing,
      _sdStreamViewType = Nothing,
      _sdStreamARN = Nothing,
      _sdShards = Nothing,
      _sdTableName = Nothing,
      _sdCreationRequestDateTime = Nothing
    }

-- | The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request. If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved. If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
sdLastEvaluatedShardId :: Lens' StreamDescription (Maybe Text)
sdLastEvaluatedShardId = lens _sdLastEvaluatedShardId (\s a -> s {_sdLastEvaluatedShardId = a})

-- | A timestamp, in ISO 8601 format, for this stream. Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:     * the AWS customer ID.     * the table name     * the @StreamLabel@
sdStreamLabel :: Lens' StreamDescription (Maybe Text)
sdStreamLabel = lens _sdStreamLabel (\s a -> s {_sdStreamLabel = a})

-- | Indicates the current status of the stream:     * @ENABLING@ - Streams is currently being enabled on the DynamoDB table.     * @ENABLED@ - the stream is enabled.     * @DISABLING@ - Streams is currently being disabled on the DynamoDB table.     * @DISABLED@ - the stream is disabled.
sdStreamStatus :: Lens' StreamDescription (Maybe StreamStatus)
sdStreamStatus = lens _sdStreamStatus (\s a -> s {_sdStreamStatus = a})

-- | The key attribute(s) of the stream's DynamoDB table.
sdKeySchema :: Lens' StreamDescription (Maybe (NonEmpty KeySchemaElement))
sdKeySchema = lens _sdKeySchema (\s a -> s {_sdKeySchema = a}) . mapping _List1

-- | Indicates the format of the records within this stream:     * @KEYS_ONLY@ - only the key attributes of items that were modified in the DynamoDB table.     * @NEW_IMAGE@ - entire items from the table, as they appeared after they were modified.     * @OLD_IMAGE@ - entire items from the table, as they appeared before they were modified.     * @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items from the table.
sdStreamViewType :: Lens' StreamDescription (Maybe StreamViewType)
sdStreamViewType = lens _sdStreamViewType (\s a -> s {_sdStreamViewType = a})

-- | The Amazon Resource Name (ARN) for the stream.
sdStreamARN :: Lens' StreamDescription (Maybe Text)
sdStreamARN = lens _sdStreamARN (\s a -> s {_sdStreamARN = a})

-- | The shards that comprise the stream.
sdShards :: Lens' StreamDescription [Shard]
sdShards = lens _sdShards (\s a -> s {_sdShards = a}) . _Default . _Coerce

-- | The DynamoDB table with which the stream is associated.
sdTableName :: Lens' StreamDescription (Maybe Text)
sdTableName = lens _sdTableName (\s a -> s {_sdTableName = a})

-- | The date and time when the request to create this stream was issued.
sdCreationRequestDateTime :: Lens' StreamDescription (Maybe UTCTime)
sdCreationRequestDateTime = lens _sdCreationRequestDateTime (\s a -> s {_sdCreationRequestDateTime = a}) . mapping _Time

instance FromJSON StreamDescription where
  parseJSON =
    withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            <$> (x .:? "LastEvaluatedShardId")
            <*> (x .:? "StreamLabel")
            <*> (x .:? "StreamStatus")
            <*> (x .:? "KeySchema")
            <*> (x .:? "StreamViewType")
            <*> (x .:? "StreamArn")
            <*> (x .:? "Shards" .!= mempty)
            <*> (x .:? "TableName")
            <*> (x .:? "CreationRequestDateTime")
      )

instance Hashable StreamDescription

instance NFData StreamDescription
