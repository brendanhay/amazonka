{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamDescription
  ( StreamDescription (..),

    -- * Smart constructor
    mkStreamDescription,

    -- * Lenses
    sdLastEvaluatedShardId,
    sdStreamLabel,
    sdStreamStatus,
    sdKeySchema,
    sdStreamViewType,
    sdStreamARN,
    sdShards,
    sdTableName,
    sdCreationRequestDateTime,
  )
where

import Network.AWS.DynamoDBStreams.Types.KeySchemaElement
import Network.AWS.DynamoDBStreams.Types.Shard
import Network.AWS.DynamoDBStreams.Types.StreamStatus
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'mkStreamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { lastEvaluatedShardId ::
      Lude.Maybe Lude.Text,
    streamLabel :: Lude.Maybe Lude.Text,
    streamStatus :: Lude.Maybe StreamStatus,
    keySchema ::
      Lude.Maybe (Lude.NonEmpty KeySchemaElement),
    streamViewType :: Lude.Maybe StreamViewType,
    streamARN :: Lude.Maybe Lude.Text,
    shards :: Lude.Maybe [Shard],
    tableName :: Lude.Maybe Lude.Text,
    creationRequestDateTime :: Lude.Maybe Lude.Timestamp
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
-- * 'creationRequestDateTime' - The date and time when the request to create this stream was issued.
-- * 'keySchema' - The key attribute(s) of the stream's DynamoDB table.
-- * 'lastEvaluatedShardId' - The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved.
-- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
-- * 'shards' - The shards that comprise the stream.
-- * 'streamARN' - The Amazon Resource Name (ARN) for the stream.
-- * 'streamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:
--
--     * the AWS customer ID.
--
--
--     * the table name
--
--
--     * the @StreamLabel@
--
--
-- * 'streamStatus' - Indicates the current status of the stream:
--
--
--     * @ENABLING@ - Streams is currently being enabled on the DynamoDB table.
--
--
--     * @ENABLED@ - the stream is enabled.
--
--
--     * @DISABLING@ - Streams is currently being disabled on the DynamoDB table.
--
--
--     * @DISABLED@ - the stream is disabled.
--
--
-- * 'streamViewType' - Indicates the format of the records within this stream:
--
--
--     * @KEYS_ONLY@ - only the key attributes of items that were modified in the DynamoDB table.
--
--
--     * @NEW_IMAGE@ - entire items from the table, as they appeared after they were modified.
--
--
--     * @OLD_IMAGE@ - entire items from the table, as they appeared before they were modified.
--
--
--     * @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items from the table.
--
--
-- * 'tableName' - The DynamoDB table with which the stream is associated.
mkStreamDescription ::
  StreamDescription
mkStreamDescription =
  StreamDescription'
    { lastEvaluatedShardId = Lude.Nothing,
      streamLabel = Lude.Nothing,
      streamStatus = Lude.Nothing,
      keySchema = Lude.Nothing,
      streamViewType = Lude.Nothing,
      streamARN = Lude.Nothing,
      shards = Lude.Nothing,
      tableName = Lude.Nothing,
      creationRequestDateTime = Lude.Nothing
    }

-- | The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved.
-- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLastEvaluatedShardId :: Lens.Lens' StreamDescription (Lude.Maybe Lude.Text)
sdLastEvaluatedShardId = Lens.lens (lastEvaluatedShardId :: StreamDescription -> Lude.Maybe Lude.Text) (\s a -> s {lastEvaluatedShardId = a} :: StreamDescription)
{-# DEPRECATED sdLastEvaluatedShardId "Use generic-lens or generic-optics with 'lastEvaluatedShardId' instead." #-}

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream, because it is possible that a stream from another table might have the same timestamp. However, the combination of the following three elements is guaranteed to be unique:
--
--     * the AWS customer ID.
--
--
--     * the table name
--
--
--     * the @StreamLabel@
--
--
--
-- /Note:/ Consider using 'streamLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamLabel :: Lens.Lens' StreamDescription (Lude.Maybe Lude.Text)
sdStreamLabel = Lens.lens (streamLabel :: StreamDescription -> Lude.Maybe Lude.Text) (\s a -> s {streamLabel = a} :: StreamDescription)
{-# DEPRECATED sdStreamLabel "Use generic-lens or generic-optics with 'streamLabel' instead." #-}

-- | Indicates the current status of the stream:
--
--
--     * @ENABLING@ - Streams is currently being enabled on the DynamoDB table.
--
--
--     * @ENABLED@ - the stream is enabled.
--
--
--     * @DISABLING@ - Streams is currently being disabled on the DynamoDB table.
--
--
--     * @DISABLED@ - the stream is disabled.
--
--
--
-- /Note:/ Consider using 'streamStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamStatus :: Lens.Lens' StreamDescription (Lude.Maybe StreamStatus)
sdStreamStatus = Lens.lens (streamStatus :: StreamDescription -> Lude.Maybe StreamStatus) (\s a -> s {streamStatus = a} :: StreamDescription)
{-# DEPRECATED sdStreamStatus "Use generic-lens or generic-optics with 'streamStatus' instead." #-}

-- | The key attribute(s) of the stream's DynamoDB table.
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdKeySchema :: Lens.Lens' StreamDescription (Lude.Maybe (Lude.NonEmpty KeySchemaElement))
sdKeySchema = Lens.lens (keySchema :: StreamDescription -> Lude.Maybe (Lude.NonEmpty KeySchemaElement)) (\s a -> s {keySchema = a} :: StreamDescription)
{-# DEPRECATED sdKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | Indicates the format of the records within this stream:
--
--
--     * @KEYS_ONLY@ - only the key attributes of items that were modified in the DynamoDB table.
--
--
--     * @NEW_IMAGE@ - entire items from the table, as they appeared after they were modified.
--
--
--     * @OLD_IMAGE@ - entire items from the table, as they appeared before they were modified.
--
--
--     * @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items from the table.
--
--
--
-- /Note:/ Consider using 'streamViewType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamViewType :: Lens.Lens' StreamDescription (Lude.Maybe StreamViewType)
sdStreamViewType = Lens.lens (streamViewType :: StreamDescription -> Lude.Maybe StreamViewType) (\s a -> s {streamViewType = a} :: StreamDescription)
{-# DEPRECATED sdStreamViewType "Use generic-lens or generic-optics with 'streamViewType' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamARN :: Lens.Lens' StreamDescription (Lude.Maybe Lude.Text)
sdStreamARN = Lens.lens (streamARN :: StreamDescription -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: StreamDescription)
{-# DEPRECATED sdStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The shards that comprise the stream.
--
-- /Note:/ Consider using 'shards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShards :: Lens.Lens' StreamDescription (Lude.Maybe [Shard])
sdShards = Lens.lens (shards :: StreamDescription -> Lude.Maybe [Shard]) (\s a -> s {shards = a} :: StreamDescription)
{-# DEPRECATED sdShards "Use generic-lens or generic-optics with 'shards' instead." #-}

-- | The DynamoDB table with which the stream is associated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTableName :: Lens.Lens' StreamDescription (Lude.Maybe Lude.Text)
sdTableName = Lens.lens (tableName :: StreamDescription -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: StreamDescription)
{-# DEPRECATED sdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The date and time when the request to create this stream was issued.
--
-- /Note:/ Consider using 'creationRequestDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdCreationRequestDateTime :: Lens.Lens' StreamDescription (Lude.Maybe Lude.Timestamp)
sdCreationRequestDateTime = Lens.lens (creationRequestDateTime :: StreamDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationRequestDateTime = a} :: StreamDescription)
{-# DEPRECATED sdCreationRequestDateTime "Use generic-lens or generic-optics with 'creationRequestDateTime' instead." #-}

instance Lude.FromJSON StreamDescription where
  parseJSON =
    Lude.withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            Lude.<$> (x Lude..:? "LastEvaluatedShardId")
            Lude.<*> (x Lude..:? "StreamLabel")
            Lude.<*> (x Lude..:? "StreamStatus")
            Lude.<*> (x Lude..:? "KeySchema")
            Lude.<*> (x Lude..:? "StreamViewType")
            Lude.<*> (x Lude..:? "StreamArn")
            Lude.<*> (x Lude..:? "Shards" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "TableName")
            Lude.<*> (x Lude..:? "CreationRequestDateTime")
      )
