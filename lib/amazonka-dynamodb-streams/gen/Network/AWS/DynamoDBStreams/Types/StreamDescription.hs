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
    sdCreationRequestDateTime,
    sdKeySchema,
    sdLastEvaluatedShardId,
    sdShards,
    sdStreamArn,
    sdStreamLabel,
    sdStreamStatus,
    sdStreamViewType,
    sdTableName,
  )
where

import qualified Network.AWS.DynamoDBStreams.Types.KeySchemaElement as Types
import qualified Network.AWS.DynamoDBStreams.Types.LastEvaluatedShardId as Types
import qualified Network.AWS.DynamoDBStreams.Types.Shard as Types
import qualified Network.AWS.DynamoDBStreams.Types.StreamArn as Types
import qualified Network.AWS.DynamoDBStreams.Types.StreamLabel as Types
import qualified Network.AWS.DynamoDBStreams.Types.StreamStatus as Types
import qualified Network.AWS.DynamoDBStreams.Types.StreamViewType as Types
import qualified Network.AWS.DynamoDBStreams.Types.TableName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'mkStreamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { -- | The date and time when the request to create this stream was issued.
    creationRequestDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | The key attribute(s) of the stream's DynamoDB table.
    keySchema :: Core.Maybe (Core.NonEmpty Types.KeySchemaElement),
    -- | The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
    --
    -- If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved.
    -- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
    lastEvaluatedShardId :: Core.Maybe Types.LastEvaluatedShardId,
    -- | The shards that comprise the stream.
    shards :: Core.Maybe [Types.Shard],
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Core.Maybe Types.StreamArn,
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
    streamLabel :: Core.Maybe Types.StreamLabel,
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
    streamStatus :: Core.Maybe Types.StreamStatus,
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
    streamViewType :: Core.Maybe Types.StreamViewType,
    -- | The DynamoDB table with which the stream is associated.
    tableName :: Core.Maybe Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StreamDescription' value with any optional fields omitted.
mkStreamDescription ::
  StreamDescription
mkStreamDescription =
  StreamDescription'
    { creationRequestDateTime = Core.Nothing,
      keySchema = Core.Nothing,
      lastEvaluatedShardId = Core.Nothing,
      shards = Core.Nothing,
      streamArn = Core.Nothing,
      streamLabel = Core.Nothing,
      streamStatus = Core.Nothing,
      streamViewType = Core.Nothing,
      tableName = Core.Nothing
    }

-- | The date and time when the request to create this stream was issued.
--
-- /Note:/ Consider using 'creationRequestDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdCreationRequestDateTime :: Lens.Lens' StreamDescription (Core.Maybe Core.NominalDiffTime)
sdCreationRequestDateTime = Lens.field @"creationRequestDateTime"
{-# DEPRECATED sdCreationRequestDateTime "Use generic-lens or generic-optics with 'creationRequestDateTime' instead." #-}

-- | The key attribute(s) of the stream's DynamoDB table.
--
-- /Note:/ Consider using 'keySchema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdKeySchema :: Lens.Lens' StreamDescription (Core.Maybe (Core.NonEmpty Types.KeySchemaElement))
sdKeySchema = Lens.field @"keySchema"
{-# DEPRECATED sdKeySchema "Use generic-lens or generic-optics with 'keySchema' instead." #-}

-- | The shard ID of the item where the operation stopped, inclusive of the previous result set. Use this value to start a new operation, excluding this value in the new request.
--
-- If @LastEvaluatedShardId@ is empty, then the "last page" of results has been processed and there is currently no more data to be retrieved.
-- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean that there is more data in the result set. The only way to know when you have reached the end of the result set is when @LastEvaluatedShardId@ is empty.
--
-- /Note:/ Consider using 'lastEvaluatedShardId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdLastEvaluatedShardId :: Lens.Lens' StreamDescription (Core.Maybe Types.LastEvaluatedShardId)
sdLastEvaluatedShardId = Lens.field @"lastEvaluatedShardId"
{-# DEPRECATED sdLastEvaluatedShardId "Use generic-lens or generic-optics with 'lastEvaluatedShardId' instead." #-}

-- | The shards that comprise the stream.
--
-- /Note:/ Consider using 'shards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdShards :: Lens.Lens' StreamDescription (Core.Maybe [Types.Shard])
sdShards = Lens.field @"shards"
{-# DEPRECATED sdShards "Use generic-lens or generic-optics with 'shards' instead." #-}

-- | The Amazon Resource Name (ARN) for the stream.
--
-- /Note:/ Consider using 'streamArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdStreamArn :: Lens.Lens' StreamDescription (Core.Maybe Types.StreamArn)
sdStreamArn = Lens.field @"streamArn"
{-# DEPRECATED sdStreamArn "Use generic-lens or generic-optics with 'streamArn' instead." #-}

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
sdStreamLabel :: Lens.Lens' StreamDescription (Core.Maybe Types.StreamLabel)
sdStreamLabel = Lens.field @"streamLabel"
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
sdStreamStatus :: Lens.Lens' StreamDescription (Core.Maybe Types.StreamStatus)
sdStreamStatus = Lens.field @"streamStatus"
{-# DEPRECATED sdStreamStatus "Use generic-lens or generic-optics with 'streamStatus' instead." #-}

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
sdStreamViewType :: Lens.Lens' StreamDescription (Core.Maybe Types.StreamViewType)
sdStreamViewType = Lens.field @"streamViewType"
{-# DEPRECATED sdStreamViewType "Use generic-lens or generic-optics with 'streamViewType' instead." #-}

-- | The DynamoDB table with which the stream is associated.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdTableName :: Lens.Lens' StreamDescription (Core.Maybe Types.TableName)
sdTableName = Lens.field @"tableName"
{-# DEPRECATED sdTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON StreamDescription where
  parseJSON =
    Core.withObject "StreamDescription" Core.$
      \x ->
        StreamDescription'
          Core.<$> (x Core..:? "CreationRequestDateTime")
          Core.<*> (x Core..:? "KeySchema")
          Core.<*> (x Core..:? "LastEvaluatedShardId")
          Core.<*> (x Core..:? "Shards")
          Core.<*> (x Core..:? "StreamArn")
          Core.<*> (x Core..:? "StreamLabel")
          Core.<*> (x Core..:? "StreamStatus")
          Core.<*> (x Core..:? "StreamViewType")
          Core.<*> (x Core..:? "TableName")
