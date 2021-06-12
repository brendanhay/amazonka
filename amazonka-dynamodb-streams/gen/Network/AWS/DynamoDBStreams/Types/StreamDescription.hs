{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.StreamDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.StreamDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDBStreams.Types.KeySchemaElement
import Network.AWS.DynamoDBStreams.Types.Shard
import Network.AWS.DynamoDBStreams.Types.StreamStatus
import Network.AWS.DynamoDBStreams.Types.StreamViewType
import qualified Network.AWS.Lens as Lens

-- | Represents all of the data describing a particular stream.
--
-- /See:/ 'newStreamDescription' smart constructor.
data StreamDescription = StreamDescription'
  { -- | The shard ID of the item where the operation stopped, inclusive of the
    -- previous result set. Use this value to start a new operation, excluding
    -- this value in the new request.
    --
    -- If @LastEvaluatedShardId@ is empty, then the \"last page\" of results
    -- has been processed and there is currently no more data to be retrieved.
    --
    -- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean
    -- that there is more data in the result set. The only way to know when you
    -- have reached the end of the result set is when @LastEvaluatedShardId@ is
    -- empty.
    lastEvaluatedShardId :: Core.Maybe Core.Text,
    -- | Indicates the format of the records within this stream:
    --
    -- -   @KEYS_ONLY@ - only the key attributes of items that were modified in
    --     the DynamoDB table.
    --
    -- -   @NEW_IMAGE@ - entire items from the table, as they appeared after
    --     they were modified.
    --
    -- -   @OLD_IMAGE@ - entire items from the table, as they appeared before
    --     they were modified.
    --
    -- -   @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items
    --     from the table.
    streamViewType :: Core.Maybe StreamViewType,
    -- | The DynamoDB table with which the stream is associated.
    tableName :: Core.Maybe Core.Text,
    -- | The date and time when the request to create this stream was issued.
    creationRequestDateTime :: Core.Maybe Core.POSIX,
    -- | The key attribute(s) of the stream\'s DynamoDB table.
    keySchema :: Core.Maybe (Core.NonEmpty KeySchemaElement),
    -- | Indicates the current status of the stream:
    --
    -- -   @ENABLING@ - Streams is currently being enabled on the DynamoDB
    --     table.
    --
    -- -   @ENABLED@ - the stream is enabled.
    --
    -- -   @DISABLING@ - Streams is currently being disabled on the DynamoDB
    --     table.
    --
    -- -   @DISABLED@ - the stream is disabled.
    streamStatus :: Core.Maybe StreamStatus,
    -- | The shards that comprise the stream.
    shards :: Core.Maybe [Shard],
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Core.Maybe Core.Text,
    -- | A timestamp, in ISO 8601 format, for this stream.
    --
    -- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
    -- because it is possible that a stream from another table might have the
    -- same timestamp. However, the combination of the following three elements
    -- is guaranteed to be unique:
    --
    -- -   the AWS customer ID.
    --
    -- -   the table name
    --
    -- -   the @StreamLabel@
    streamLabel :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StreamDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastEvaluatedShardId', 'streamDescription_lastEvaluatedShardId' - The shard ID of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request.
--
-- If @LastEvaluatedShardId@ is empty, then the \"last page\" of results
-- has been processed and there is currently no more data to be retrieved.
--
-- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean
-- that there is more data in the result set. The only way to know when you
-- have reached the end of the result set is when @LastEvaluatedShardId@ is
-- empty.
--
-- 'streamViewType', 'streamDescription_streamViewType' - Indicates the format of the records within this stream:
--
-- -   @KEYS_ONLY@ - only the key attributes of items that were modified in
--     the DynamoDB table.
--
-- -   @NEW_IMAGE@ - entire items from the table, as they appeared after
--     they were modified.
--
-- -   @OLD_IMAGE@ - entire items from the table, as they appeared before
--     they were modified.
--
-- -   @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items
--     from the table.
--
-- 'tableName', 'streamDescription_tableName' - The DynamoDB table with which the stream is associated.
--
-- 'creationRequestDateTime', 'streamDescription_creationRequestDateTime' - The date and time when the request to create this stream was issued.
--
-- 'keySchema', 'streamDescription_keySchema' - The key attribute(s) of the stream\'s DynamoDB table.
--
-- 'streamStatus', 'streamDescription_streamStatus' - Indicates the current status of the stream:
--
-- -   @ENABLING@ - Streams is currently being enabled on the DynamoDB
--     table.
--
-- -   @ENABLED@ - the stream is enabled.
--
-- -   @DISABLING@ - Streams is currently being disabled on the DynamoDB
--     table.
--
-- -   @DISABLED@ - the stream is disabled.
--
-- 'shards', 'streamDescription_shards' - The shards that comprise the stream.
--
-- 'streamArn', 'streamDescription_streamArn' - The Amazon Resource Name (ARN) for the stream.
--
-- 'streamLabel', 'streamDescription_streamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the AWS customer ID.
--
-- -   the table name
--
-- -   the @StreamLabel@
newStreamDescription ::
  StreamDescription
newStreamDescription =
  StreamDescription'
    { lastEvaluatedShardId =
        Core.Nothing,
      streamViewType = Core.Nothing,
      tableName = Core.Nothing,
      creationRequestDateTime = Core.Nothing,
      keySchema = Core.Nothing,
      streamStatus = Core.Nothing,
      shards = Core.Nothing,
      streamArn = Core.Nothing,
      streamLabel = Core.Nothing
    }

-- | The shard ID of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request.
--
-- If @LastEvaluatedShardId@ is empty, then the \"last page\" of results
-- has been processed and there is currently no more data to be retrieved.
--
-- If @LastEvaluatedShardId@ is not empty, it does not necessarily mean
-- that there is more data in the result set. The only way to know when you
-- have reached the end of the result set is when @LastEvaluatedShardId@ is
-- empty.
streamDescription_lastEvaluatedShardId :: Lens.Lens' StreamDescription (Core.Maybe Core.Text)
streamDescription_lastEvaluatedShardId = Lens.lens (\StreamDescription' {lastEvaluatedShardId} -> lastEvaluatedShardId) (\s@StreamDescription' {} a -> s {lastEvaluatedShardId = a} :: StreamDescription)

-- | Indicates the format of the records within this stream:
--
-- -   @KEYS_ONLY@ - only the key attributes of items that were modified in
--     the DynamoDB table.
--
-- -   @NEW_IMAGE@ - entire items from the table, as they appeared after
--     they were modified.
--
-- -   @OLD_IMAGE@ - entire items from the table, as they appeared before
--     they were modified.
--
-- -   @NEW_AND_OLD_IMAGES@ - both the new and the old images of the items
--     from the table.
streamDescription_streamViewType :: Lens.Lens' StreamDescription (Core.Maybe StreamViewType)
streamDescription_streamViewType = Lens.lens (\StreamDescription' {streamViewType} -> streamViewType) (\s@StreamDescription' {} a -> s {streamViewType = a} :: StreamDescription)

-- | The DynamoDB table with which the stream is associated.
streamDescription_tableName :: Lens.Lens' StreamDescription (Core.Maybe Core.Text)
streamDescription_tableName = Lens.lens (\StreamDescription' {tableName} -> tableName) (\s@StreamDescription' {} a -> s {tableName = a} :: StreamDescription)

-- | The date and time when the request to create this stream was issued.
streamDescription_creationRequestDateTime :: Lens.Lens' StreamDescription (Core.Maybe Core.UTCTime)
streamDescription_creationRequestDateTime = Lens.lens (\StreamDescription' {creationRequestDateTime} -> creationRequestDateTime) (\s@StreamDescription' {} a -> s {creationRequestDateTime = a} :: StreamDescription) Core.. Lens.mapping Core._Time

-- | The key attribute(s) of the stream\'s DynamoDB table.
streamDescription_keySchema :: Lens.Lens' StreamDescription (Core.Maybe (Core.NonEmpty KeySchemaElement))
streamDescription_keySchema = Lens.lens (\StreamDescription' {keySchema} -> keySchema) (\s@StreamDescription' {} a -> s {keySchema = a} :: StreamDescription) Core.. Lens.mapping Lens._Coerce

-- | Indicates the current status of the stream:
--
-- -   @ENABLING@ - Streams is currently being enabled on the DynamoDB
--     table.
--
-- -   @ENABLED@ - the stream is enabled.
--
-- -   @DISABLING@ - Streams is currently being disabled on the DynamoDB
--     table.
--
-- -   @DISABLED@ - the stream is disabled.
streamDescription_streamStatus :: Lens.Lens' StreamDescription (Core.Maybe StreamStatus)
streamDescription_streamStatus = Lens.lens (\StreamDescription' {streamStatus} -> streamStatus) (\s@StreamDescription' {} a -> s {streamStatus = a} :: StreamDescription)

-- | The shards that comprise the stream.
streamDescription_shards :: Lens.Lens' StreamDescription (Core.Maybe [Shard])
streamDescription_shards = Lens.lens (\StreamDescription' {shards} -> shards) (\s@StreamDescription' {} a -> s {shards = a} :: StreamDescription) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) for the stream.
streamDescription_streamArn :: Lens.Lens' StreamDescription (Core.Maybe Core.Text)
streamDescription_streamArn = Lens.lens (\StreamDescription' {streamArn} -> streamArn) (\s@StreamDescription' {} a -> s {streamArn = a} :: StreamDescription)

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   the AWS customer ID.
--
-- -   the table name
--
-- -   the @StreamLabel@
streamDescription_streamLabel :: Lens.Lens' StreamDescription (Core.Maybe Core.Text)
streamDescription_streamLabel = Lens.lens (\StreamDescription' {streamLabel} -> streamLabel) (\s@StreamDescription' {} a -> s {streamLabel = a} :: StreamDescription)

instance Core.FromJSON StreamDescription where
  parseJSON =
    Core.withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            Core.<$> (x Core..:? "LastEvaluatedShardId")
            Core.<*> (x Core..:? "StreamViewType")
            Core.<*> (x Core..:? "TableName")
            Core.<*> (x Core..:? "CreationRequestDateTime")
            Core.<*> (x Core..:? "KeySchema")
            Core.<*> (x Core..:? "StreamStatus")
            Core.<*> (x Core..:? "Shards" Core..!= Core.mempty)
            Core.<*> (x Core..:? "StreamArn")
            Core.<*> (x Core..:? "StreamLabel")
      )

instance Core.Hashable StreamDescription

instance Core.NFData StreamDescription
