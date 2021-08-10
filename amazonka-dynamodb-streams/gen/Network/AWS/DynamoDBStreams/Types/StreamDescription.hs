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
import qualified Network.AWS.Prelude as Prelude

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
    lastEvaluatedShardId :: Prelude.Maybe Prelude.Text,
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
    streamViewType :: Prelude.Maybe StreamViewType,
    -- | The DynamoDB table with which the stream is associated.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the request to create this stream was issued.
    creationRequestDateTime :: Prelude.Maybe Core.POSIX,
    -- | The key attribute(s) of the stream\'s DynamoDB table.
    keySchema :: Prelude.Maybe (Prelude.NonEmpty KeySchemaElement),
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
    streamStatus :: Prelude.Maybe StreamStatus,
    -- | The shards that comprise the stream.
    shards :: Prelude.Maybe [Shard],
    -- | The Amazon Resource Name (ARN) for the stream.
    streamArn :: Prelude.Maybe Prelude.Text,
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
    streamLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      streamViewType = Prelude.Nothing,
      tableName = Prelude.Nothing,
      creationRequestDateTime = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      streamStatus = Prelude.Nothing,
      shards = Prelude.Nothing,
      streamArn = Prelude.Nothing,
      streamLabel = Prelude.Nothing
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
streamDescription_lastEvaluatedShardId :: Lens.Lens' StreamDescription (Prelude.Maybe Prelude.Text)
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
streamDescription_streamViewType :: Lens.Lens' StreamDescription (Prelude.Maybe StreamViewType)
streamDescription_streamViewType = Lens.lens (\StreamDescription' {streamViewType} -> streamViewType) (\s@StreamDescription' {} a -> s {streamViewType = a} :: StreamDescription)

-- | The DynamoDB table with which the stream is associated.
streamDescription_tableName :: Lens.Lens' StreamDescription (Prelude.Maybe Prelude.Text)
streamDescription_tableName = Lens.lens (\StreamDescription' {tableName} -> tableName) (\s@StreamDescription' {} a -> s {tableName = a} :: StreamDescription)

-- | The date and time when the request to create this stream was issued.
streamDescription_creationRequestDateTime :: Lens.Lens' StreamDescription (Prelude.Maybe Prelude.UTCTime)
streamDescription_creationRequestDateTime = Lens.lens (\StreamDescription' {creationRequestDateTime} -> creationRequestDateTime) (\s@StreamDescription' {} a -> s {creationRequestDateTime = a} :: StreamDescription) Prelude.. Lens.mapping Core._Time

-- | The key attribute(s) of the stream\'s DynamoDB table.
streamDescription_keySchema :: Lens.Lens' StreamDescription (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
streamDescription_keySchema = Lens.lens (\StreamDescription' {keySchema} -> keySchema) (\s@StreamDescription' {} a -> s {keySchema = a} :: StreamDescription) Prelude.. Lens.mapping Lens._Coerce

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
streamDescription_streamStatus :: Lens.Lens' StreamDescription (Prelude.Maybe StreamStatus)
streamDescription_streamStatus = Lens.lens (\StreamDescription' {streamStatus} -> streamStatus) (\s@StreamDescription' {} a -> s {streamStatus = a} :: StreamDescription)

-- | The shards that comprise the stream.
streamDescription_shards :: Lens.Lens' StreamDescription (Prelude.Maybe [Shard])
streamDescription_shards = Lens.lens (\StreamDescription' {shards} -> shards) (\s@StreamDescription' {} a -> s {shards = a} :: StreamDescription) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) for the stream.
streamDescription_streamArn :: Lens.Lens' StreamDescription (Prelude.Maybe Prelude.Text)
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
streamDescription_streamLabel :: Lens.Lens' StreamDescription (Prelude.Maybe Prelude.Text)
streamDescription_streamLabel = Lens.lens (\StreamDescription' {streamLabel} -> streamLabel) (\s@StreamDescription' {} a -> s {streamLabel = a} :: StreamDescription)

instance Core.FromJSON StreamDescription where
  parseJSON =
    Core.withObject
      "StreamDescription"
      ( \x ->
          StreamDescription'
            Prelude.<$> (x Core..:? "LastEvaluatedShardId")
            Prelude.<*> (x Core..:? "StreamViewType")
            Prelude.<*> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "CreationRequestDateTime")
            Prelude.<*> (x Core..:? "KeySchema")
            Prelude.<*> (x Core..:? "StreamStatus")
            Prelude.<*> (x Core..:? "Shards" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "StreamArn")
            Prelude.<*> (x Core..:? "StreamLabel")
      )

instance Prelude.Hashable StreamDescription

instance Prelude.NFData StreamDescription
