{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.ListShards
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shards in a stream and provides information about each shard.
-- This operation has a limit of 100 transactions per second per data
-- stream.
--
-- This API is a new operation that is used by the Amazon Kinesis Client
-- Library (KCL). If you have a fine-grained IAM policy that only allows
-- specific operations, you must update your policy to allow calls to this
-- API. For more information, see
-- <https://docs.aws.amazon.com/streams/latest/dev/controlling-access.html Controlling Access to Amazon Kinesis Data Streams Resources Using IAM>.
--
-- This operation returns paginated results.
module Network.AWS.Kinesis.ListShards
  ( -- * Creating a Request
    ListShards (..),
    newListShards,

    -- * Request Lenses
    listShards_exclusiveStartShardId,
    listShards_nextToken,
    listShards_shardFilter,
    listShards_maxResults,
    listShards_streamCreationTimestamp,
    listShards_streamName,

    -- * Destructuring the Response
    ListShardsResponse (..),
    newListShardsResponse,

    -- * Response Lenses
    listShardsResponse_nextToken,
    listShardsResponse_shards,
    listShardsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListShards' smart constructor.
data ListShards = ListShards'
  { -- | Specify this parameter to indicate that you want to list the shards
    -- starting with the shard whose ID immediately follows
    -- @ExclusiveStartShardId@.
    --
    -- If you don\'t specify this parameter, the default behavior is for
    -- @ListShards@ to list the shards starting with the first one in the
    -- stream.
    --
    -- You cannot specify this parameter if you specify @NextToken@.
    exclusiveStartShardId :: Prelude.Maybe Prelude.Text,
    -- | When the number of shards in the data stream is greater than the default
    -- value for the @MaxResults@ parameter, or if you explicitly specify a
    -- value for @MaxResults@ that is less than the number of shards in the
    -- data stream, the response includes a pagination token named @NextToken@.
    -- You can specify this @NextToken@ value in a subsequent call to
    -- @ListShards@ to list the next set of shards.
    --
    -- Don\'t specify @StreamName@ or @StreamCreationTimestamp@ if you specify
    -- @NextToken@ because the latter unambiguously identifies the stream.
    --
    -- You can optionally specify a value for the @MaxResults@ parameter when
    -- you specify @NextToken@. If you specify a @MaxResults@ value that is
    -- less than the number of shards that the operation returns if you don\'t
    -- specify @MaxResults@, the response will contain a new @NextToken@ value.
    -- You can use the new @NextToken@ value in a subsequent call to the
    -- @ListShards@ operation.
    --
    -- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
    -- in the response to a call to @ListShards@, you have 300 seconds to use
    -- that value. If you specify an expired token in a call to @ListShards@,
    -- you get @ExpiredNextTokenException@.
    nextToken :: Prelude.Maybe Prelude.Text,
    shardFilter :: Prelude.Maybe ShardFilter,
    -- | The maximum number of shards to return in a single call to @ListShards@.
    -- The minimum value you can specify for this parameter is 1, and the
    -- maximum is 10,000, which is also the default.
    --
    -- When the number of shards to be listed is greater than the value of
    -- @MaxResults@, the response contains a @NextToken@ value that you can use
    -- in a subsequent call to @ListShards@ to list the next set of shards.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify this input parameter to distinguish data streams that have the
    -- same name. For example, if you create a data stream and then delete it,
    -- and you later create another data stream with the same name, you can use
    -- this input parameter to specify which of the two streams you want to
    -- list the shards for.
    --
    -- You cannot specify this parameter if you specify the @NextToken@
    -- parameter.
    streamCreationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the data stream whose shards you want to list.
    --
    -- You cannot specify this parameter if you specify the @NextToken@
    -- parameter.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListShards' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveStartShardId', 'listShards_exclusiveStartShardId' - Specify this parameter to indicate that you want to list the shards
-- starting with the shard whose ID immediately follows
-- @ExclusiveStartShardId@.
--
-- If you don\'t specify this parameter, the default behavior is for
-- @ListShards@ to list the shards starting with the first one in the
-- stream.
--
-- You cannot specify this parameter if you specify @NextToken@.
--
-- 'nextToken', 'listShards_nextToken' - When the number of shards in the data stream is greater than the default
-- value for the @MaxResults@ parameter, or if you explicitly specify a
-- value for @MaxResults@ that is less than the number of shards in the
-- data stream, the response includes a pagination token named @NextToken@.
-- You can specify this @NextToken@ value in a subsequent call to
-- @ListShards@ to list the next set of shards.
--
-- Don\'t specify @StreamName@ or @StreamCreationTimestamp@ if you specify
-- @NextToken@ because the latter unambiguously identifies the stream.
--
-- You can optionally specify a value for the @MaxResults@ parameter when
-- you specify @NextToken@. If you specify a @MaxResults@ value that is
-- less than the number of shards that the operation returns if you don\'t
-- specify @MaxResults@, the response will contain a new @NextToken@ value.
-- You can use the new @NextToken@ value in a subsequent call to the
-- @ListShards@ operation.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListShards@, you have 300 seconds to use
-- that value. If you specify an expired token in a call to @ListShards@,
-- you get @ExpiredNextTokenException@.
--
-- 'shardFilter', 'listShards_shardFilter' - Undocumented member.
--
-- 'maxResults', 'listShards_maxResults' - The maximum number of shards to return in a single call to @ListShards@.
-- The minimum value you can specify for this parameter is 1, and the
-- maximum is 10,000, which is also the default.
--
-- When the number of shards to be listed is greater than the value of
-- @MaxResults@, the response contains a @NextToken@ value that you can use
-- in a subsequent call to @ListShards@ to list the next set of shards.
--
-- 'streamCreationTimestamp', 'listShards_streamCreationTimestamp' - Specify this input parameter to distinguish data streams that have the
-- same name. For example, if you create a data stream and then delete it,
-- and you later create another data stream with the same name, you can use
-- this input parameter to specify which of the two streams you want to
-- list the shards for.
--
-- You cannot specify this parameter if you specify the @NextToken@
-- parameter.
--
-- 'streamName', 'listShards_streamName' - The name of the data stream whose shards you want to list.
--
-- You cannot specify this parameter if you specify the @NextToken@
-- parameter.
newListShards ::
  ListShards
newListShards =
  ListShards'
    { exclusiveStartShardId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      shardFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      streamCreationTimestamp = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Specify this parameter to indicate that you want to list the shards
-- starting with the shard whose ID immediately follows
-- @ExclusiveStartShardId@.
--
-- If you don\'t specify this parameter, the default behavior is for
-- @ListShards@ to list the shards starting with the first one in the
-- stream.
--
-- You cannot specify this parameter if you specify @NextToken@.
listShards_exclusiveStartShardId :: Lens.Lens' ListShards (Prelude.Maybe Prelude.Text)
listShards_exclusiveStartShardId = Lens.lens (\ListShards' {exclusiveStartShardId} -> exclusiveStartShardId) (\s@ListShards' {} a -> s {exclusiveStartShardId = a} :: ListShards)

-- | When the number of shards in the data stream is greater than the default
-- value for the @MaxResults@ parameter, or if you explicitly specify a
-- value for @MaxResults@ that is less than the number of shards in the
-- data stream, the response includes a pagination token named @NextToken@.
-- You can specify this @NextToken@ value in a subsequent call to
-- @ListShards@ to list the next set of shards.
--
-- Don\'t specify @StreamName@ or @StreamCreationTimestamp@ if you specify
-- @NextToken@ because the latter unambiguously identifies the stream.
--
-- You can optionally specify a value for the @MaxResults@ parameter when
-- you specify @NextToken@. If you specify a @MaxResults@ value that is
-- less than the number of shards that the operation returns if you don\'t
-- specify @MaxResults@, the response will contain a new @NextToken@ value.
-- You can use the new @NextToken@ value in a subsequent call to the
-- @ListShards@ operation.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListShards@, you have 300 seconds to use
-- that value. If you specify an expired token in a call to @ListShards@,
-- you get @ExpiredNextTokenException@.
listShards_nextToken :: Lens.Lens' ListShards (Prelude.Maybe Prelude.Text)
listShards_nextToken = Lens.lens (\ListShards' {nextToken} -> nextToken) (\s@ListShards' {} a -> s {nextToken = a} :: ListShards)

-- | Undocumented member.
listShards_shardFilter :: Lens.Lens' ListShards (Prelude.Maybe ShardFilter)
listShards_shardFilter = Lens.lens (\ListShards' {shardFilter} -> shardFilter) (\s@ListShards' {} a -> s {shardFilter = a} :: ListShards)

-- | The maximum number of shards to return in a single call to @ListShards@.
-- The minimum value you can specify for this parameter is 1, and the
-- maximum is 10,000, which is also the default.
--
-- When the number of shards to be listed is greater than the value of
-- @MaxResults@, the response contains a @NextToken@ value that you can use
-- in a subsequent call to @ListShards@ to list the next set of shards.
listShards_maxResults :: Lens.Lens' ListShards (Prelude.Maybe Prelude.Natural)
listShards_maxResults = Lens.lens (\ListShards' {maxResults} -> maxResults) (\s@ListShards' {} a -> s {maxResults = a} :: ListShards)

-- | Specify this input parameter to distinguish data streams that have the
-- same name. For example, if you create a data stream and then delete it,
-- and you later create another data stream with the same name, you can use
-- this input parameter to specify which of the two streams you want to
-- list the shards for.
--
-- You cannot specify this parameter if you specify the @NextToken@
-- parameter.
listShards_streamCreationTimestamp :: Lens.Lens' ListShards (Prelude.Maybe Prelude.UTCTime)
listShards_streamCreationTimestamp = Lens.lens (\ListShards' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@ListShards' {} a -> s {streamCreationTimestamp = a} :: ListShards) Prelude.. Lens.mapping Core._Time

-- | The name of the data stream whose shards you want to list.
--
-- You cannot specify this parameter if you specify the @NextToken@
-- parameter.
listShards_streamName :: Lens.Lens' ListShards (Prelude.Maybe Prelude.Text)
listShards_streamName = Lens.lens (\ListShards' {streamName} -> streamName) (\s@ListShards' {} a -> s {streamName = a} :: ListShards)

instance Core.AWSPager ListShards where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listShardsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listShardsResponse_shards Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listShards_nextToken
          Lens..~ rs
          Lens.^? listShardsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListShards where
  type AWSResponse ListShards = ListShardsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListShardsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Shards" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListShards

instance Prelude.NFData ListShards

instance Core.ToHeaders ListShards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Kinesis_20131202.ListShards" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListShards where
  toJSON ListShards' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartShardId" Core..=)
              Prelude.<$> exclusiveStartShardId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ShardFilter" Core..=) Prelude.<$> shardFilter,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StreamCreationTimestamp" Core..=)
              Prelude.<$> streamCreationTimestamp,
            ("StreamName" Core..=) Prelude.<$> streamName
          ]
      )

instance Core.ToPath ListShards where
  toPath = Prelude.const "/"

instance Core.ToQuery ListShards where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListShardsResponse' smart constructor.
data ListShardsResponse = ListShardsResponse'
  { -- | When the number of shards in the data stream is greater than the default
    -- value for the @MaxResults@ parameter, or if you explicitly specify a
    -- value for @MaxResults@ that is less than the number of shards in the
    -- data stream, the response includes a pagination token named @NextToken@.
    -- You can specify this @NextToken@ value in a subsequent call to
    -- @ListShards@ to list the next set of shards. For more information about
    -- the use of this pagination token when calling the @ListShards@
    -- operation, see ListShardsInput$NextToken.
    --
    -- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
    -- in the response to a call to @ListShards@, you have 300 seconds to use
    -- that value. If you specify an expired token in a call to @ListShards@,
    -- you get @ExpiredNextTokenException@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of JSON objects. Each object represents one shard and specifies
    -- the IDs of the shard, the shard\'s parent, and the shard that\'s
    -- adjacent to the shard\'s parent. Each object also contains the starting
    -- and ending hash keys and the starting and ending sequence numbers for
    -- the shard.
    shards :: Prelude.Maybe [Shard],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListShardsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listShardsResponse_nextToken' - When the number of shards in the data stream is greater than the default
-- value for the @MaxResults@ parameter, or if you explicitly specify a
-- value for @MaxResults@ that is less than the number of shards in the
-- data stream, the response includes a pagination token named @NextToken@.
-- You can specify this @NextToken@ value in a subsequent call to
-- @ListShards@ to list the next set of shards. For more information about
-- the use of this pagination token when calling the @ListShards@
-- operation, see ListShardsInput$NextToken.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListShards@, you have 300 seconds to use
-- that value. If you specify an expired token in a call to @ListShards@,
-- you get @ExpiredNextTokenException@.
--
-- 'shards', 'listShardsResponse_shards' - An array of JSON objects. Each object represents one shard and specifies
-- the IDs of the shard, the shard\'s parent, and the shard that\'s
-- adjacent to the shard\'s parent. Each object also contains the starting
-- and ending hash keys and the starting and ending sequence numbers for
-- the shard.
--
-- 'httpStatus', 'listShardsResponse_httpStatus' - The response's http status code.
newListShardsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListShardsResponse
newListShardsResponse pHttpStatus_ =
  ListShardsResponse'
    { nextToken = Prelude.Nothing,
      shards = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the number of shards in the data stream is greater than the default
-- value for the @MaxResults@ parameter, or if you explicitly specify a
-- value for @MaxResults@ that is less than the number of shards in the
-- data stream, the response includes a pagination token named @NextToken@.
-- You can specify this @NextToken@ value in a subsequent call to
-- @ListShards@ to list the next set of shards. For more information about
-- the use of this pagination token when calling the @ListShards@
-- operation, see ListShardsInput$NextToken.
--
-- Tokens expire after 300 seconds. When you obtain a value for @NextToken@
-- in the response to a call to @ListShards@, you have 300 seconds to use
-- that value. If you specify an expired token in a call to @ListShards@,
-- you get @ExpiredNextTokenException@.
listShardsResponse_nextToken :: Lens.Lens' ListShardsResponse (Prelude.Maybe Prelude.Text)
listShardsResponse_nextToken = Lens.lens (\ListShardsResponse' {nextToken} -> nextToken) (\s@ListShardsResponse' {} a -> s {nextToken = a} :: ListShardsResponse)

-- | An array of JSON objects. Each object represents one shard and specifies
-- the IDs of the shard, the shard\'s parent, and the shard that\'s
-- adjacent to the shard\'s parent. Each object also contains the starting
-- and ending hash keys and the starting and ending sequence numbers for
-- the shard.
listShardsResponse_shards :: Lens.Lens' ListShardsResponse (Prelude.Maybe [Shard])
listShardsResponse_shards = Lens.lens (\ListShardsResponse' {shards} -> shards) (\s@ListShardsResponse' {} a -> s {shards = a} :: ListShardsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listShardsResponse_httpStatus :: Lens.Lens' ListShardsResponse Prelude.Int
listShardsResponse_httpStatus = Lens.lens (\ListShardsResponse' {httpStatus} -> httpStatus) (\s@ListShardsResponse' {} a -> s {httpStatus = a} :: ListShardsResponse)

instance Prelude.NFData ListShardsResponse
