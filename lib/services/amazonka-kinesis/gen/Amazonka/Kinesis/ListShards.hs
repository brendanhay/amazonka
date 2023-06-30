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
-- Module      : Amazonka.Kinesis.ListShards
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the shards in a stream and provides information about each shard.
-- This operation has a limit of 1000 transactions per second per data
-- stream.
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
--
-- This action does not list expired shards. For information about expired
-- shards, see
-- <https://docs.aws.amazon.com/streams/latest/dev/kinesis-using-sdk-java-after-resharding.html#kinesis-using-sdk-java-resharding-data-routing Data Routing, Data Persistence, and Shard State after a Reshard>.
--
-- This API is a new operation that is used by the Amazon Kinesis Client
-- Library (KCL). If you have a fine-grained IAM policy that only allows
-- specific operations, you must update your policy to allow calls to this
-- API. For more information, see
-- <https://docs.aws.amazon.com/streams/latest/dev/controlling-access.html Controlling Access to Amazon Kinesis Data Streams Resources Using IAM>.
--
-- This operation returns paginated results.
module Amazonka.Kinesis.ListShards
  ( -- * Creating a Request
    ListShards (..),
    newListShards,

    -- * Request Lenses
    listShards_exclusiveStartShardId,
    listShards_maxResults,
    listShards_nextToken,
    listShards_shardFilter,
    listShards_streamARN,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | The maximum number of shards to return in a single call to @ListShards@.
    -- The maximum number of shards to return in a single call. The default
    -- value is 1000. If you specify a value greater than 1000, at most 1000
    -- results are returned.
    --
    -- When the number of shards to be listed is greater than the value of
    -- @MaxResults@, the response contains a @NextToken@ value that you can use
    -- in a subsequent call to @ListShards@ to list the next set of shards.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
    -- | Enables you to filter out the response of the @ListShards@ API. You can
    -- only specify one filter at a time.
    --
    -- If you use the @ShardFilter@ parameter when invoking the ListShards API,
    -- the @Type@ is the required property and must be specified. If you
    -- specify the @AT_TRIM_HORIZON@, @FROM_TRIM_HORIZON@, or @AT_LATEST@
    -- types, you do not need to specify either the @ShardId@ or the
    -- @Timestamp@ optional properties.
    --
    -- If you specify the @AFTER_SHARD_ID@ type, you must also provide the
    -- value for the optional @ShardId@ property. The @ShardId@ property is
    -- identical in fuctionality to the @ExclusiveStartShardId@ parameter of
    -- the @ListShards@ API. When @ShardId@ property is specified, the response
    -- includes the shards starting with the shard whose ID immediately follows
    -- the @ShardId@ that you provided.
    --
    -- If you specify the @AT_TIMESTAMP@ or @FROM_TIMESTAMP_ID@ type, you must
    -- also provide the value for the optional @Timestamp@ property. If you
    -- specify the AT_TIMESTAMP type, then all shards that were open at the
    -- provided timestamp are returned. If you specify the FROM_TIMESTAMP type,
    -- then all shards starting from the provided timestamp to TIP are
    -- returned.
    shardFilter :: Prelude.Maybe ShardFilter,
    -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | Specify this input parameter to distinguish data streams that have the
    -- same name. For example, if you create a data stream and then delete it,
    -- and you later create another data stream with the same name, you can use
    -- this input parameter to specify which of the two streams you want to
    -- list the shards for.
    --
    -- You cannot specify this parameter if you specify the @NextToken@
    -- parameter.
    streamCreationTimestamp :: Prelude.Maybe Data.POSIX,
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
-- 'maxResults', 'listShards_maxResults' - The maximum number of shards to return in a single call to @ListShards@.
-- The maximum number of shards to return in a single call. The default
-- value is 1000. If you specify a value greater than 1000, at most 1000
-- results are returned.
--
-- When the number of shards to be listed is greater than the value of
-- @MaxResults@, the response contains a @NextToken@ value that you can use
-- in a subsequent call to @ListShards@ to list the next set of shards.
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
-- 'shardFilter', 'listShards_shardFilter' - Enables you to filter out the response of the @ListShards@ API. You can
-- only specify one filter at a time.
--
-- If you use the @ShardFilter@ parameter when invoking the ListShards API,
-- the @Type@ is the required property and must be specified. If you
-- specify the @AT_TRIM_HORIZON@, @FROM_TRIM_HORIZON@, or @AT_LATEST@
-- types, you do not need to specify either the @ShardId@ or the
-- @Timestamp@ optional properties.
--
-- If you specify the @AFTER_SHARD_ID@ type, you must also provide the
-- value for the optional @ShardId@ property. The @ShardId@ property is
-- identical in fuctionality to the @ExclusiveStartShardId@ parameter of
-- the @ListShards@ API. When @ShardId@ property is specified, the response
-- includes the shards starting with the shard whose ID immediately follows
-- the @ShardId@ that you provided.
--
-- If you specify the @AT_TIMESTAMP@ or @FROM_TIMESTAMP_ID@ type, you must
-- also provide the value for the optional @Timestamp@ property. If you
-- specify the AT_TIMESTAMP type, then all shards that were open at the
-- provided timestamp are returned. If you specify the FROM_TIMESTAMP type,
-- then all shards starting from the provided timestamp to TIP are
-- returned.
--
-- 'streamARN', 'listShards_streamARN' - The ARN of the stream.
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
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      shardFilter = Prelude.Nothing,
      streamARN = Prelude.Nothing,
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

-- | The maximum number of shards to return in a single call to @ListShards@.
-- The maximum number of shards to return in a single call. The default
-- value is 1000. If you specify a value greater than 1000, at most 1000
-- results are returned.
--
-- When the number of shards to be listed is greater than the value of
-- @MaxResults@, the response contains a @NextToken@ value that you can use
-- in a subsequent call to @ListShards@ to list the next set of shards.
listShards_maxResults :: Lens.Lens' ListShards (Prelude.Maybe Prelude.Natural)
listShards_maxResults = Lens.lens (\ListShards' {maxResults} -> maxResults) (\s@ListShards' {} a -> s {maxResults = a} :: ListShards)

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

-- | Enables you to filter out the response of the @ListShards@ API. You can
-- only specify one filter at a time.
--
-- If you use the @ShardFilter@ parameter when invoking the ListShards API,
-- the @Type@ is the required property and must be specified. If you
-- specify the @AT_TRIM_HORIZON@, @FROM_TRIM_HORIZON@, or @AT_LATEST@
-- types, you do not need to specify either the @ShardId@ or the
-- @Timestamp@ optional properties.
--
-- If you specify the @AFTER_SHARD_ID@ type, you must also provide the
-- value for the optional @ShardId@ property. The @ShardId@ property is
-- identical in fuctionality to the @ExclusiveStartShardId@ parameter of
-- the @ListShards@ API. When @ShardId@ property is specified, the response
-- includes the shards starting with the shard whose ID immediately follows
-- the @ShardId@ that you provided.
--
-- If you specify the @AT_TIMESTAMP@ or @FROM_TIMESTAMP_ID@ type, you must
-- also provide the value for the optional @Timestamp@ property. If you
-- specify the AT_TIMESTAMP type, then all shards that were open at the
-- provided timestamp are returned. If you specify the FROM_TIMESTAMP type,
-- then all shards starting from the provided timestamp to TIP are
-- returned.
listShards_shardFilter :: Lens.Lens' ListShards (Prelude.Maybe ShardFilter)
listShards_shardFilter = Lens.lens (\ListShards' {shardFilter} -> shardFilter) (\s@ListShards' {} a -> s {shardFilter = a} :: ListShards)

-- | The ARN of the stream.
listShards_streamARN :: Lens.Lens' ListShards (Prelude.Maybe Prelude.Text)
listShards_streamARN = Lens.lens (\ListShards' {streamARN} -> streamARN) (\s@ListShards' {} a -> s {streamARN = a} :: ListShards)

-- | Specify this input parameter to distinguish data streams that have the
-- same name. For example, if you create a data stream and then delete it,
-- and you later create another data stream with the same name, you can use
-- this input parameter to specify which of the two streams you want to
-- list the shards for.
--
-- You cannot specify this parameter if you specify the @NextToken@
-- parameter.
listShards_streamCreationTimestamp :: Lens.Lens' ListShards (Prelude.Maybe Prelude.UTCTime)
listShards_streamCreationTimestamp = Lens.lens (\ListShards' {streamCreationTimestamp} -> streamCreationTimestamp) (\s@ListShards' {} a -> s {streamCreationTimestamp = a} :: ListShards) Prelude.. Lens.mapping Data._Time

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
            Lens.^? listShardsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listShardsResponse_shards
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listShards_nextToken
          Lens..~ rs
          Lens.^? listShardsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListShards where
  type AWSResponse ListShards = ListShardsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListShardsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Shards" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListShards where
  hashWithSalt _salt ListShards' {..} =
    _salt
      `Prelude.hashWithSalt` exclusiveStartShardId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` shardFilter
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamCreationTimestamp
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData ListShards where
  rnf ListShards' {..} =
    Prelude.rnf exclusiveStartShardId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf shardFilter
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamCreationTimestamp
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders ListShards where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.ListShards" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListShards where
  toJSON ListShards' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartShardId" Data..=)
              Prelude.<$> exclusiveStartShardId,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ShardFilter" Data..=) Prelude.<$> shardFilter,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamCreationTimestamp" Data..=)
              Prelude.<$> streamCreationTimestamp,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath ListShards where
  toPath = Prelude.const "/"

instance Data.ToQuery ListShards where
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
listShardsResponse_shards = Lens.lens (\ListShardsResponse' {shards} -> shards) (\s@ListShardsResponse' {} a -> s {shards = a} :: ListShardsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listShardsResponse_httpStatus :: Lens.Lens' ListShardsResponse Prelude.Int
listShardsResponse_httpStatus = Lens.lens (\ListShardsResponse' {httpStatus} -> httpStatus) (\s@ListShardsResponse' {} a -> s {httpStatus = a} :: ListShardsResponse)

instance Prelude.NFData ListShardsResponse where
  rnf ListShardsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf shards
      `Prelude.seq` Prelude.rnf httpStatus
