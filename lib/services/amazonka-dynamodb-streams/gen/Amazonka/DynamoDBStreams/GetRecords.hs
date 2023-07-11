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
-- Module      : Amazonka.DynamoDBStreams.GetRecords
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the stream records from a given shard.
--
-- Specify a shard iterator using the @ShardIterator@ parameter. The shard
-- iterator specifies the position in the shard from which you want to
-- start reading stream records sequentially. If there are no stream
-- records available in the portion of the shard that the iterator points
-- to, @GetRecords@ returns an empty list. Note that it might take multiple
-- calls to get to a portion of the shard that contains stream records.
--
-- @GetRecords@ can retrieve a maximum of 1 MB of data or 1000 stream
-- records, whichever comes first.
module Amazonka.DynamoDBStreams.GetRecords
  ( -- * Creating a Request
    GetRecords (..),
    newGetRecords,

    -- * Request Lenses
    getRecords_limit,
    getRecords_shardIterator,

    -- * Destructuring the Response
    GetRecordsResponse (..),
    newGetRecordsResponse,

    -- * Response Lenses
    getRecordsResponse_nextShardIterator,
    getRecordsResponse_records,
    getRecordsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDBStreams.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetRecords@ operation.
--
-- /See:/ 'newGetRecords' smart constructor.
data GetRecords = GetRecords'
  { -- | The maximum number of records to return from the shard. The upper limit
    -- is 1000.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | A shard iterator that was retrieved from a previous GetShardIterator
    -- operation. This iterator can be used to access the stream records in
    -- this shard.
    shardIterator :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'getRecords_limit' - The maximum number of records to return from the shard. The upper limit
-- is 1000.
--
-- 'shardIterator', 'getRecords_shardIterator' - A shard iterator that was retrieved from a previous GetShardIterator
-- operation. This iterator can be used to access the stream records in
-- this shard.
newGetRecords ::
  -- | 'shardIterator'
  Prelude.Text ->
  GetRecords
newGetRecords pShardIterator_ =
  GetRecords'
    { limit = Prelude.Nothing,
      shardIterator = pShardIterator_
    }

-- | The maximum number of records to return from the shard. The upper limit
-- is 1000.
getRecords_limit :: Lens.Lens' GetRecords (Prelude.Maybe Prelude.Natural)
getRecords_limit = Lens.lens (\GetRecords' {limit} -> limit) (\s@GetRecords' {} a -> s {limit = a} :: GetRecords)

-- | A shard iterator that was retrieved from a previous GetShardIterator
-- operation. This iterator can be used to access the stream records in
-- this shard.
getRecords_shardIterator :: Lens.Lens' GetRecords Prelude.Text
getRecords_shardIterator = Lens.lens (\GetRecords' {shardIterator} -> shardIterator) (\s@GetRecords' {} a -> s {shardIterator = a} :: GetRecords)

instance Core.AWSRequest GetRecords where
  type AWSResponse GetRecords = GetRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecordsResponse'
            Prelude.<$> (x Data..?> "NextShardIterator")
            Prelude.<*> (x Data..?> "Records" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRecords where
  hashWithSalt _salt GetRecords' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` shardIterator

instance Prelude.NFData GetRecords where
  rnf GetRecords' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf shardIterator

instance Data.ToHeaders GetRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDBStreams_20120810.GetRecords" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRecords where
  toJSON GetRecords' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            Prelude.Just
              ("ShardIterator" Data..= shardIterator)
          ]
      )

instance Data.ToPath GetRecords where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRecords where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetRecords@ operation.
--
-- /See:/ 'newGetRecordsResponse' smart constructor.
data GetRecordsResponse = GetRecordsResponse'
  { -- | The next position in the shard from which to start sequentially reading
    -- stream records. If set to @null@, the shard has been closed and the
    -- requested iterator will not return any more data.
    nextShardIterator :: Prelude.Maybe Prelude.Text,
    -- | The stream records from the shard, which were retrieved using the shard
    -- iterator.
    records :: Prelude.Maybe [Record],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextShardIterator', 'getRecordsResponse_nextShardIterator' - The next position in the shard from which to start sequentially reading
-- stream records. If set to @null@, the shard has been closed and the
-- requested iterator will not return any more data.
--
-- 'records', 'getRecordsResponse_records' - The stream records from the shard, which were retrieved using the shard
-- iterator.
--
-- 'httpStatus', 'getRecordsResponse_httpStatus' - The response's http status code.
newGetRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecordsResponse
newGetRecordsResponse pHttpStatus_ =
  GetRecordsResponse'
    { nextShardIterator =
        Prelude.Nothing,
      records = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next position in the shard from which to start sequentially reading
-- stream records. If set to @null@, the shard has been closed and the
-- requested iterator will not return any more data.
getRecordsResponse_nextShardIterator :: Lens.Lens' GetRecordsResponse (Prelude.Maybe Prelude.Text)
getRecordsResponse_nextShardIterator = Lens.lens (\GetRecordsResponse' {nextShardIterator} -> nextShardIterator) (\s@GetRecordsResponse' {} a -> s {nextShardIterator = a} :: GetRecordsResponse)

-- | The stream records from the shard, which were retrieved using the shard
-- iterator.
getRecordsResponse_records :: Lens.Lens' GetRecordsResponse (Prelude.Maybe [Record])
getRecordsResponse_records = Lens.lens (\GetRecordsResponse' {records} -> records) (\s@GetRecordsResponse' {} a -> s {records = a} :: GetRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRecordsResponse_httpStatus :: Lens.Lens' GetRecordsResponse Prelude.Int
getRecordsResponse_httpStatus = Lens.lens (\GetRecordsResponse' {httpStatus} -> httpStatus) (\s@GetRecordsResponse' {} a -> s {httpStatus = a} :: GetRecordsResponse)

instance Prelude.NFData GetRecordsResponse where
  rnf GetRecordsResponse' {..} =
    Prelude.rnf nextShardIterator
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf httpStatus
