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
-- Module      : Amazonka.DynamoDBStreams.ListStreams
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of stream ARNs associated with the current account and
-- endpoint. If the @TableName@ parameter is present, then @ListStreams@
-- will return only the streams ARNs for that table.
--
-- You can call @ListStreams@ at a maximum rate of 5 times per second.
module Amazonka.DynamoDBStreams.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_tableName,
    listStreams_exclusiveStartStreamArn,
    listStreams_limit,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_streams,
    listStreamsResponse_lastEvaluatedStreamArn,
    listStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDBStreams.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListStreams@ operation.
--
-- /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | If this parameter is provided, then only the streams associated with
    -- this table name are returned.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the first item that this operation
    -- will evaluate. Use the value that was returned for
    -- @LastEvaluatedStreamArn@ in the previous operation.
    exclusiveStartStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of streams to return. The upper limit is 100.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'listStreams_tableName' - If this parameter is provided, then only the streams associated with
-- this table name are returned.
--
-- 'exclusiveStartStreamArn', 'listStreams_exclusiveStartStreamArn' - The ARN (Amazon Resource Name) of the first item that this operation
-- will evaluate. Use the value that was returned for
-- @LastEvaluatedStreamArn@ in the previous operation.
--
-- 'limit', 'listStreams_limit' - The maximum number of streams to return. The upper limit is 100.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { tableName = Prelude.Nothing,
      exclusiveStartStreamArn = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If this parameter is provided, then only the streams associated with
-- this table name are returned.
listStreams_tableName :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_tableName = Lens.lens (\ListStreams' {tableName} -> tableName) (\s@ListStreams' {} a -> s {tableName = a} :: ListStreams)

-- | The ARN (Amazon Resource Name) of the first item that this operation
-- will evaluate. Use the value that was returned for
-- @LastEvaluatedStreamArn@ in the previous operation.
listStreams_exclusiveStartStreamArn :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_exclusiveStartStreamArn = Lens.lens (\ListStreams' {exclusiveStartStreamArn} -> exclusiveStartStreamArn) (\s@ListStreams' {} a -> s {exclusiveStartStreamArn = a} :: ListStreams)

-- | The maximum number of streams to return. The upper limit is 100.
listStreams_limit :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Natural)
listStreams_limit = Lens.lens (\ListStreams' {limit} -> limit) (\s@ListStreams' {} a -> s {limit = a} :: ListStreams)

instance Core.AWSRequest ListStreams where
  type AWSResponse ListStreams = ListStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Prelude.<$> (x Core..?> "Streams" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LastEvaluatedStreamArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreams where
  hashWithSalt _salt ListStreams' {..} =
    _salt `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` exclusiveStartStreamArn
      `Prelude.hashWithSalt` limit

instance Prelude.NFData ListStreams where
  rnf ListStreams' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf exclusiveStartStreamArn
      `Prelude.seq` Prelude.rnf limit

instance Core.ToHeaders ListStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDBStreams_20120810.ListStreams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TableName" Core..=) Prelude.<$> tableName,
            ("ExclusiveStartStreamArn" Core..=)
              Prelude.<$> exclusiveStartStreamArn,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListStreams where
  toPath = Prelude.const "/"

instance Core.ToQuery ListStreams where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListStreams@ operation.
--
-- /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { -- | A list of stream descriptors associated with the current account and
    -- endpoint.
    streams :: Prelude.Maybe [Stream],
    -- | The stream ARN of the item where the operation stopped, inclusive of the
    -- previous result set. Use this value to start a new operation, excluding
    -- this value in the new request.
    --
    -- If @LastEvaluatedStreamArn@ is empty, then the \"last page\" of results
    -- has been processed and there is no more data to be retrieved.
    --
    -- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean
    -- that there is more data in the result set. The only way to know when you
    -- have reached the end of the result set is when @LastEvaluatedStreamArn@
    -- is empty.
    lastEvaluatedStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streams', 'listStreamsResponse_streams' - A list of stream descriptors associated with the current account and
-- endpoint.
--
-- 'lastEvaluatedStreamArn', 'listStreamsResponse_lastEvaluatedStreamArn' - The stream ARN of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request.
--
-- If @LastEvaluatedStreamArn@ is empty, then the \"last page\" of results
-- has been processed and there is no more data to be retrieved.
--
-- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean
-- that there is more data in the result set. The only way to know when you
-- have reached the end of the result set is when @LastEvaluatedStreamArn@
-- is empty.
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
newListStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ =
  ListStreamsResponse'
    { streams = Prelude.Nothing,
      lastEvaluatedStreamArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of stream descriptors associated with the current account and
-- endpoint.
listStreamsResponse_streams :: Lens.Lens' ListStreamsResponse (Prelude.Maybe [Stream])
listStreamsResponse_streams = Lens.lens (\ListStreamsResponse' {streams} -> streams) (\s@ListStreamsResponse' {} a -> s {streams = a} :: ListStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The stream ARN of the item where the operation stopped, inclusive of the
-- previous result set. Use this value to start a new operation, excluding
-- this value in the new request.
--
-- If @LastEvaluatedStreamArn@ is empty, then the \"last page\" of results
-- has been processed and there is no more data to be retrieved.
--
-- If @LastEvaluatedStreamArn@ is not empty, it does not necessarily mean
-- that there is more data in the result set. The only way to know when you
-- have reached the end of the result set is when @LastEvaluatedStreamArn@
-- is empty.
listStreamsResponse_lastEvaluatedStreamArn :: Lens.Lens' ListStreamsResponse (Prelude.Maybe Prelude.Text)
listStreamsResponse_lastEvaluatedStreamArn = Lens.lens (\ListStreamsResponse' {lastEvaluatedStreamArn} -> lastEvaluatedStreamArn) (\s@ListStreamsResponse' {} a -> s {lastEvaluatedStreamArn = a} :: ListStreamsResponse)

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Prelude.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

instance Prelude.NFData ListStreamsResponse where
  rnf ListStreamsResponse' {..} =
    Prelude.rnf streams
      `Prelude.seq` Prelude.rnf lastEvaluatedStreamArn
      `Prelude.seq` Prelude.rnf httpStatus
