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
-- Module      : Amazonka.Kinesis.ListStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Kinesis data streams.
--
-- The number of streams may be too large to return from a single call to
-- @ListStreams@. You can limit the number of returned streams using the
-- @Limit@ parameter. If you do not specify a value for the @Limit@
-- parameter, Kinesis Data Streams uses the default limit, which is
-- currently 100.
--
-- You can detect if there are more streams available to list by using the
-- @HasMoreStreams@ flag from the returned output. If there are more
-- streams available, you can request more streams by using the name of the
-- last stream returned by the @ListStreams@ request in the
-- @ExclusiveStartStreamName@ parameter in a subsequent request to
-- @ListStreams@. The group of stream names returned by the subsequent
-- request is then added to the list. You can continue this process until
-- all the stream names have been collected in the list.
--
-- ListStreams has a limit of five transactions per second per account.
--
-- This operation returns paginated results.
module Amazonka.Kinesis.ListStreams
  ( -- * Creating a Request
    ListStreams (..),
    newListStreams,

    -- * Request Lenses
    listStreams_exclusiveStartStreamName,
    listStreams_limit,
    listStreams_nextToken,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
    listStreamsResponse_nextToken,
    listStreamsResponse_streamSummaries,
    listStreamsResponse_httpStatus,
    listStreamsResponse_streamNames,
    listStreamsResponse_hasMoreStreams,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for @ListStreams@.
--
-- /See:/ 'newListStreams' smart constructor.
data ListStreams = ListStreams'
  { -- | The name of the stream to start the list with.
    exclusiveStartStreamName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of streams to list. The default value is 100. If you
    -- specify a value greater than 100, at most 100 results are returned.
    limit :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'exclusiveStartStreamName', 'listStreams_exclusiveStartStreamName' - The name of the stream to start the list with.
--
-- 'limit', 'listStreams_limit' - The maximum number of streams to list. The default value is 100. If you
-- specify a value greater than 100, at most 100 results are returned.
--
-- 'nextToken', 'listStreams_nextToken' -
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { exclusiveStartStreamName =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The name of the stream to start the list with.
listStreams_exclusiveStartStreamName :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_exclusiveStartStreamName = Lens.lens (\ListStreams' {exclusiveStartStreamName} -> exclusiveStartStreamName) (\s@ListStreams' {} a -> s {exclusiveStartStreamName = a} :: ListStreams)

-- | The maximum number of streams to list. The default value is 100. If you
-- specify a value greater than 100, at most 100 results are returned.
listStreams_limit :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Natural)
listStreams_limit = Lens.lens (\ListStreams' {limit} -> limit) (\s@ListStreams' {} a -> s {limit = a} :: ListStreams)

listStreams_nextToken :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_nextToken = Lens.lens (\ListStreams' {nextToken} -> nextToken) (\s@ListStreams' {} a -> s {nextToken = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        (rs Lens.^. listStreamsResponse_hasMoreStreams) =
        Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listStreamsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStreams_nextToken
          Lens..~ rs
          Lens.^? listStreamsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListStreams where
  type AWSResponse ListStreams = ListStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "StreamSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "StreamNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "HasMoreStreams")
      )

instance Prelude.Hashable ListStreams where
  hashWithSalt _salt ListStreams' {..} =
    _salt
      `Prelude.hashWithSalt` exclusiveStartStreamName
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStreams where
  rnf ListStreams' {..} =
    Prelude.rnf exclusiveStartStreamName
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.ListStreams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStreams where
  toJSON ListStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveStartStreamName" Data..=)
              Prelude.<$> exclusiveStartStreamName,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListStreams where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStreams where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output for @ListStreams@.
--
-- /See:/ 'newListStreamsResponse' smart constructor.
data ListStreamsResponse = ListStreamsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    streamSummaries :: Prelude.Maybe [StreamSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The names of the streams that are associated with the Amazon Web
    -- Services account making the @ListStreams@ request.
    streamNames :: [Prelude.Text],
    -- | If set to @true@, there are more streams available to list.
    hasMoreStreams :: Prelude.Bool
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
-- 'nextToken', 'listStreamsResponse_nextToken' -
--
-- 'streamSummaries', 'listStreamsResponse_streamSummaries' -
--
-- 'httpStatus', 'listStreamsResponse_httpStatus' - The response's http status code.
--
-- 'streamNames', 'listStreamsResponse_streamNames' - The names of the streams that are associated with the Amazon Web
-- Services account making the @ListStreams@ request.
--
-- 'hasMoreStreams', 'listStreamsResponse_hasMoreStreams' - If set to @true@, there are more streams available to list.
newListStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'hasMoreStreams'
  Prelude.Bool ->
  ListStreamsResponse
newListStreamsResponse pHttpStatus_ pHasMoreStreams_ =
  ListStreamsResponse'
    { nextToken = Prelude.Nothing,
      streamSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      streamNames = Prelude.mempty,
      hasMoreStreams = pHasMoreStreams_
    }

listStreamsResponse_nextToken :: Lens.Lens' ListStreamsResponse (Prelude.Maybe Prelude.Text)
listStreamsResponse_nextToken = Lens.lens (\ListStreamsResponse' {nextToken} -> nextToken) (\s@ListStreamsResponse' {} a -> s {nextToken = a} :: ListStreamsResponse)

listStreamsResponse_streamSummaries :: Lens.Lens' ListStreamsResponse (Prelude.Maybe [StreamSummary])
listStreamsResponse_streamSummaries = Lens.lens (\ListStreamsResponse' {streamSummaries} -> streamSummaries) (\s@ListStreamsResponse' {} a -> s {streamSummaries = a} :: ListStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStreamsResponse_httpStatus :: Lens.Lens' ListStreamsResponse Prelude.Int
listStreamsResponse_httpStatus = Lens.lens (\ListStreamsResponse' {httpStatus} -> httpStatus) (\s@ListStreamsResponse' {} a -> s {httpStatus = a} :: ListStreamsResponse)

-- | The names of the streams that are associated with the Amazon Web
-- Services account making the @ListStreams@ request.
listStreamsResponse_streamNames :: Lens.Lens' ListStreamsResponse [Prelude.Text]
listStreamsResponse_streamNames = Lens.lens (\ListStreamsResponse' {streamNames} -> streamNames) (\s@ListStreamsResponse' {} a -> s {streamNames = a} :: ListStreamsResponse) Prelude.. Lens.coerced

-- | If set to @true@, there are more streams available to list.
listStreamsResponse_hasMoreStreams :: Lens.Lens' ListStreamsResponse Prelude.Bool
listStreamsResponse_hasMoreStreams = Lens.lens (\ListStreamsResponse' {hasMoreStreams} -> hasMoreStreams) (\s@ListStreamsResponse' {} a -> s {hasMoreStreams = a} :: ListStreamsResponse)

instance Prelude.NFData ListStreamsResponse where
  rnf ListStreamsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamSummaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf streamNames
      `Prelude.seq` Prelude.rnf hasMoreStreams
