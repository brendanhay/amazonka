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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listStreams_limit,
    listStreams_exclusiveStartStreamName,

    -- * Destructuring the Response
    ListStreamsResponse (..),
    newListStreamsResponse,

    -- * Response Lenses
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
  { -- | The maximum number of streams to list. The default value is 100. If you
    -- specify a value greater than 100, at most 100 results are returned.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The name of the stream to start the list with.
    exclusiveStartStreamName :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listStreams_limit' - The maximum number of streams to list. The default value is 100. If you
-- specify a value greater than 100, at most 100 results are returned.
--
-- 'exclusiveStartStreamName', 'listStreams_exclusiveStartStreamName' - The name of the stream to start the list with.
newListStreams ::
  ListStreams
newListStreams =
  ListStreams'
    { limit = Prelude.Nothing,
      exclusiveStartStreamName = Prelude.Nothing
    }

-- | The maximum number of streams to list. The default value is 100. If you
-- specify a value greater than 100, at most 100 results are returned.
listStreams_limit :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Natural)
listStreams_limit = Lens.lens (\ListStreams' {limit} -> limit) (\s@ListStreams' {} a -> s {limit = a} :: ListStreams)

-- | The name of the stream to start the list with.
listStreams_exclusiveStartStreamName :: Lens.Lens' ListStreams (Prelude.Maybe Prelude.Text)
listStreams_exclusiveStartStreamName = Lens.lens (\ListStreams' {exclusiveStartStreamName} -> exclusiveStartStreamName) (\s@ListStreams' {} a -> s {exclusiveStartStreamName = a} :: ListStreams)

instance Core.AWSPager ListStreams where
  page rq rs
    | Core.stop
        (rs Lens.^. listStreamsResponse_hasMoreStreams) =
      Prelude.Nothing
    | Prelude.isNothing
        ( rs
            Lens.^? listStreamsResponse_streamNames Prelude.. Lens._last
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStreams_exclusiveStartStreamName
          Lens..~ rs
          Lens.^? listStreamsResponse_streamNames Prelude.. Lens._last

instance Core.AWSRequest ListStreams where
  type AWSResponse ListStreams = ListStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "StreamNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "HasMoreStreams")
      )

instance Prelude.Hashable ListStreams where
  hashWithSalt _salt ListStreams' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` exclusiveStartStreamName

instance Prelude.NFData ListStreams where
  rnf ListStreams' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf exclusiveStartStreamName

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
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("ExclusiveStartStreamName" Data..=)
              Prelude.<$> exclusiveStartStreamName
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
  { -- | The response's http status code.
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
    { httpStatus = pHttpStatus_,
      streamNames = Prelude.mempty,
      hasMoreStreams = pHasMoreStreams_
    }

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
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf streamNames
      `Prelude.seq` Prelude.rnf hasMoreStreams
