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
-- Module      : Amazonka.KinesisVideoArchivedMedia.ListFragments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of Fragment objects from the specified stream and
-- timestamp range within the archived data.
--
-- Listing fragments is eventually consistent. This means that even if the
-- producer receives an acknowledgment that a fragment is persisted, the
-- result might not be returned immediately from a request to
-- @ListFragments@. However, results are typically available in less than
-- one second.
--
-- You must first call the @GetDataEndpoint@ API to get an endpoint. Then
-- send the @ListFragments@ requests to this endpoint using the
-- <https://docs.aws.amazon.com/cli/latest/reference/ --endpoint-url parameter>.
--
-- If an error is thrown after invoking a Kinesis Video Streams archived
-- media API, in addition to the HTTP status code and the response body, it
-- includes the following pieces of information:
--
-- -   @x-amz-ErrorType@ HTTP header – contains a more specific error type
--     in addition to what the HTTP status code provides.
--
-- -   @x-amz-RequestId@ HTTP header – if you want to report an issue to
--     AWS, the support team can better diagnose the problem if given the
--     Request Id.
--
-- Both the HTTP status code and the ErrorType header can be utilized to
-- make programmatic decisions about whether errors are retry-able and
-- under what conditions, as well as provide information on what actions
-- the client programmer might need to take in order to successfully try
-- again.
--
-- For more information, see the __Errors__ section at the bottom of this
-- topic, as well as
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/CommonErrors.html Common Errors>.
--
-- This operation returns paginated results.
module Amazonka.KinesisVideoArchivedMedia.ListFragments
  ( -- * Creating a Request
    ListFragments (..),
    newListFragments,

    -- * Request Lenses
    listFragments_fragmentSelector,
    listFragments_maxResults,
    listFragments_nextToken,
    listFragments_streamARN,
    listFragments_streamName,

    -- * Destructuring the Response
    ListFragmentsResponse (..),
    newListFragmentsResponse,

    -- * Response Lenses
    listFragmentsResponse_fragments,
    listFragmentsResponse_nextToken,
    listFragmentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoArchivedMedia.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFragments' smart constructor.
data ListFragments = ListFragments'
  { -- | Describes the timestamp range and timestamp origin for the range of
    -- fragments to return.
    fragmentSelector :: Prelude.Maybe FragmentSelector,
    -- | The total number of fragments to return. If the total number of
    -- fragments available is more than the value specified in @max-results@,
    -- then a ListFragmentsOutput$NextToken is provided in the output that you
    -- can use to resume pagination.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the
    -- ListFragmentsOutput$NextToken from a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the stream from which to retrieve a
    -- fragment list. Specify either this parameter or the @StreamName@
    -- parameter.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream from which to retrieve a fragment list. Specify
    -- either this parameter or the @StreamARN@ parameter.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFragments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragmentSelector', 'listFragments_fragmentSelector' - Describes the timestamp range and timestamp origin for the range of
-- fragments to return.
--
-- 'maxResults', 'listFragments_maxResults' - The total number of fragments to return. If the total number of
-- fragments available is more than the value specified in @max-results@,
-- then a ListFragmentsOutput$NextToken is provided in the output that you
-- can use to resume pagination.
--
-- 'nextToken', 'listFragments_nextToken' - A token to specify where to start paginating. This is the
-- ListFragmentsOutput$NextToken from a previously truncated response.
--
-- 'streamARN', 'listFragments_streamARN' - The Amazon Resource Name (ARN) of the stream from which to retrieve a
-- fragment list. Specify either this parameter or the @StreamName@
-- parameter.
--
-- 'streamName', 'listFragments_streamName' - The name of the stream from which to retrieve a fragment list. Specify
-- either this parameter or the @StreamARN@ parameter.
newListFragments ::
  ListFragments
newListFragments =
  ListFragments'
    { fragmentSelector = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Describes the timestamp range and timestamp origin for the range of
-- fragments to return.
listFragments_fragmentSelector :: Lens.Lens' ListFragments (Prelude.Maybe FragmentSelector)
listFragments_fragmentSelector = Lens.lens (\ListFragments' {fragmentSelector} -> fragmentSelector) (\s@ListFragments' {} a -> s {fragmentSelector = a} :: ListFragments)

-- | The total number of fragments to return. If the total number of
-- fragments available is more than the value specified in @max-results@,
-- then a ListFragmentsOutput$NextToken is provided in the output that you
-- can use to resume pagination.
listFragments_maxResults :: Lens.Lens' ListFragments (Prelude.Maybe Prelude.Natural)
listFragments_maxResults = Lens.lens (\ListFragments' {maxResults} -> maxResults) (\s@ListFragments' {} a -> s {maxResults = a} :: ListFragments)

-- | A token to specify where to start paginating. This is the
-- ListFragmentsOutput$NextToken from a previously truncated response.
listFragments_nextToken :: Lens.Lens' ListFragments (Prelude.Maybe Prelude.Text)
listFragments_nextToken = Lens.lens (\ListFragments' {nextToken} -> nextToken) (\s@ListFragments' {} a -> s {nextToken = a} :: ListFragments)

-- | The Amazon Resource Name (ARN) of the stream from which to retrieve a
-- fragment list. Specify either this parameter or the @StreamName@
-- parameter.
listFragments_streamARN :: Lens.Lens' ListFragments (Prelude.Maybe Prelude.Text)
listFragments_streamARN = Lens.lens (\ListFragments' {streamARN} -> streamARN) (\s@ListFragments' {} a -> s {streamARN = a} :: ListFragments)

-- | The name of the stream from which to retrieve a fragment list. Specify
-- either this parameter or the @StreamARN@ parameter.
listFragments_streamName :: Lens.Lens' ListFragments (Prelude.Maybe Prelude.Text)
listFragments_streamName = Lens.lens (\ListFragments' {streamName} -> streamName) (\s@ListFragments' {} a -> s {streamName = a} :: ListFragments)

instance Core.AWSPager ListFragments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFragmentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFragmentsResponse_fragments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listFragments_nextToken
          Lens..~ rs
          Lens.^? listFragmentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListFragments where
  type
    AWSResponse ListFragments =
      ListFragmentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFragmentsResponse'
            Prelude.<$> (x Data..?> "Fragments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFragments where
  hashWithSalt _salt ListFragments' {..} =
    _salt
      `Prelude.hashWithSalt` fragmentSelector
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData ListFragments where
  rnf ListFragments' {..} =
    Prelude.rnf fragmentSelector
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders ListFragments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListFragments where
  toJSON ListFragments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FragmentSelector" Data..=)
              Prelude.<$> fragmentSelector,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath ListFragments where
  toPath = Prelude.const "/listFragments"

instance Data.ToQuery ListFragments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFragmentsResponse' smart constructor.
data ListFragmentsResponse = ListFragmentsResponse'
  { -- | A list of archived Fragment objects from the stream that meet the
    -- selector criteria. Results are in no specific order, even across pages.
    fragments :: Prelude.Maybe [Fragment],
    -- | If the returned list is truncated, the operation returns this token to
    -- use to retrieve the next page of results. This value is @null@ when
    -- there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFragmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fragments', 'listFragmentsResponse_fragments' - A list of archived Fragment objects from the stream that meet the
-- selector criteria. Results are in no specific order, even across pages.
--
-- 'nextToken', 'listFragmentsResponse_nextToken' - If the returned list is truncated, the operation returns this token to
-- use to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
--
-- 'httpStatus', 'listFragmentsResponse_httpStatus' - The response's http status code.
newListFragmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFragmentsResponse
newListFragmentsResponse pHttpStatus_ =
  ListFragmentsResponse'
    { fragments = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of archived Fragment objects from the stream that meet the
-- selector criteria. Results are in no specific order, even across pages.
listFragmentsResponse_fragments :: Lens.Lens' ListFragmentsResponse (Prelude.Maybe [Fragment])
listFragmentsResponse_fragments = Lens.lens (\ListFragmentsResponse' {fragments} -> fragments) (\s@ListFragmentsResponse' {} a -> s {fragments = a} :: ListFragmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the returned list is truncated, the operation returns this token to
-- use to retrieve the next page of results. This value is @null@ when
-- there are no more results to return.
listFragmentsResponse_nextToken :: Lens.Lens' ListFragmentsResponse (Prelude.Maybe Prelude.Text)
listFragmentsResponse_nextToken = Lens.lens (\ListFragmentsResponse' {nextToken} -> nextToken) (\s@ListFragmentsResponse' {} a -> s {nextToken = a} :: ListFragmentsResponse)

-- | The response's http status code.
listFragmentsResponse_httpStatus :: Lens.Lens' ListFragmentsResponse Prelude.Int
listFragmentsResponse_httpStatus = Lens.lens (\ListFragmentsResponse' {httpStatus} -> httpStatus) (\s@ListFragmentsResponse' {} a -> s {httpStatus = a} :: ListFragmentsResponse)

instance Prelude.NFData ListFragmentsResponse where
  rnf ListFragmentsResponse' {..} =
    Prelude.rnf fragments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
