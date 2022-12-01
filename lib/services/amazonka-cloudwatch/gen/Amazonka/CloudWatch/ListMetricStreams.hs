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
-- Module      : Amazonka.CloudWatch.ListMetricStreams
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of metric streams in this account.
module Amazonka.CloudWatch.ListMetricStreams
  ( -- * Creating a Request
    ListMetricStreams (..),
    newListMetricStreams,

    -- * Request Lenses
    listMetricStreams_nextToken,
    listMetricStreams_maxResults,

    -- * Destructuring the Response
    ListMetricStreamsResponse (..),
    newListMetricStreamsResponse,

    -- * Response Lenses
    listMetricStreamsResponse_nextToken,
    listMetricStreamsResponse_entries,
    listMetricStreamsResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetricStreams' smart constructor.
data ListMetricStreams = ListMetricStreams'
  { -- | Include this value, if it was returned by the previous call, to get the
    -- next set of metric streams.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in one operation.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMetricStreams_nextToken' - Include this value, if it was returned by the previous call, to get the
-- next set of metric streams.
--
-- 'maxResults', 'listMetricStreams_maxResults' - The maximum number of results to return in one operation.
newListMetricStreams ::
  ListMetricStreams
newListMetricStreams =
  ListMetricStreams'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Include this value, if it was returned by the previous call, to get the
-- next set of metric streams.
listMetricStreams_nextToken :: Lens.Lens' ListMetricStreams (Prelude.Maybe Prelude.Text)
listMetricStreams_nextToken = Lens.lens (\ListMetricStreams' {nextToken} -> nextToken) (\s@ListMetricStreams' {} a -> s {nextToken = a} :: ListMetricStreams)

-- | The maximum number of results to return in one operation.
listMetricStreams_maxResults :: Lens.Lens' ListMetricStreams (Prelude.Maybe Prelude.Natural)
listMetricStreams_maxResults = Lens.lens (\ListMetricStreams' {maxResults} -> maxResults) (\s@ListMetricStreams' {} a -> s {maxResults = a} :: ListMetricStreams)

instance Core.AWSRequest ListMetricStreams where
  type
    AWSResponse ListMetricStreams =
      ListMetricStreamsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListMetricStreamsResult"
      ( \s h x ->
          ListMetricStreamsResponse'
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "Entries" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetricStreams where
  hashWithSalt _salt ListMetricStreams' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMetricStreams where
  rnf ListMetricStreams' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListMetricStreams where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListMetricStreams where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMetricStreams where
  toQuery ListMetricStreams' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ListMetricStreams" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-08-01" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListMetricStreamsResponse' smart constructor.
data ListMetricStreamsResponse = ListMetricStreamsResponse'
  { -- | The token that marks the start of the next batch of returned results.
    -- You can use this token in a subsequent operation to get the next batch
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The array of metric stream information.
    entries :: Prelude.Maybe [MetricStreamEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMetricStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMetricStreamsResponse_nextToken' - The token that marks the start of the next batch of returned results.
-- You can use this token in a subsequent operation to get the next batch
-- of results.
--
-- 'entries', 'listMetricStreamsResponse_entries' - The array of metric stream information.
--
-- 'httpStatus', 'listMetricStreamsResponse_httpStatus' - The response's http status code.
newListMetricStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricStreamsResponse
newListMetricStreamsResponse pHttpStatus_ =
  ListMetricStreamsResponse'
    { nextToken =
        Prelude.Nothing,
      entries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that marks the start of the next batch of returned results.
-- You can use this token in a subsequent operation to get the next batch
-- of results.
listMetricStreamsResponse_nextToken :: Lens.Lens' ListMetricStreamsResponse (Prelude.Maybe Prelude.Text)
listMetricStreamsResponse_nextToken = Lens.lens (\ListMetricStreamsResponse' {nextToken} -> nextToken) (\s@ListMetricStreamsResponse' {} a -> s {nextToken = a} :: ListMetricStreamsResponse)

-- | The array of metric stream information.
listMetricStreamsResponse_entries :: Lens.Lens' ListMetricStreamsResponse (Prelude.Maybe [MetricStreamEntry])
listMetricStreamsResponse_entries = Lens.lens (\ListMetricStreamsResponse' {entries} -> entries) (\s@ListMetricStreamsResponse' {} a -> s {entries = a} :: ListMetricStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMetricStreamsResponse_httpStatus :: Lens.Lens' ListMetricStreamsResponse Prelude.Int
listMetricStreamsResponse_httpStatus = Lens.lens (\ListMetricStreamsResponse' {httpStatus} -> httpStatus) (\s@ListMetricStreamsResponse' {} a -> s {httpStatus = a} :: ListMetricStreamsResponse)

instance Prelude.NFData ListMetricStreamsResponse where
  rnf ListMetricStreamsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf entries
      `Prelude.seq` Prelude.rnf httpStatus
