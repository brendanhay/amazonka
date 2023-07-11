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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of metric streams in this account.
module Amazonka.CloudWatch.ListMetricStreams
  ( -- * Creating a Request
    ListMetricStreams (..),
    newListMetricStreams,

    -- * Request Lenses
    listMetricStreams_maxResults,
    listMetricStreams_nextToken,

    -- * Destructuring the Response
    ListMetricStreamsResponse (..),
    newListMetricStreamsResponse,

    -- * Response Lenses
    listMetricStreamsResponse_entries,
    listMetricStreamsResponse_nextToken,
    listMetricStreamsResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMetricStreams' smart constructor.
data ListMetricStreams = ListMetricStreams'
  { -- | The maximum number of results to return in one operation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Include this value, if it was returned by the previous call, to get the
    -- next set of metric streams.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listMetricStreams_maxResults' - The maximum number of results to return in one operation.
--
-- 'nextToken', 'listMetricStreams_nextToken' - Include this value, if it was returned by the previous call, to get the
-- next set of metric streams.
newListMetricStreams ::
  ListMetricStreams
newListMetricStreams =
  ListMetricStreams'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in one operation.
listMetricStreams_maxResults :: Lens.Lens' ListMetricStreams (Prelude.Maybe Prelude.Natural)
listMetricStreams_maxResults = Lens.lens (\ListMetricStreams' {maxResults} -> maxResults) (\s@ListMetricStreams' {} a -> s {maxResults = a} :: ListMetricStreams)

-- | Include this value, if it was returned by the previous call, to get the
-- next set of metric streams.
listMetricStreams_nextToken :: Lens.Lens' ListMetricStreams (Prelude.Maybe Prelude.Text)
listMetricStreams_nextToken = Lens.lens (\ListMetricStreams' {nextToken} -> nextToken) (\s@ListMetricStreams' {} a -> s {nextToken = a} :: ListMetricStreams)

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
            Prelude.<$> ( x
                            Data..@? "Entries"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMetricStreams where
  hashWithSalt _salt ListMetricStreams' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListMetricStreams where
  rnf ListMetricStreams' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListMetricStreams where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListMetricStreams where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMetricStreams where
  toQuery ListMetricStreams' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListMetricStreams" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListMetricStreamsResponse' smart constructor.
data ListMetricStreamsResponse = ListMetricStreamsResponse'
  { -- | The array of metric stream information.
    entries :: Prelude.Maybe [MetricStreamEntry],
    -- | The token that marks the start of the next batch of returned results.
    -- You can use this token in a subsequent operation to get the next batch
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'entries', 'listMetricStreamsResponse_entries' - The array of metric stream information.
--
-- 'nextToken', 'listMetricStreamsResponse_nextToken' - The token that marks the start of the next batch of returned results.
-- You can use this token in a subsequent operation to get the next batch
-- of results.
--
-- 'httpStatus', 'listMetricStreamsResponse_httpStatus' - The response's http status code.
newListMetricStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMetricStreamsResponse
newListMetricStreamsResponse pHttpStatus_ =
  ListMetricStreamsResponse'
    { entries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of metric stream information.
listMetricStreamsResponse_entries :: Lens.Lens' ListMetricStreamsResponse (Prelude.Maybe [MetricStreamEntry])
listMetricStreamsResponse_entries = Lens.lens (\ListMetricStreamsResponse' {entries} -> entries) (\s@ListMetricStreamsResponse' {} a -> s {entries = a} :: ListMetricStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that marks the start of the next batch of returned results.
-- You can use this token in a subsequent operation to get the next batch
-- of results.
listMetricStreamsResponse_nextToken :: Lens.Lens' ListMetricStreamsResponse (Prelude.Maybe Prelude.Text)
listMetricStreamsResponse_nextToken = Lens.lens (\ListMetricStreamsResponse' {nextToken} -> nextToken) (\s@ListMetricStreamsResponse' {} a -> s {nextToken = a} :: ListMetricStreamsResponse)

-- | The response's http status code.
listMetricStreamsResponse_httpStatus :: Lens.Lens' ListMetricStreamsResponse Prelude.Int
listMetricStreamsResponse_httpStatus = Lens.lens (\ListMetricStreamsResponse' {httpStatus} -> httpStatus) (\s@ListMetricStreamsResponse' {} a -> s {httpStatus = a} :: ListMetricStreamsResponse)

instance Prelude.NFData ListMetricStreamsResponse where
  rnf ListMetricStreamsResponse' {..} =
    Prelude.rnf entries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
