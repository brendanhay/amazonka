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
-- Module      : Amazonka.QuickSight.ListTemplates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the templates in the current Amazon QuickSight account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListTemplates
  ( -- * Creating a Request
    ListTemplates (..),
    newListTemplates,

    -- * Request Lenses
    listTemplates_maxResults,
    listTemplates_nextToken,
    listTemplates_awsAccountId,

    -- * Destructuring the Response
    ListTemplatesResponse (..),
    newListTemplatesResponse,

    -- * Response Lenses
    listTemplatesResponse_nextToken,
    listTemplatesResponse_requestId,
    listTemplatesResponse_templateSummaryList,
    listTemplatesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the templates
    -- that you\'re listing.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTemplates_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listTemplates_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listTemplates_awsAccountId' - The ID of the Amazon Web Services account that contains the templates
-- that you\'re listing.
newListTemplates ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListTemplates
newListTemplates pAwsAccountId_ =
  ListTemplates'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listTemplates_maxResults :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Natural)
listTemplates_maxResults = Lens.lens (\ListTemplates' {maxResults} -> maxResults) (\s@ListTemplates' {} a -> s {maxResults = a} :: ListTemplates)

-- | The token for the next set of results, or null if there are no more
-- results.
listTemplates_nextToken :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Text)
listTemplates_nextToken = Lens.lens (\ListTemplates' {nextToken} -> nextToken) (\s@ListTemplates' {} a -> s {nextToken = a} :: ListTemplates)

-- | The ID of the Amazon Web Services account that contains the templates
-- that you\'re listing.
listTemplates_awsAccountId :: Lens.Lens' ListTemplates Prelude.Text
listTemplates_awsAccountId = Lens.lens (\ListTemplates' {awsAccountId} -> awsAccountId) (\s@ListTemplates' {} a -> s {awsAccountId = a} :: ListTemplates)

instance Core.AWSPager ListTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplatesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTemplatesResponse_templateSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTemplates_nextToken
          Lens..~ rs
          Lens.^? listTemplatesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTemplates where
  type
    AWSResponse ListTemplates =
      ListTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x
                            Data..?> "TemplateSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTemplates where
  hashWithSalt _salt ListTemplates' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListTemplates where
  rnf ListTemplates' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTemplates where
  toPath ListTemplates' {..} =
    Prelude.mconcat
      ["/accounts/", Data.toBS awsAccountId, "/templates"]

instance Data.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Prelude.mconcat
      [ "max-result" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure containing information about the templates in the list.
    templateSummaryList :: Prelude.Maybe [TemplateSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplatesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listTemplatesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateSummaryList', 'listTemplatesResponse_templateSummaryList' - A structure containing information about the templates in the list.
--
-- 'status', 'listTemplatesResponse_status' - The HTTP status of the request.
newListTemplatesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListTemplatesResponse
newListTemplatesResponse pStatus_ =
  ListTemplatesResponse'
    { nextToken = Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listTemplatesResponse_nextToken :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe Prelude.Text)
listTemplatesResponse_nextToken = Lens.lens (\ListTemplatesResponse' {nextToken} -> nextToken) (\s@ListTemplatesResponse' {} a -> s {nextToken = a} :: ListTemplatesResponse)

-- | The Amazon Web Services request ID for this operation.
listTemplatesResponse_requestId :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe Prelude.Text)
listTemplatesResponse_requestId = Lens.lens (\ListTemplatesResponse' {requestId} -> requestId) (\s@ListTemplatesResponse' {} a -> s {requestId = a} :: ListTemplatesResponse)

-- | A structure containing information about the templates in the list.
listTemplatesResponse_templateSummaryList :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe [TemplateSummary])
listTemplatesResponse_templateSummaryList = Lens.lens (\ListTemplatesResponse' {templateSummaryList} -> templateSummaryList) (\s@ListTemplatesResponse' {} a -> s {templateSummaryList = a} :: ListTemplatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listTemplatesResponse_status :: Lens.Lens' ListTemplatesResponse Prelude.Int
listTemplatesResponse_status = Lens.lens (\ListTemplatesResponse' {status} -> status) (\s@ListTemplatesResponse' {} a -> s {status = a} :: ListTemplatesResponse)

instance Prelude.NFData ListTemplatesResponse where
  rnf ListTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf templateSummaryList
      `Prelude.seq` Prelude.rnf status
