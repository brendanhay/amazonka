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
-- Module      : Amazonka.QuickSight.ListTemplateVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of the templates in the current Amazon QuickSight
-- account.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListTemplateVersions
  ( -- * Creating a Request
    ListTemplateVersions (..),
    newListTemplateVersions,

    -- * Request Lenses
    listTemplateVersions_maxResults,
    listTemplateVersions_nextToken,
    listTemplateVersions_awsAccountId,
    listTemplateVersions_templateId,

    -- * Destructuring the Response
    ListTemplateVersionsResponse (..),
    newListTemplateVersionsResponse,

    -- * Response Lenses
    listTemplateVersionsResponse_nextToken,
    listTemplateVersionsResponse_requestId,
    listTemplateVersionsResponse_templateVersionSummaryList,
    listTemplateVersionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplateVersions' smart constructor.
data ListTemplateVersions = ListTemplateVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the templates
    -- that you\'re listing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTemplateVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listTemplateVersions_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listTemplateVersions_awsAccountId' - The ID of the Amazon Web Services account that contains the templates
-- that you\'re listing.
--
-- 'templateId', 'listTemplateVersions_templateId' - The ID for the template.
newListTemplateVersions ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  ListTemplateVersions
newListTemplateVersions pAwsAccountId_ pTemplateId_ =
  ListTemplateVersions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      templateId = pTemplateId_
    }

-- | The maximum number of results to be returned per request.
listTemplateVersions_maxResults :: Lens.Lens' ListTemplateVersions (Prelude.Maybe Prelude.Natural)
listTemplateVersions_maxResults = Lens.lens (\ListTemplateVersions' {maxResults} -> maxResults) (\s@ListTemplateVersions' {} a -> s {maxResults = a} :: ListTemplateVersions)

-- | The token for the next set of results, or null if there are no more
-- results.
listTemplateVersions_nextToken :: Lens.Lens' ListTemplateVersions (Prelude.Maybe Prelude.Text)
listTemplateVersions_nextToken = Lens.lens (\ListTemplateVersions' {nextToken} -> nextToken) (\s@ListTemplateVersions' {} a -> s {nextToken = a} :: ListTemplateVersions)

-- | The ID of the Amazon Web Services account that contains the templates
-- that you\'re listing.
listTemplateVersions_awsAccountId :: Lens.Lens' ListTemplateVersions Prelude.Text
listTemplateVersions_awsAccountId = Lens.lens (\ListTemplateVersions' {awsAccountId} -> awsAccountId) (\s@ListTemplateVersions' {} a -> s {awsAccountId = a} :: ListTemplateVersions)

-- | The ID for the template.
listTemplateVersions_templateId :: Lens.Lens' ListTemplateVersions Prelude.Text
listTemplateVersions_templateId = Lens.lens (\ListTemplateVersions' {templateId} -> templateId) (\s@ListTemplateVersions' {} a -> s {templateId = a} :: ListTemplateVersions)

instance Core.AWSPager ListTemplateVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplateVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTemplateVersionsResponse_templateVersionSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTemplateVersions_nextToken
          Lens..~ rs
          Lens.^? listTemplateVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTemplateVersions where
  type
    AWSResponse ListTemplateVersions =
      ListTemplateVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x
                            Data..?> "TemplateVersionSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTemplateVersions where
  hashWithSalt _salt ListTemplateVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData ListTemplateVersions where
  rnf ListTemplateVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf templateId

instance Data.ToHeaders ListTemplateVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTemplateVersions where
  toPath ListTemplateVersions' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/versions"
      ]

instance Data.ToQuery ListTemplateVersions where
  toQuery ListTemplateVersions' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListTemplateVersionsResponse' smart constructor.
data ListTemplateVersionsResponse = ListTemplateVersionsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure containing a list of all the versions of the specified
    -- template.
    templateVersionSummaryList :: Prelude.Maybe [TemplateVersionSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateVersionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listTemplateVersionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateVersionSummaryList', 'listTemplateVersionsResponse_templateVersionSummaryList' - A structure containing a list of all the versions of the specified
-- template.
--
-- 'status', 'listTemplateVersionsResponse_status' - The HTTP status of the request.
newListTemplateVersionsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListTemplateVersionsResponse
newListTemplateVersionsResponse pStatus_ =
  ListTemplateVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateVersionSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listTemplateVersionsResponse_nextToken :: Lens.Lens' ListTemplateVersionsResponse (Prelude.Maybe Prelude.Text)
listTemplateVersionsResponse_nextToken = Lens.lens (\ListTemplateVersionsResponse' {nextToken} -> nextToken) (\s@ListTemplateVersionsResponse' {} a -> s {nextToken = a} :: ListTemplateVersionsResponse)

-- | The Amazon Web Services request ID for this operation.
listTemplateVersionsResponse_requestId :: Lens.Lens' ListTemplateVersionsResponse (Prelude.Maybe Prelude.Text)
listTemplateVersionsResponse_requestId = Lens.lens (\ListTemplateVersionsResponse' {requestId} -> requestId) (\s@ListTemplateVersionsResponse' {} a -> s {requestId = a} :: ListTemplateVersionsResponse)

-- | A structure containing a list of all the versions of the specified
-- template.
listTemplateVersionsResponse_templateVersionSummaryList :: Lens.Lens' ListTemplateVersionsResponse (Prelude.Maybe [TemplateVersionSummary])
listTemplateVersionsResponse_templateVersionSummaryList = Lens.lens (\ListTemplateVersionsResponse' {templateVersionSummaryList} -> templateVersionSummaryList) (\s@ListTemplateVersionsResponse' {} a -> s {templateVersionSummaryList = a} :: ListTemplateVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listTemplateVersionsResponse_status :: Lens.Lens' ListTemplateVersionsResponse Prelude.Int
listTemplateVersionsResponse_status = Lens.lens (\ListTemplateVersionsResponse' {status} -> status) (\s@ListTemplateVersionsResponse' {} a -> s {status = a} :: ListTemplateVersionsResponse)

instance Prelude.NFData ListTemplateVersionsResponse where
  rnf ListTemplateVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf templateVersionSummaryList
      `Prelude.seq` Prelude.rnf status
