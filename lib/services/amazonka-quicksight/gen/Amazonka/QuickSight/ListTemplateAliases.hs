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
-- Module      : Amazonka.QuickSight.ListTemplateAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the aliases of a template.
--
-- This operation returns paginated results.
module Amazonka.QuickSight.ListTemplateAliases
  ( -- * Creating a Request
    ListTemplateAliases (..),
    newListTemplateAliases,

    -- * Request Lenses
    listTemplateAliases_maxResults,
    listTemplateAliases_nextToken,
    listTemplateAliases_awsAccountId,
    listTemplateAliases_templateId,

    -- * Destructuring the Response
    ListTemplateAliasesResponse (..),
    newListTemplateAliasesResponse,

    -- * Response Lenses
    listTemplateAliasesResponse_nextToken,
    listTemplateAliasesResponse_requestId,
    listTemplateAliasesResponse_templateAliasList,
    listTemplateAliasesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplateAliases' smart constructor.
data ListTemplateAliases = ListTemplateAliases'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the template
    -- aliases that you\'re listing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTemplateAliases_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listTemplateAliases_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listTemplateAliases_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- aliases that you\'re listing.
--
-- 'templateId', 'listTemplateAliases_templateId' - The ID for the template.
newListTemplateAliases ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  ListTemplateAliases
newListTemplateAliases pAwsAccountId_ pTemplateId_ =
  ListTemplateAliases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      templateId = pTemplateId_
    }

-- | The maximum number of results to be returned per request.
listTemplateAliases_maxResults :: Lens.Lens' ListTemplateAliases (Prelude.Maybe Prelude.Natural)
listTemplateAliases_maxResults = Lens.lens (\ListTemplateAliases' {maxResults} -> maxResults) (\s@ListTemplateAliases' {} a -> s {maxResults = a} :: ListTemplateAliases)

-- | The token for the next set of results, or null if there are no more
-- results.
listTemplateAliases_nextToken :: Lens.Lens' ListTemplateAliases (Prelude.Maybe Prelude.Text)
listTemplateAliases_nextToken = Lens.lens (\ListTemplateAliases' {nextToken} -> nextToken) (\s@ListTemplateAliases' {} a -> s {nextToken = a} :: ListTemplateAliases)

-- | The ID of the Amazon Web Services account that contains the template
-- aliases that you\'re listing.
listTemplateAliases_awsAccountId :: Lens.Lens' ListTemplateAliases Prelude.Text
listTemplateAliases_awsAccountId = Lens.lens (\ListTemplateAliases' {awsAccountId} -> awsAccountId) (\s@ListTemplateAliases' {} a -> s {awsAccountId = a} :: ListTemplateAliases)

-- | The ID for the template.
listTemplateAliases_templateId :: Lens.Lens' ListTemplateAliases Prelude.Text
listTemplateAliases_templateId = Lens.lens (\ListTemplateAliases' {templateId} -> templateId) (\s@ListTemplateAliases' {} a -> s {templateId = a} :: ListTemplateAliases)

instance Core.AWSPager ListTemplateAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplateAliasesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTemplateAliasesResponse_templateAliasList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listTemplateAliases_nextToken
              Lens..~ rs
              Lens.^? listTemplateAliasesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListTemplateAliases where
  type
    AWSResponse ListTemplateAliases =
      ListTemplateAliasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateAliasesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x
                            Data..?> "TemplateAliasList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTemplateAliases where
  hashWithSalt _salt ListTemplateAliases' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData ListTemplateAliases where
  rnf ListTemplateAliases' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf awsAccountId `Prelude.seq`
          Prelude.rnf templateId

instance Data.ToHeaders ListTemplateAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTemplateAliases where
  toPath ListTemplateAliases' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId,
        "/aliases"
      ]

instance Data.ToQuery ListTemplateAliases where
  toQuery ListTemplateAliases' {..} =
    Prelude.mconcat
      [ "max-result" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListTemplateAliasesResponse' smart constructor.
data ListTemplateAliasesResponse = ListTemplateAliasesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure containing the list of the template\'s aliases.
    templateAliasList :: Prelude.Maybe [TemplateAlias],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateAliasesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listTemplateAliasesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateAliasList', 'listTemplateAliasesResponse_templateAliasList' - A structure containing the list of the template\'s aliases.
--
-- 'status', 'listTemplateAliasesResponse_status' - The HTTP status of the request.
newListTemplateAliasesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListTemplateAliasesResponse
newListTemplateAliasesResponse pStatus_ =
  ListTemplateAliasesResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateAliasList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listTemplateAliasesResponse_nextToken :: Lens.Lens' ListTemplateAliasesResponse (Prelude.Maybe Prelude.Text)
listTemplateAliasesResponse_nextToken = Lens.lens (\ListTemplateAliasesResponse' {nextToken} -> nextToken) (\s@ListTemplateAliasesResponse' {} a -> s {nextToken = a} :: ListTemplateAliasesResponse)

-- | The Amazon Web Services request ID for this operation.
listTemplateAliasesResponse_requestId :: Lens.Lens' ListTemplateAliasesResponse (Prelude.Maybe Prelude.Text)
listTemplateAliasesResponse_requestId = Lens.lens (\ListTemplateAliasesResponse' {requestId} -> requestId) (\s@ListTemplateAliasesResponse' {} a -> s {requestId = a} :: ListTemplateAliasesResponse)

-- | A structure containing the list of the template\'s aliases.
listTemplateAliasesResponse_templateAliasList :: Lens.Lens' ListTemplateAliasesResponse (Prelude.Maybe [TemplateAlias])
listTemplateAliasesResponse_templateAliasList = Lens.lens (\ListTemplateAliasesResponse' {templateAliasList} -> templateAliasList) (\s@ListTemplateAliasesResponse' {} a -> s {templateAliasList = a} :: ListTemplateAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listTemplateAliasesResponse_status :: Lens.Lens' ListTemplateAliasesResponse Prelude.Int
listTemplateAliasesResponse_status = Lens.lens (\ListTemplateAliasesResponse' {status} -> status) (\s@ListTemplateAliasesResponse' {} a -> s {status = a} :: ListTemplateAliasesResponse)

instance Prelude.NFData ListTemplateAliasesResponse where
  rnf ListTemplateAliasesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf templateAliasList `Prelude.seq`
          Prelude.rnf status
