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
-- Module      : Amazonka.QuickSight.ListThemeAliases
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the aliases of a theme.
module Amazonka.QuickSight.ListThemeAliases
  ( -- * Creating a Request
    ListThemeAliases (..),
    newListThemeAliases,

    -- * Request Lenses
    listThemeAliases_maxResults,
    listThemeAliases_nextToken,
    listThemeAliases_awsAccountId,
    listThemeAliases_themeId,

    -- * Destructuring the Response
    ListThemeAliasesResponse (..),
    newListThemeAliasesResponse,

    -- * Response Lenses
    listThemeAliasesResponse_nextToken,
    listThemeAliasesResponse_requestId,
    listThemeAliasesResponse_themeAliasList,
    listThemeAliasesResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThemeAliases' smart constructor.
data ListThemeAliases = ListThemeAliases'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that contains the theme
    -- aliases that you\'re listing.
    awsAccountId :: Prelude.Text,
    -- | The ID for the theme.
    themeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemeAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThemeAliases_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listThemeAliases_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listThemeAliases_awsAccountId' - The ID of the Amazon Web Services account that contains the theme
-- aliases that you\'re listing.
--
-- 'themeId', 'listThemeAliases_themeId' - The ID for the theme.
newListThemeAliases ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'themeId'
  Prelude.Text ->
  ListThemeAliases
newListThemeAliases pAwsAccountId_ pThemeId_ =
  ListThemeAliases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      themeId = pThemeId_
    }

-- | The maximum number of results to be returned per request.
listThemeAliases_maxResults :: Lens.Lens' ListThemeAliases (Prelude.Maybe Prelude.Natural)
listThemeAliases_maxResults = Lens.lens (\ListThemeAliases' {maxResults} -> maxResults) (\s@ListThemeAliases' {} a -> s {maxResults = a} :: ListThemeAliases)

-- | The token for the next set of results, or null if there are no more
-- results.
listThemeAliases_nextToken :: Lens.Lens' ListThemeAliases (Prelude.Maybe Prelude.Text)
listThemeAliases_nextToken = Lens.lens (\ListThemeAliases' {nextToken} -> nextToken) (\s@ListThemeAliases' {} a -> s {nextToken = a} :: ListThemeAliases)

-- | The ID of the Amazon Web Services account that contains the theme
-- aliases that you\'re listing.
listThemeAliases_awsAccountId :: Lens.Lens' ListThemeAliases Prelude.Text
listThemeAliases_awsAccountId = Lens.lens (\ListThemeAliases' {awsAccountId} -> awsAccountId) (\s@ListThemeAliases' {} a -> s {awsAccountId = a} :: ListThemeAliases)

-- | The ID for the theme.
listThemeAliases_themeId :: Lens.Lens' ListThemeAliases Prelude.Text
listThemeAliases_themeId = Lens.lens (\ListThemeAliases' {themeId} -> themeId) (\s@ListThemeAliases' {} a -> s {themeId = a} :: ListThemeAliases)

instance Core.AWSRequest ListThemeAliases where
  type
    AWSResponse ListThemeAliases =
      ListThemeAliasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThemeAliasesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "ThemeAliasList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListThemeAliases where
  hashWithSalt _salt ListThemeAliases' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData ListThemeAliases where
  rnf ListThemeAliases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf themeId

instance Data.ToHeaders ListThemeAliases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListThemeAliases where
  toPath ListThemeAliases' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/themes/",
        Data.toBS themeId,
        "/aliases"
      ]

instance Data.ToQuery ListThemeAliases where
  toQuery ListThemeAliases' {..} =
    Prelude.mconcat
      [ "max-result" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListThemeAliasesResponse' smart constructor.
data ListThemeAliasesResponse = ListThemeAliasesResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A structure containing the list of the theme\'s aliases.
    themeAliasList :: Prelude.Maybe [ThemeAlias],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemeAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThemeAliasesResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listThemeAliasesResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'themeAliasList', 'listThemeAliasesResponse_themeAliasList' - A structure containing the list of the theme\'s aliases.
--
-- 'status', 'listThemeAliasesResponse_status' - The HTTP status of the request.
newListThemeAliasesResponse ::
  -- | 'status'
  Prelude.Int ->
  ListThemeAliasesResponse
newListThemeAliasesResponse pStatus_ =
  ListThemeAliasesResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      themeAliasList = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listThemeAliasesResponse_nextToken :: Lens.Lens' ListThemeAliasesResponse (Prelude.Maybe Prelude.Text)
listThemeAliasesResponse_nextToken = Lens.lens (\ListThemeAliasesResponse' {nextToken} -> nextToken) (\s@ListThemeAliasesResponse' {} a -> s {nextToken = a} :: ListThemeAliasesResponse)

-- | The Amazon Web Services request ID for this operation.
listThemeAliasesResponse_requestId :: Lens.Lens' ListThemeAliasesResponse (Prelude.Maybe Prelude.Text)
listThemeAliasesResponse_requestId = Lens.lens (\ListThemeAliasesResponse' {requestId} -> requestId) (\s@ListThemeAliasesResponse' {} a -> s {requestId = a} :: ListThemeAliasesResponse)

-- | A structure containing the list of the theme\'s aliases.
listThemeAliasesResponse_themeAliasList :: Lens.Lens' ListThemeAliasesResponse (Prelude.Maybe [ThemeAlias])
listThemeAliasesResponse_themeAliasList = Lens.lens (\ListThemeAliasesResponse' {themeAliasList} -> themeAliasList) (\s@ListThemeAliasesResponse' {} a -> s {themeAliasList = a} :: ListThemeAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listThemeAliasesResponse_status :: Lens.Lens' ListThemeAliasesResponse Prelude.Int
listThemeAliasesResponse_status = Lens.lens (\ListThemeAliasesResponse' {status} -> status) (\s@ListThemeAliasesResponse' {} a -> s {status = a} :: ListThemeAliasesResponse)

instance Prelude.NFData ListThemeAliasesResponse where
  rnf ListThemeAliasesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf themeAliasList
      `Prelude.seq` Prelude.rnf status
