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
-- Module      : Amazonka.ApplicationInsights.ListLogPatternSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log pattern sets in the specific application.
module Amazonka.ApplicationInsights.ListLogPatternSets
  ( -- * Creating a Request
    ListLogPatternSets (..),
    newListLogPatternSets,

    -- * Request Lenses
    listLogPatternSets_maxResults,
    listLogPatternSets_nextToken,
    listLogPatternSets_resourceGroupName,

    -- * Destructuring the Response
    ListLogPatternSetsResponse (..),
    newListLogPatternSetsResponse,

    -- * Response Lenses
    listLogPatternSetsResponse_logPatternSets,
    listLogPatternSetsResponse_nextToken,
    listLogPatternSetsResponse_resourceGroupName,
    listLogPatternSetsResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLogPatternSets' smart constructor.
data ListLogPatternSets = ListLogPatternSets'
  { -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogPatternSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLogPatternSets_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'listLogPatternSets_nextToken' - The token to request the next page of results.
--
-- 'resourceGroupName', 'listLogPatternSets_resourceGroupName' - The name of the resource group.
newListLogPatternSets ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  ListLogPatternSets
newListLogPatternSets pResourceGroupName_ =
  ListLogPatternSets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceGroupName = pResourceGroupName_
    }

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
listLogPatternSets_maxResults :: Lens.Lens' ListLogPatternSets (Prelude.Maybe Prelude.Natural)
listLogPatternSets_maxResults = Lens.lens (\ListLogPatternSets' {maxResults} -> maxResults) (\s@ListLogPatternSets' {} a -> s {maxResults = a} :: ListLogPatternSets)

-- | The token to request the next page of results.
listLogPatternSets_nextToken :: Lens.Lens' ListLogPatternSets (Prelude.Maybe Prelude.Text)
listLogPatternSets_nextToken = Lens.lens (\ListLogPatternSets' {nextToken} -> nextToken) (\s@ListLogPatternSets' {} a -> s {nextToken = a} :: ListLogPatternSets)

-- | The name of the resource group.
listLogPatternSets_resourceGroupName :: Lens.Lens' ListLogPatternSets Prelude.Text
listLogPatternSets_resourceGroupName = Lens.lens (\ListLogPatternSets' {resourceGroupName} -> resourceGroupName) (\s@ListLogPatternSets' {} a -> s {resourceGroupName = a} :: ListLogPatternSets)

instance Core.AWSRequest ListLogPatternSets where
  type
    AWSResponse ListLogPatternSets =
      ListLogPatternSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogPatternSetsResponse'
            Prelude.<$> (x Data..?> "LogPatternSets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResourceGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLogPatternSets where
  hashWithSalt _salt ListLogPatternSets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceGroupName

instance Prelude.NFData ListLogPatternSets where
  rnf ListLogPatternSets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceGroupName

instance Data.ToHeaders ListLogPatternSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.ListLogPatternSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLogPatternSets where
  toJSON ListLogPatternSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName)
          ]
      )

instance Data.ToPath ListLogPatternSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLogPatternSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLogPatternSetsResponse' smart constructor.
data ListLogPatternSetsResponse = ListLogPatternSetsResponse'
  { -- | The list of log pattern sets.
    logPatternSets :: Prelude.Maybe [Prelude.Text],
    -- | The token used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogPatternSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPatternSets', 'listLogPatternSetsResponse_logPatternSets' - The list of log pattern sets.
--
-- 'nextToken', 'listLogPatternSetsResponse_nextToken' - The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'resourceGroupName', 'listLogPatternSetsResponse_resourceGroupName' - The name of the resource group.
--
-- 'httpStatus', 'listLogPatternSetsResponse_httpStatus' - The response's http status code.
newListLogPatternSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLogPatternSetsResponse
newListLogPatternSetsResponse pHttpStatus_ =
  ListLogPatternSetsResponse'
    { logPatternSets =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of log pattern sets.
listLogPatternSetsResponse_logPatternSets :: Lens.Lens' ListLogPatternSetsResponse (Prelude.Maybe [Prelude.Text])
listLogPatternSetsResponse_logPatternSets = Lens.lens (\ListLogPatternSetsResponse' {logPatternSets} -> logPatternSets) (\s@ListLogPatternSetsResponse' {} a -> s {logPatternSets = a} :: ListLogPatternSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listLogPatternSetsResponse_nextToken :: Lens.Lens' ListLogPatternSetsResponse (Prelude.Maybe Prelude.Text)
listLogPatternSetsResponse_nextToken = Lens.lens (\ListLogPatternSetsResponse' {nextToken} -> nextToken) (\s@ListLogPatternSetsResponse' {} a -> s {nextToken = a} :: ListLogPatternSetsResponse)

-- | The name of the resource group.
listLogPatternSetsResponse_resourceGroupName :: Lens.Lens' ListLogPatternSetsResponse (Prelude.Maybe Prelude.Text)
listLogPatternSetsResponse_resourceGroupName = Lens.lens (\ListLogPatternSetsResponse' {resourceGroupName} -> resourceGroupName) (\s@ListLogPatternSetsResponse' {} a -> s {resourceGroupName = a} :: ListLogPatternSetsResponse)

-- | The response's http status code.
listLogPatternSetsResponse_httpStatus :: Lens.Lens' ListLogPatternSetsResponse Prelude.Int
listLogPatternSetsResponse_httpStatus = Lens.lens (\ListLogPatternSetsResponse' {httpStatus} -> httpStatus) (\s@ListLogPatternSetsResponse' {} a -> s {httpStatus = a} :: ListLogPatternSetsResponse)

instance Prelude.NFData ListLogPatternSetsResponse where
  rnf ListLogPatternSetsResponse' {..} =
    Prelude.rnf logPatternSets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf httpStatus
