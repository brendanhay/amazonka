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
-- Module      : Amazonka.ApplicationInsights.ListLogPatterns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the log patterns in the specific log @LogPatternSet@.
module Amazonka.ApplicationInsights.ListLogPatterns
  ( -- * Creating a Request
    ListLogPatterns (..),
    newListLogPatterns,

    -- * Request Lenses
    listLogPatterns_maxResults,
    listLogPatterns_nextToken,
    listLogPatterns_patternSetName,
    listLogPatterns_resourceGroupName,

    -- * Destructuring the Response
    ListLogPatternsResponse (..),
    newListLogPatternsResponse,

    -- * Response Lenses
    listLogPatternsResponse_logPatterns,
    listLogPatternsResponse_nextToken,
    listLogPatternsResponse_resourceGroupName,
    listLogPatternsResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLogPatterns' smart constructor.
data ListLogPatterns = ListLogPatterns'
  { -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the log pattern set.
    patternSetName :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLogPatterns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLogPatterns_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
--
-- 'nextToken', 'listLogPatterns_nextToken' - The token to request the next page of results.
--
-- 'patternSetName', 'listLogPatterns_patternSetName' - The name of the log pattern set.
--
-- 'resourceGroupName', 'listLogPatterns_resourceGroupName' - The name of the resource group.
newListLogPatterns ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  ListLogPatterns
newListLogPatterns pResourceGroupName_ =
  ListLogPatterns'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      patternSetName = Prelude.Nothing,
      resourceGroupName = pResourceGroupName_
    }

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value.
listLogPatterns_maxResults :: Lens.Lens' ListLogPatterns (Prelude.Maybe Prelude.Natural)
listLogPatterns_maxResults = Lens.lens (\ListLogPatterns' {maxResults} -> maxResults) (\s@ListLogPatterns' {} a -> s {maxResults = a} :: ListLogPatterns)

-- | The token to request the next page of results.
listLogPatterns_nextToken :: Lens.Lens' ListLogPatterns (Prelude.Maybe Prelude.Text)
listLogPatterns_nextToken = Lens.lens (\ListLogPatterns' {nextToken} -> nextToken) (\s@ListLogPatterns' {} a -> s {nextToken = a} :: ListLogPatterns)

-- | The name of the log pattern set.
listLogPatterns_patternSetName :: Lens.Lens' ListLogPatterns (Prelude.Maybe Prelude.Text)
listLogPatterns_patternSetName = Lens.lens (\ListLogPatterns' {patternSetName} -> patternSetName) (\s@ListLogPatterns' {} a -> s {patternSetName = a} :: ListLogPatterns)

-- | The name of the resource group.
listLogPatterns_resourceGroupName :: Lens.Lens' ListLogPatterns Prelude.Text
listLogPatterns_resourceGroupName = Lens.lens (\ListLogPatterns' {resourceGroupName} -> resourceGroupName) (\s@ListLogPatterns' {} a -> s {resourceGroupName = a} :: ListLogPatterns)

instance Core.AWSRequest ListLogPatterns where
  type
    AWSResponse ListLogPatterns =
      ListLogPatternsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLogPatternsResponse'
            Prelude.<$> (x Data..?> "LogPatterns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResourceGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLogPatterns where
  hashWithSalt _salt ListLogPatterns' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` patternSetName
      `Prelude.hashWithSalt` resourceGroupName

instance Prelude.NFData ListLogPatterns where
  rnf ListLogPatterns' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf patternSetName
      `Prelude.seq` Prelude.rnf resourceGroupName

instance Data.ToHeaders ListLogPatterns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.ListLogPatterns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLogPatterns where
  toJSON ListLogPatterns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("PatternSetName" Data..=)
              Prelude.<$> patternSetName,
            Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName)
          ]
      )

instance Data.ToPath ListLogPatterns where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLogPatterns where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLogPatternsResponse' smart constructor.
data ListLogPatternsResponse = ListLogPatternsResponse'
  { -- | The list of log patterns.
    logPatterns :: Prelude.Maybe [LogPattern],
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
-- Create a value of 'ListLogPatternsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPatterns', 'listLogPatternsResponse_logPatterns' - The list of log patterns.
--
-- 'nextToken', 'listLogPatternsResponse_nextToken' - The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'resourceGroupName', 'listLogPatternsResponse_resourceGroupName' - The name of the resource group.
--
-- 'httpStatus', 'listLogPatternsResponse_httpStatus' - The response's http status code.
newListLogPatternsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLogPatternsResponse
newListLogPatternsResponse pHttpStatus_ =
  ListLogPatternsResponse'
    { logPatterns =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of log patterns.
listLogPatternsResponse_logPatterns :: Lens.Lens' ListLogPatternsResponse (Prelude.Maybe [LogPattern])
listLogPatternsResponse_logPatterns = Lens.lens (\ListLogPatternsResponse' {logPatterns} -> logPatterns) (\s@ListLogPatternsResponse' {} a -> s {logPatterns = a} :: ListLogPatternsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
listLogPatternsResponse_nextToken :: Lens.Lens' ListLogPatternsResponse (Prelude.Maybe Prelude.Text)
listLogPatternsResponse_nextToken = Lens.lens (\ListLogPatternsResponse' {nextToken} -> nextToken) (\s@ListLogPatternsResponse' {} a -> s {nextToken = a} :: ListLogPatternsResponse)

-- | The name of the resource group.
listLogPatternsResponse_resourceGroupName :: Lens.Lens' ListLogPatternsResponse (Prelude.Maybe Prelude.Text)
listLogPatternsResponse_resourceGroupName = Lens.lens (\ListLogPatternsResponse' {resourceGroupName} -> resourceGroupName) (\s@ListLogPatternsResponse' {} a -> s {resourceGroupName = a} :: ListLogPatternsResponse)

-- | The response's http status code.
listLogPatternsResponse_httpStatus :: Lens.Lens' ListLogPatternsResponse Prelude.Int
listLogPatternsResponse_httpStatus = Lens.lens (\ListLogPatternsResponse' {httpStatus} -> httpStatus) (\s@ListLogPatternsResponse' {} a -> s {httpStatus = a} :: ListLogPatternsResponse)

instance Prelude.NFData ListLogPatternsResponse where
  rnf ListLogPatternsResponse' {..} =
    Prelude.rnf logPatterns
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf httpStatus
