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
-- Module      : Amazonka.AuditManager.ListControlDomainInsights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the latest analytics data for control domains across all of your
-- active assessments.
--
-- A control domain is listed only if at least one of the controls within
-- that domain collected evidence on the @lastUpdated@ date of
-- @controlDomainInsights@. If this condition isn’t met, no data is listed
-- for that control domain.
module Amazonka.AuditManager.ListControlDomainInsights
  ( -- * Creating a Request
    ListControlDomainInsights (..),
    newListControlDomainInsights,

    -- * Request Lenses
    listControlDomainInsights_maxResults,
    listControlDomainInsights_nextToken,

    -- * Destructuring the Response
    ListControlDomainInsightsResponse (..),
    newListControlDomainInsightsResponse,

    -- * Response Lenses
    listControlDomainInsightsResponse_controlDomainInsights,
    listControlDomainInsightsResponse_nextToken,
    listControlDomainInsightsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListControlDomainInsights' smart constructor.
data ListControlDomainInsights = ListControlDomainInsights'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlDomainInsights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listControlDomainInsights_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'listControlDomainInsights_nextToken' - The pagination token that\'s used to fetch the next set of results.
newListControlDomainInsights ::
  ListControlDomainInsights
newListControlDomainInsights =
  ListControlDomainInsights'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Represents the maximum number of results on a page or for an API request
-- call.
listControlDomainInsights_maxResults :: Lens.Lens' ListControlDomainInsights (Prelude.Maybe Prelude.Natural)
listControlDomainInsights_maxResults = Lens.lens (\ListControlDomainInsights' {maxResults} -> maxResults) (\s@ListControlDomainInsights' {} a -> s {maxResults = a} :: ListControlDomainInsights)

-- | The pagination token that\'s used to fetch the next set of results.
listControlDomainInsights_nextToken :: Lens.Lens' ListControlDomainInsights (Prelude.Maybe Prelude.Text)
listControlDomainInsights_nextToken = Lens.lens (\ListControlDomainInsights' {nextToken} -> nextToken) (\s@ListControlDomainInsights' {} a -> s {nextToken = a} :: ListControlDomainInsights)

instance Core.AWSRequest ListControlDomainInsights where
  type
    AWSResponse ListControlDomainInsights =
      ListControlDomainInsightsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListControlDomainInsightsResponse'
            Prelude.<$> ( x Data..?> "controlDomainInsights"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListControlDomainInsights where
  hashWithSalt _salt ListControlDomainInsights' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListControlDomainInsights where
  rnf ListControlDomainInsights' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListControlDomainInsights where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListControlDomainInsights where
  toPath = Prelude.const "/insights/control-domains"

instance Data.ToQuery ListControlDomainInsights where
  toQuery ListControlDomainInsights' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListControlDomainInsightsResponse' smart constructor.
data ListControlDomainInsightsResponse = ListControlDomainInsightsResponse'
  { -- | The control domain analytics data that the @ListControlDomainInsights@
    -- API returned.
    controlDomainInsights :: Prelude.Maybe [ControlDomainInsights],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlDomainInsightsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlDomainInsights', 'listControlDomainInsightsResponse_controlDomainInsights' - The control domain analytics data that the @ListControlDomainInsights@
-- API returned.
--
-- 'nextToken', 'listControlDomainInsightsResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'listControlDomainInsightsResponse_httpStatus' - The response's http status code.
newListControlDomainInsightsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListControlDomainInsightsResponse
newListControlDomainInsightsResponse pHttpStatus_ =
  ListControlDomainInsightsResponse'
    { controlDomainInsights =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The control domain analytics data that the @ListControlDomainInsights@
-- API returned.
listControlDomainInsightsResponse_controlDomainInsights :: Lens.Lens' ListControlDomainInsightsResponse (Prelude.Maybe [ControlDomainInsights])
listControlDomainInsightsResponse_controlDomainInsights = Lens.lens (\ListControlDomainInsightsResponse' {controlDomainInsights} -> controlDomainInsights) (\s@ListControlDomainInsightsResponse' {} a -> s {controlDomainInsights = a} :: ListControlDomainInsightsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
listControlDomainInsightsResponse_nextToken :: Lens.Lens' ListControlDomainInsightsResponse (Prelude.Maybe Prelude.Text)
listControlDomainInsightsResponse_nextToken = Lens.lens (\ListControlDomainInsightsResponse' {nextToken} -> nextToken) (\s@ListControlDomainInsightsResponse' {} a -> s {nextToken = a} :: ListControlDomainInsightsResponse)

-- | The response's http status code.
listControlDomainInsightsResponse_httpStatus :: Lens.Lens' ListControlDomainInsightsResponse Prelude.Int
listControlDomainInsightsResponse_httpStatus = Lens.lens (\ListControlDomainInsightsResponse' {httpStatus} -> httpStatus) (\s@ListControlDomainInsightsResponse' {} a -> s {httpStatus = a} :: ListControlDomainInsightsResponse)

instance
  Prelude.NFData
    ListControlDomainInsightsResponse
  where
  rnf ListControlDomainInsightsResponse' {..} =
    Prelude.rnf controlDomainInsights
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
