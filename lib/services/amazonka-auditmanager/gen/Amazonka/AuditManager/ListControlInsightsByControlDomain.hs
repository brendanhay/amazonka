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
-- Module      : Amazonka.AuditManager.ListControlInsightsByControlDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the latest analytics data for controls within a specific control
-- domain across all active assessments.
--
-- Control insights are listed only if the control belongs to the control
-- domain that was specified and the control collected evidence on the
-- @lastUpdated@ date of @controlInsightsMetadata@. If neither of these
-- conditions are met, no data is listed for that control.
module Amazonka.AuditManager.ListControlInsightsByControlDomain
  ( -- * Creating a Request
    ListControlInsightsByControlDomain (..),
    newListControlInsightsByControlDomain,

    -- * Request Lenses
    listControlInsightsByControlDomain_maxResults,
    listControlInsightsByControlDomain_nextToken,
    listControlInsightsByControlDomain_controlDomainId,

    -- * Destructuring the Response
    ListControlInsightsByControlDomainResponse (..),
    newListControlInsightsByControlDomainResponse,

    -- * Response Lenses
    listControlInsightsByControlDomainResponse_controlInsightsMetadata,
    listControlInsightsByControlDomainResponse_nextToken,
    listControlInsightsByControlDomainResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListControlInsightsByControlDomain' smart constructor.
data ListControlInsightsByControlDomain = ListControlInsightsByControlDomain'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the control domain.
    controlDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlInsightsByControlDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listControlInsightsByControlDomain_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'listControlInsightsByControlDomain_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'controlDomainId', 'listControlInsightsByControlDomain_controlDomainId' - The unique identifier for the control domain.
newListControlInsightsByControlDomain ::
  -- | 'controlDomainId'
  Prelude.Text ->
  ListControlInsightsByControlDomain
newListControlInsightsByControlDomain
  pControlDomainId_ =
    ListControlInsightsByControlDomain'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        controlDomainId = pControlDomainId_
      }

-- | Represents the maximum number of results on a page or for an API request
-- call.
listControlInsightsByControlDomain_maxResults :: Lens.Lens' ListControlInsightsByControlDomain (Prelude.Maybe Prelude.Natural)
listControlInsightsByControlDomain_maxResults = Lens.lens (\ListControlInsightsByControlDomain' {maxResults} -> maxResults) (\s@ListControlInsightsByControlDomain' {} a -> s {maxResults = a} :: ListControlInsightsByControlDomain)

-- | The pagination token that\'s used to fetch the next set of results.
listControlInsightsByControlDomain_nextToken :: Lens.Lens' ListControlInsightsByControlDomain (Prelude.Maybe Prelude.Text)
listControlInsightsByControlDomain_nextToken = Lens.lens (\ListControlInsightsByControlDomain' {nextToken} -> nextToken) (\s@ListControlInsightsByControlDomain' {} a -> s {nextToken = a} :: ListControlInsightsByControlDomain)

-- | The unique identifier for the control domain.
listControlInsightsByControlDomain_controlDomainId :: Lens.Lens' ListControlInsightsByControlDomain Prelude.Text
listControlInsightsByControlDomain_controlDomainId = Lens.lens (\ListControlInsightsByControlDomain' {controlDomainId} -> controlDomainId) (\s@ListControlInsightsByControlDomain' {} a -> s {controlDomainId = a} :: ListControlInsightsByControlDomain)

instance
  Core.AWSRequest
    ListControlInsightsByControlDomain
  where
  type
    AWSResponse ListControlInsightsByControlDomain =
      ListControlInsightsByControlDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListControlInsightsByControlDomainResponse'
            Prelude.<$> ( x
                            Data..?> "controlInsightsMetadata"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListControlInsightsByControlDomain
  where
  hashWithSalt
    _salt
    ListControlInsightsByControlDomain' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` controlDomainId

instance
  Prelude.NFData
    ListControlInsightsByControlDomain
  where
  rnf ListControlInsightsByControlDomain' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf controlDomainId

instance
  Data.ToHeaders
    ListControlInsightsByControlDomain
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListControlInsightsByControlDomain
  where
  toPath = Prelude.const "/insights/controls"

instance
  Data.ToQuery
    ListControlInsightsByControlDomain
  where
  toQuery ListControlInsightsByControlDomain' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "controlDomainId" Data.=: controlDomainId
      ]

-- | /See:/ 'newListControlInsightsByControlDomainResponse' smart constructor.
data ListControlInsightsByControlDomainResponse = ListControlInsightsByControlDomainResponse'
  { -- | The control analytics data that the @ListControlInsightsByControlDomain@
    -- API returned.
    controlInsightsMetadata :: Prelude.Maybe [ControlInsightsMetadataItem],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListControlInsightsByControlDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controlInsightsMetadata', 'listControlInsightsByControlDomainResponse_controlInsightsMetadata' - The control analytics data that the @ListControlInsightsByControlDomain@
-- API returned.
--
-- 'nextToken', 'listControlInsightsByControlDomainResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'listControlInsightsByControlDomainResponse_httpStatus' - The response's http status code.
newListControlInsightsByControlDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListControlInsightsByControlDomainResponse
newListControlInsightsByControlDomainResponse
  pHttpStatus_ =
    ListControlInsightsByControlDomainResponse'
      { controlInsightsMetadata =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The control analytics data that the @ListControlInsightsByControlDomain@
-- API returned.
listControlInsightsByControlDomainResponse_controlInsightsMetadata :: Lens.Lens' ListControlInsightsByControlDomainResponse (Prelude.Maybe [ControlInsightsMetadataItem])
listControlInsightsByControlDomainResponse_controlInsightsMetadata = Lens.lens (\ListControlInsightsByControlDomainResponse' {controlInsightsMetadata} -> controlInsightsMetadata) (\s@ListControlInsightsByControlDomainResponse' {} a -> s {controlInsightsMetadata = a} :: ListControlInsightsByControlDomainResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
listControlInsightsByControlDomainResponse_nextToken :: Lens.Lens' ListControlInsightsByControlDomainResponse (Prelude.Maybe Prelude.Text)
listControlInsightsByControlDomainResponse_nextToken = Lens.lens (\ListControlInsightsByControlDomainResponse' {nextToken} -> nextToken) (\s@ListControlInsightsByControlDomainResponse' {} a -> s {nextToken = a} :: ListControlInsightsByControlDomainResponse)

-- | The response's http status code.
listControlInsightsByControlDomainResponse_httpStatus :: Lens.Lens' ListControlInsightsByControlDomainResponse Prelude.Int
listControlInsightsByControlDomainResponse_httpStatus = Lens.lens (\ListControlInsightsByControlDomainResponse' {httpStatus} -> httpStatus) (\s@ListControlInsightsByControlDomainResponse' {} a -> s {httpStatus = a} :: ListControlInsightsByControlDomainResponse)

instance
  Prelude.NFData
    ListControlInsightsByControlDomainResponse
  where
  rnf ListControlInsightsByControlDomainResponse' {..} =
    Prelude.rnf controlInsightsMetadata
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
