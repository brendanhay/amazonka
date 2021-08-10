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
-- Module      : Network.AWS.IoT.ListAuditSuppressions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender audit listings.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListAuditSuppressions
  ( -- * Creating a Request
    ListAuditSuppressions (..),
    newListAuditSuppressions,

    -- * Request Lenses
    listAuditSuppressions_nextToken,
    listAuditSuppressions_maxResults,
    listAuditSuppressions_resourceIdentifier,
    listAuditSuppressions_checkName,
    listAuditSuppressions_ascendingOrder,

    -- * Destructuring the Response
    ListAuditSuppressionsResponse (..),
    newListAuditSuppressionsResponse,

    -- * Response Lenses
    listAuditSuppressionsResponse_nextToken,
    listAuditSuppressionsResponse_suppressions,
    listAuditSuppressionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAuditSuppressions' smart constructor.
data ListAuditSuppressions = ListAuditSuppressions'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier,
    checkName :: Prelude.Maybe Prelude.Text,
    -- | Determines whether suppressions are listed in ascending order by
    -- expiration date or not. If parameter isn\'t provided,
    -- @ascendingOrder=true@.
    ascendingOrder :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditSuppressions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditSuppressions_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listAuditSuppressions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'resourceIdentifier', 'listAuditSuppressions_resourceIdentifier' - Undocumented member.
--
-- 'checkName', 'listAuditSuppressions_checkName' - Undocumented member.
--
-- 'ascendingOrder', 'listAuditSuppressions_ascendingOrder' - Determines whether suppressions are listed in ascending order by
-- expiration date or not. If parameter isn\'t provided,
-- @ascendingOrder=true@.
newListAuditSuppressions ::
  ListAuditSuppressions
newListAuditSuppressions =
  ListAuditSuppressions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      checkName = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing
    }

-- | The token for the next set of results.
listAuditSuppressions_nextToken :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Text)
listAuditSuppressions_nextToken = Lens.lens (\ListAuditSuppressions' {nextToken} -> nextToken) (\s@ListAuditSuppressions' {} a -> s {nextToken = a} :: ListAuditSuppressions)

-- | The maximum number of results to return at one time. The default is 25.
listAuditSuppressions_maxResults :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Natural)
listAuditSuppressions_maxResults = Lens.lens (\ListAuditSuppressions' {maxResults} -> maxResults) (\s@ListAuditSuppressions' {} a -> s {maxResults = a} :: ListAuditSuppressions)

-- | Undocumented member.
listAuditSuppressions_resourceIdentifier :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe ResourceIdentifier)
listAuditSuppressions_resourceIdentifier = Lens.lens (\ListAuditSuppressions' {resourceIdentifier} -> resourceIdentifier) (\s@ListAuditSuppressions' {} a -> s {resourceIdentifier = a} :: ListAuditSuppressions)

-- | Undocumented member.
listAuditSuppressions_checkName :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Text)
listAuditSuppressions_checkName = Lens.lens (\ListAuditSuppressions' {checkName} -> checkName) (\s@ListAuditSuppressions' {} a -> s {checkName = a} :: ListAuditSuppressions)

-- | Determines whether suppressions are listed in ascending order by
-- expiration date or not. If parameter isn\'t provided,
-- @ascendingOrder=true@.
listAuditSuppressions_ascendingOrder :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Bool)
listAuditSuppressions_ascendingOrder = Lens.lens (\ListAuditSuppressions' {ascendingOrder} -> ascendingOrder) (\s@ListAuditSuppressions' {} a -> s {ascendingOrder = a} :: ListAuditSuppressions)

instance Core.AWSPager ListAuditSuppressions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAuditSuppressionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAuditSuppressionsResponse_suppressions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAuditSuppressions_nextToken
          Lens..~ rs
          Lens.^? listAuditSuppressionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAuditSuppressions where
  type
    AWSResponse ListAuditSuppressions =
      ListAuditSuppressionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditSuppressionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "suppressions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAuditSuppressions

instance Prelude.NFData ListAuditSuppressions

instance Core.ToHeaders ListAuditSuppressions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListAuditSuppressions where
  toJSON ListAuditSuppressions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("resourceIdentifier" Core..=)
              Prelude.<$> resourceIdentifier,
            ("checkName" Core..=) Prelude.<$> checkName,
            ("ascendingOrder" Core..=)
              Prelude.<$> ascendingOrder
          ]
      )

instance Core.ToPath ListAuditSuppressions where
  toPath = Prelude.const "/audit/suppressions/list"

instance Core.ToQuery ListAuditSuppressions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAuditSuppressionsResponse' smart constructor.
data ListAuditSuppressionsResponse = ListAuditSuppressionsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@
    -- if there are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of audit suppressions.
    suppressions :: Prelude.Maybe [AuditSuppression],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAuditSuppressionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAuditSuppressionsResponse_nextToken' - A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
--
-- 'suppressions', 'listAuditSuppressionsResponse_suppressions' - List of audit suppressions.
--
-- 'httpStatus', 'listAuditSuppressionsResponse_httpStatus' - The response's http status code.
newListAuditSuppressionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAuditSuppressionsResponse
newListAuditSuppressionsResponse pHttpStatus_ =
  ListAuditSuppressionsResponse'
    { nextToken =
        Prelude.Nothing,
      suppressions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token that can be used to retrieve the next set of results, or @null@
-- if there are no additional results.
listAuditSuppressionsResponse_nextToken :: Lens.Lens' ListAuditSuppressionsResponse (Prelude.Maybe Prelude.Text)
listAuditSuppressionsResponse_nextToken = Lens.lens (\ListAuditSuppressionsResponse' {nextToken} -> nextToken) (\s@ListAuditSuppressionsResponse' {} a -> s {nextToken = a} :: ListAuditSuppressionsResponse)

-- | List of audit suppressions.
listAuditSuppressionsResponse_suppressions :: Lens.Lens' ListAuditSuppressionsResponse (Prelude.Maybe [AuditSuppression])
listAuditSuppressionsResponse_suppressions = Lens.lens (\ListAuditSuppressionsResponse' {suppressions} -> suppressions) (\s@ListAuditSuppressionsResponse' {} a -> s {suppressions = a} :: ListAuditSuppressionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAuditSuppressionsResponse_httpStatus :: Lens.Lens' ListAuditSuppressionsResponse Prelude.Int
listAuditSuppressionsResponse_httpStatus = Lens.lens (\ListAuditSuppressionsResponse' {httpStatus} -> httpStatus) (\s@ListAuditSuppressionsResponse' {} a -> s {httpStatus = a} :: ListAuditSuppressionsResponse)

instance Prelude.NFData ListAuditSuppressionsResponse
