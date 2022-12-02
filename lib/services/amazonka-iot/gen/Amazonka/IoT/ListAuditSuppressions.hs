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
-- Module      : Amazonka.IoT.ListAuditSuppressions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Device Defender audit listings.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListAuditSuppressions>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListAuditSuppressions
  ( -- * Creating a Request
    ListAuditSuppressions (..),
    newListAuditSuppressions,

    -- * Request Lenses
    listAuditSuppressions_nextToken,
    listAuditSuppressions_checkName,
    listAuditSuppressions_resourceIdentifier,
    listAuditSuppressions_maxResults,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAuditSuppressions' smart constructor.
data ListAuditSuppressions = ListAuditSuppressions'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    checkName :: Prelude.Maybe Prelude.Text,
    resourceIdentifier :: Prelude.Maybe ResourceIdentifier,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'checkName', 'listAuditSuppressions_checkName' - Undocumented member.
--
-- 'resourceIdentifier', 'listAuditSuppressions_resourceIdentifier' - Undocumented member.
--
-- 'maxResults', 'listAuditSuppressions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'ascendingOrder', 'listAuditSuppressions_ascendingOrder' - Determines whether suppressions are listed in ascending order by
-- expiration date or not. If parameter isn\'t provided,
-- @ascendingOrder=true@.
newListAuditSuppressions ::
  ListAuditSuppressions
newListAuditSuppressions =
  ListAuditSuppressions'
    { nextToken = Prelude.Nothing,
      checkName = Prelude.Nothing,
      resourceIdentifier = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing
    }

-- | The token for the next set of results.
listAuditSuppressions_nextToken :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Text)
listAuditSuppressions_nextToken = Lens.lens (\ListAuditSuppressions' {nextToken} -> nextToken) (\s@ListAuditSuppressions' {} a -> s {nextToken = a} :: ListAuditSuppressions)

-- | Undocumented member.
listAuditSuppressions_checkName :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Text)
listAuditSuppressions_checkName = Lens.lens (\ListAuditSuppressions' {checkName} -> checkName) (\s@ListAuditSuppressions' {} a -> s {checkName = a} :: ListAuditSuppressions)

-- | Undocumented member.
listAuditSuppressions_resourceIdentifier :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe ResourceIdentifier)
listAuditSuppressions_resourceIdentifier = Lens.lens (\ListAuditSuppressions' {resourceIdentifier} -> resourceIdentifier) (\s@ListAuditSuppressions' {} a -> s {resourceIdentifier = a} :: ListAuditSuppressions)

-- | The maximum number of results to return at one time. The default is 25.
listAuditSuppressions_maxResults :: Lens.Lens' ListAuditSuppressions (Prelude.Maybe Prelude.Natural)
listAuditSuppressions_maxResults = Lens.lens (\ListAuditSuppressions' {maxResults} -> maxResults) (\s@ListAuditSuppressions' {} a -> s {maxResults = a} :: ListAuditSuppressions)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAuditSuppressionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "suppressions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAuditSuppressions where
  hashWithSalt _salt ListAuditSuppressions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` checkName
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` ascendingOrder

instance Prelude.NFData ListAuditSuppressions where
  rnf ListAuditSuppressions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf checkName
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf ascendingOrder

instance Data.ToHeaders ListAuditSuppressions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListAuditSuppressions where
  toJSON ListAuditSuppressions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("checkName" Data..=) Prelude.<$> checkName,
            ("resourceIdentifier" Data..=)
              Prelude.<$> resourceIdentifier,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("ascendingOrder" Data..=)
              Prelude.<$> ascendingOrder
          ]
      )

instance Data.ToPath ListAuditSuppressions where
  toPath = Prelude.const "/audit/suppressions/list"

instance Data.ToQuery ListAuditSuppressions where
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
listAuditSuppressionsResponse_suppressions = Lens.lens (\ListAuditSuppressionsResponse' {suppressions} -> suppressions) (\s@ListAuditSuppressionsResponse' {} a -> s {suppressions = a} :: ListAuditSuppressionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAuditSuppressionsResponse_httpStatus :: Lens.Lens' ListAuditSuppressionsResponse Prelude.Int
listAuditSuppressionsResponse_httpStatus = Lens.lens (\ListAuditSuppressionsResponse' {httpStatus} -> httpStatus) (\s@ListAuditSuppressionsResponse' {} a -> s {httpStatus = a} :: ListAuditSuppressionsResponse)

instance Prelude.NFData ListAuditSuppressionsResponse where
  rnf ListAuditSuppressionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf suppressions
      `Prelude.seq` Prelude.rnf httpStatus
