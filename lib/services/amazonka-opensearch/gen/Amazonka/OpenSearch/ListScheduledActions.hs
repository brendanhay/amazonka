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
-- Module      : Amazonka.OpenSearch.ListScheduledActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of configuration changes that are scheduled for a
-- domain. These changes can be
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/service-software.html service software updates>
-- or
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html#auto-tune-types blue\/green Auto-Tune enhancements>.
module Amazonka.OpenSearch.ListScheduledActions
  ( -- * Creating a Request
    ListScheduledActions (..),
    newListScheduledActions,

    -- * Request Lenses
    listScheduledActions_maxResults,
    listScheduledActions_nextToken,
    listScheduledActions_domainName,

    -- * Destructuring the Response
    ListScheduledActionsResponse (..),
    newListScheduledActionsResponse,

    -- * Response Lenses
    listScheduledActionsResponse_nextToken,
    listScheduledActionsResponse_scheduledActions,
    listScheduledActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListScheduledActions' smart constructor.
data ListScheduledActions = ListScheduledActions'
  { -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @ListScheduledActions@ operation returns a @nextToken@,
    -- you can include the returned @nextToken@ in subsequent
    -- @ListScheduledActions@ operations, which returns results in the next
    -- page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduledActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listScheduledActions_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'listScheduledActions_nextToken' - If your initial @ListScheduledActions@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListScheduledActions@ operations, which returns results in the next
-- page.
--
-- 'domainName', 'listScheduledActions_domainName' - The name of the domain.
newListScheduledActions ::
  -- | 'domainName'
  Prelude.Text ->
  ListScheduledActions
newListScheduledActions pDomainName_ =
  ListScheduledActions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
listScheduledActions_maxResults :: Lens.Lens' ListScheduledActions (Prelude.Maybe Prelude.Int)
listScheduledActions_maxResults = Lens.lens (\ListScheduledActions' {maxResults} -> maxResults) (\s@ListScheduledActions' {} a -> s {maxResults = a} :: ListScheduledActions)

-- | If your initial @ListScheduledActions@ operation returns a @nextToken@,
-- you can include the returned @nextToken@ in subsequent
-- @ListScheduledActions@ operations, which returns results in the next
-- page.
listScheduledActions_nextToken :: Lens.Lens' ListScheduledActions (Prelude.Maybe Prelude.Text)
listScheduledActions_nextToken = Lens.lens (\ListScheduledActions' {nextToken} -> nextToken) (\s@ListScheduledActions' {} a -> s {nextToken = a} :: ListScheduledActions)

-- | The name of the domain.
listScheduledActions_domainName :: Lens.Lens' ListScheduledActions Prelude.Text
listScheduledActions_domainName = Lens.lens (\ListScheduledActions' {domainName} -> domainName) (\s@ListScheduledActions' {} a -> s {domainName = a} :: ListScheduledActions)

instance Core.AWSRequest ListScheduledActions where
  type
    AWSResponse ListScheduledActions =
      ListScheduledActionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScheduledActionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ScheduledActions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListScheduledActions where
  hashWithSalt _salt ListScheduledActions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListScheduledActions where
  rnf ListScheduledActions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders ListScheduledActions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListScheduledActions where
  toPath ListScheduledActions' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/scheduledActions"
      ]

instance Data.ToQuery ListScheduledActions where
  toQuery ListScheduledActions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListScheduledActionsResponse' smart constructor.
data ListScheduledActionsResponse = ListScheduledActionsResponse'
  { -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of actions that are scheduled for the domain.
    scheduledActions :: Prelude.Maybe [ScheduledAction],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduledActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScheduledActionsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'scheduledActions', 'listScheduledActionsResponse_scheduledActions' - A list of actions that are scheduled for the domain.
--
-- 'httpStatus', 'listScheduledActionsResponse_httpStatus' - The response's http status code.
newListScheduledActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListScheduledActionsResponse
newListScheduledActionsResponse pHttpStatus_ =
  ListScheduledActionsResponse'
    { nextToken =
        Prelude.Nothing,
      scheduledActions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
listScheduledActionsResponse_nextToken :: Lens.Lens' ListScheduledActionsResponse (Prelude.Maybe Prelude.Text)
listScheduledActionsResponse_nextToken = Lens.lens (\ListScheduledActionsResponse' {nextToken} -> nextToken) (\s@ListScheduledActionsResponse' {} a -> s {nextToken = a} :: ListScheduledActionsResponse)

-- | A list of actions that are scheduled for the domain.
listScheduledActionsResponse_scheduledActions :: Lens.Lens' ListScheduledActionsResponse (Prelude.Maybe [ScheduledAction])
listScheduledActionsResponse_scheduledActions = Lens.lens (\ListScheduledActionsResponse' {scheduledActions} -> scheduledActions) (\s@ListScheduledActionsResponse' {} a -> s {scheduledActions = a} :: ListScheduledActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listScheduledActionsResponse_httpStatus :: Lens.Lens' ListScheduledActionsResponse Prelude.Int
listScheduledActionsResponse_httpStatus = Lens.lens (\ListScheduledActionsResponse' {httpStatus} -> httpStatus) (\s@ListScheduledActionsResponse' {} a -> s {httpStatus = a} :: ListScheduledActionsResponse)

instance Prelude.NFData ListScheduledActionsResponse where
  rnf ListScheduledActionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf scheduledActions
      `Prelude.seq` Prelude.rnf httpStatus
