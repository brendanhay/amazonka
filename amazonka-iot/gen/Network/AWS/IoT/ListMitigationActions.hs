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
-- Module      : Network.AWS.IoT.ListMitigationActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all mitigation actions that match the specified filter
-- criteria.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListMitigationActions
  ( -- * Creating a Request
    ListMitigationActions (..),
    newListMitigationActions,

    -- * Request Lenses
    listMitigationActions_nextToken,
    listMitigationActions_maxResults,
    listMitigationActions_actionType,

    -- * Destructuring the Response
    ListMitigationActionsResponse (..),
    newListMitigationActionsResponse,

    -- * Response Lenses
    listMitigationActionsResponse_nextToken,
    listMitigationActionsResponse_actionIdentifiers,
    listMitigationActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListMitigationActions' smart constructor.
data ListMitigationActions = ListMitigationActions'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time. The default is 25.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify a value to limit the result to mitigation actions with a
    -- specific action type.
    actionType :: Prelude.Maybe MitigationActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMitigationActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMitigationActions_nextToken' - The token for the next set of results.
--
-- 'maxResults', 'listMitigationActions_maxResults' - The maximum number of results to return at one time. The default is 25.
--
-- 'actionType', 'listMitigationActions_actionType' - Specify a value to limit the result to mitigation actions with a
-- specific action type.
newListMitigationActions ::
  ListMitigationActions
newListMitigationActions =
  ListMitigationActions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      actionType = Prelude.Nothing
    }

-- | The token for the next set of results.
listMitigationActions_nextToken :: Lens.Lens' ListMitigationActions (Prelude.Maybe Prelude.Text)
listMitigationActions_nextToken = Lens.lens (\ListMitigationActions' {nextToken} -> nextToken) (\s@ListMitigationActions' {} a -> s {nextToken = a} :: ListMitigationActions)

-- | The maximum number of results to return at one time. The default is 25.
listMitigationActions_maxResults :: Lens.Lens' ListMitigationActions (Prelude.Maybe Prelude.Natural)
listMitigationActions_maxResults = Lens.lens (\ListMitigationActions' {maxResults} -> maxResults) (\s@ListMitigationActions' {} a -> s {maxResults = a} :: ListMitigationActions)

-- | Specify a value to limit the result to mitigation actions with a
-- specific action type.
listMitigationActions_actionType :: Lens.Lens' ListMitigationActions (Prelude.Maybe MitigationActionType)
listMitigationActions_actionType = Lens.lens (\ListMitigationActions' {actionType} -> actionType) (\s@ListMitigationActions' {} a -> s {actionType = a} :: ListMitigationActions)

instance Core.AWSPager ListMitigationActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listMitigationActionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listMitigationActionsResponse_actionIdentifiers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listMitigationActions_nextToken
          Lens..~ rs
          Lens.^? listMitigationActionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListMitigationActions where
  type
    AWSResponse ListMitigationActions =
      ListMitigationActionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMitigationActionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "actionIdentifiers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMitigationActions

instance Prelude.NFData ListMitigationActions

instance Core.ToHeaders ListMitigationActions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListMitigationActions where
  toPath = Prelude.const "/mitigationactions/actions"

instance Core.ToQuery ListMitigationActions where
  toQuery ListMitigationActions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "actionType" Core.=: actionType
      ]

-- | /See:/ 'newListMitigationActionsResponse' smart constructor.
data ListMitigationActionsResponse = ListMitigationActionsResponse'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A set of actions that matched the specified filter criteria.
    actionIdentifiers :: Prelude.Maybe [MitigationActionIdentifier],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMitigationActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMitigationActionsResponse_nextToken' - The token for the next set of results.
--
-- 'actionIdentifiers', 'listMitigationActionsResponse_actionIdentifiers' - A set of actions that matched the specified filter criteria.
--
-- 'httpStatus', 'listMitigationActionsResponse_httpStatus' - The response's http status code.
newListMitigationActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMitigationActionsResponse
newListMitigationActionsResponse pHttpStatus_ =
  ListMitigationActionsResponse'
    { nextToken =
        Prelude.Nothing,
      actionIdentifiers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results.
listMitigationActionsResponse_nextToken :: Lens.Lens' ListMitigationActionsResponse (Prelude.Maybe Prelude.Text)
listMitigationActionsResponse_nextToken = Lens.lens (\ListMitigationActionsResponse' {nextToken} -> nextToken) (\s@ListMitigationActionsResponse' {} a -> s {nextToken = a} :: ListMitigationActionsResponse)

-- | A set of actions that matched the specified filter criteria.
listMitigationActionsResponse_actionIdentifiers :: Lens.Lens' ListMitigationActionsResponse (Prelude.Maybe [MitigationActionIdentifier])
listMitigationActionsResponse_actionIdentifiers = Lens.lens (\ListMitigationActionsResponse' {actionIdentifiers} -> actionIdentifiers) (\s@ListMitigationActionsResponse' {} a -> s {actionIdentifiers = a} :: ListMitigationActionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMitigationActionsResponse_httpStatus :: Lens.Lens' ListMitigationActionsResponse Prelude.Int
listMitigationActionsResponse_httpStatus = Lens.lens (\ListMitigationActionsResponse' {httpStatus} -> httpStatus) (\s@ListMitigationActionsResponse' {} a -> s {httpStatus = a} :: ListMitigationActionsResponse)

instance Prelude.NFData ListMitigationActionsResponse
