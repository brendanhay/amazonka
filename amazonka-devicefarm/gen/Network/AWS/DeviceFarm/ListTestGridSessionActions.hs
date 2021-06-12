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
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessionActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the actions taken in a TestGridSession.
module Network.AWS.DeviceFarm.ListTestGridSessionActions
  ( -- * Creating a Request
    ListTestGridSessionActions (..),
    newListTestGridSessionActions,

    -- * Request Lenses
    listTestGridSessionActions_nextToken,
    listTestGridSessionActions_maxResult,
    listTestGridSessionActions_sessionArn,

    -- * Destructuring the Response
    ListTestGridSessionActionsResponse (..),
    newListTestGridSessionActionsResponse,

    -- * Response Lenses
    listTestGridSessionActionsResponse_nextToken,
    listTestGridSessionActionsResponse_actions,
    listTestGridSessionActionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTestGridSessionActions' smart constructor.
data ListTestGridSessionActions = ListTestGridSessionActions'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of sessions to return per response.
    maxResult :: Core.Maybe Core.Natural,
    -- | The ARN of the session to retrieve.
    sessionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTestGridSessionActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestGridSessionActions_nextToken' - Pagination token.
--
-- 'maxResult', 'listTestGridSessionActions_maxResult' - The maximum number of sessions to return per response.
--
-- 'sessionArn', 'listTestGridSessionActions_sessionArn' - The ARN of the session to retrieve.
newListTestGridSessionActions ::
  -- | 'sessionArn'
  Core.Text ->
  ListTestGridSessionActions
newListTestGridSessionActions pSessionArn_ =
  ListTestGridSessionActions'
    { nextToken =
        Core.Nothing,
      maxResult = Core.Nothing,
      sessionArn = pSessionArn_
    }

-- | Pagination token.
listTestGridSessionActions_nextToken :: Lens.Lens' ListTestGridSessionActions (Core.Maybe Core.Text)
listTestGridSessionActions_nextToken = Lens.lens (\ListTestGridSessionActions' {nextToken} -> nextToken) (\s@ListTestGridSessionActions' {} a -> s {nextToken = a} :: ListTestGridSessionActions)

-- | The maximum number of sessions to return per response.
listTestGridSessionActions_maxResult :: Lens.Lens' ListTestGridSessionActions (Core.Maybe Core.Natural)
listTestGridSessionActions_maxResult = Lens.lens (\ListTestGridSessionActions' {maxResult} -> maxResult) (\s@ListTestGridSessionActions' {} a -> s {maxResult = a} :: ListTestGridSessionActions)

-- | The ARN of the session to retrieve.
listTestGridSessionActions_sessionArn :: Lens.Lens' ListTestGridSessionActions Core.Text
listTestGridSessionActions_sessionArn = Lens.lens (\ListTestGridSessionActions' {sessionArn} -> sessionArn) (\s@ListTestGridSessionActions' {} a -> s {sessionArn = a} :: ListTestGridSessionActions)

instance Core.AWSRequest ListTestGridSessionActions where
  type
    AWSResponse ListTestGridSessionActions =
      ListTestGridSessionActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionActionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "actions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListTestGridSessionActions

instance Core.NFData ListTestGridSessionActions

instance Core.ToHeaders ListTestGridSessionActions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTestGridSessionActions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListTestGridSessionActions where
  toJSON ListTestGridSessionActions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResult" Core..=) Core.<$> maxResult,
            Core.Just ("sessionArn" Core..= sessionArn)
          ]
      )

instance Core.ToPath ListTestGridSessionActions where
  toPath = Core.const "/"

instance Core.ToQuery ListTestGridSessionActions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListTestGridSessionActionsResponse' smart constructor.
data ListTestGridSessionActionsResponse = ListTestGridSessionActionsResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The action taken by the session.
    actions :: Core.Maybe [TestGridSessionAction],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListTestGridSessionActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTestGridSessionActionsResponse_nextToken' - Pagination token.
--
-- 'actions', 'listTestGridSessionActionsResponse_actions' - The action taken by the session.
--
-- 'httpStatus', 'listTestGridSessionActionsResponse_httpStatus' - The response's http status code.
newListTestGridSessionActionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListTestGridSessionActionsResponse
newListTestGridSessionActionsResponse pHttpStatus_ =
  ListTestGridSessionActionsResponse'
    { nextToken =
        Core.Nothing,
      actions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
listTestGridSessionActionsResponse_nextToken :: Lens.Lens' ListTestGridSessionActionsResponse (Core.Maybe Core.Text)
listTestGridSessionActionsResponse_nextToken = Lens.lens (\ListTestGridSessionActionsResponse' {nextToken} -> nextToken) (\s@ListTestGridSessionActionsResponse' {} a -> s {nextToken = a} :: ListTestGridSessionActionsResponse)

-- | The action taken by the session.
listTestGridSessionActionsResponse_actions :: Lens.Lens' ListTestGridSessionActionsResponse (Core.Maybe [TestGridSessionAction])
listTestGridSessionActionsResponse_actions = Lens.lens (\ListTestGridSessionActionsResponse' {actions} -> actions) (\s@ListTestGridSessionActionsResponse' {} a -> s {actions = a} :: ListTestGridSessionActionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTestGridSessionActionsResponse_httpStatus :: Lens.Lens' ListTestGridSessionActionsResponse Core.Int
listTestGridSessionActionsResponse_httpStatus = Lens.lens (\ListTestGridSessionActionsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionActionsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionActionsResponse)

instance
  Core.NFData
    ListTestGridSessionActionsResponse
