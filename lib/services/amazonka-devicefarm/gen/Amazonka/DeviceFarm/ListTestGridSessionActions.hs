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
-- Module      : Amazonka.DeviceFarm.ListTestGridSessionActions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the actions taken in a TestGridSession.
module Amazonka.DeviceFarm.ListTestGridSessionActions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTestGridSessionActions' smart constructor.
data ListTestGridSessionActions = ListTestGridSessionActions'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of sessions to return per response.
    maxResult :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the session to retrieve.
    sessionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListTestGridSessionActions
newListTestGridSessionActions pSessionArn_ =
  ListTestGridSessionActions'
    { nextToken =
        Prelude.Nothing,
      maxResult = Prelude.Nothing,
      sessionArn = pSessionArn_
    }

-- | Pagination token.
listTestGridSessionActions_nextToken :: Lens.Lens' ListTestGridSessionActions (Prelude.Maybe Prelude.Text)
listTestGridSessionActions_nextToken = Lens.lens (\ListTestGridSessionActions' {nextToken} -> nextToken) (\s@ListTestGridSessionActions' {} a -> s {nextToken = a} :: ListTestGridSessionActions)

-- | The maximum number of sessions to return per response.
listTestGridSessionActions_maxResult :: Lens.Lens' ListTestGridSessionActions (Prelude.Maybe Prelude.Natural)
listTestGridSessionActions_maxResult = Lens.lens (\ListTestGridSessionActions' {maxResult} -> maxResult) (\s@ListTestGridSessionActions' {} a -> s {maxResult = a} :: ListTestGridSessionActions)

-- | The ARN of the session to retrieve.
listTestGridSessionActions_sessionArn :: Lens.Lens' ListTestGridSessionActions Prelude.Text
listTestGridSessionActions_sessionArn = Lens.lens (\ListTestGridSessionActions' {sessionArn} -> sessionArn) (\s@ListTestGridSessionActions' {} a -> s {sessionArn = a} :: ListTestGridSessionActions)

instance Core.AWSRequest ListTestGridSessionActions where
  type
    AWSResponse ListTestGridSessionActions =
      ListTestGridSessionActionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTestGridSessionActionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "actions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTestGridSessionActions where
  hashWithSalt _salt ListTestGridSessionActions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResult
      `Prelude.hashWithSalt` sessionArn

instance Prelude.NFData ListTestGridSessionActions where
  rnf ListTestGridSessionActions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResult
      `Prelude.seq` Prelude.rnf sessionArn

instance Core.ToHeaders ListTestGridSessionActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListTestGridSessionActions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTestGridSessionActions where
  toJSON ListTestGridSessionActions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResult" Core..=) Prelude.<$> maxResult,
            Prelude.Just ("sessionArn" Core..= sessionArn)
          ]
      )

instance Core.ToPath ListTestGridSessionActions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTestGridSessionActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTestGridSessionActionsResponse' smart constructor.
data ListTestGridSessionActionsResponse = ListTestGridSessionActionsResponse'
  { -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The action taken by the session.
    actions :: Prelude.Maybe [TestGridSessionAction],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListTestGridSessionActionsResponse
newListTestGridSessionActionsResponse pHttpStatus_ =
  ListTestGridSessionActionsResponse'
    { nextToken =
        Prelude.Nothing,
      actions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Pagination token.
listTestGridSessionActionsResponse_nextToken :: Lens.Lens' ListTestGridSessionActionsResponse (Prelude.Maybe Prelude.Text)
listTestGridSessionActionsResponse_nextToken = Lens.lens (\ListTestGridSessionActionsResponse' {nextToken} -> nextToken) (\s@ListTestGridSessionActionsResponse' {} a -> s {nextToken = a} :: ListTestGridSessionActionsResponse)

-- | The action taken by the session.
listTestGridSessionActionsResponse_actions :: Lens.Lens' ListTestGridSessionActionsResponse (Prelude.Maybe [TestGridSessionAction])
listTestGridSessionActionsResponse_actions = Lens.lens (\ListTestGridSessionActionsResponse' {actions} -> actions) (\s@ListTestGridSessionActionsResponse' {} a -> s {actions = a} :: ListTestGridSessionActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTestGridSessionActionsResponse_httpStatus :: Lens.Lens' ListTestGridSessionActionsResponse Prelude.Int
listTestGridSessionActionsResponse_httpStatus = Lens.lens (\ListTestGridSessionActionsResponse' {httpStatus} -> httpStatus) (\s@ListTestGridSessionActionsResponse' {} a -> s {httpStatus = a} :: ListTestGridSessionActionsResponse)

instance
  Prelude.NFData
    ListTestGridSessionActionsResponse
  where
  rnf ListTestGridSessionActionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf httpStatus
