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
-- Module      : Amazonka.ChimeSDKIdentity.ListAppInstanceUsers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all @AppInstanceUsers@ created under a single @AppInstance@.
module Amazonka.ChimeSDKIdentity.ListAppInstanceUsers
  ( -- * Creating a Request
    ListAppInstanceUsers (..),
    newListAppInstanceUsers,

    -- * Request Lenses
    listAppInstanceUsers_nextToken,
    listAppInstanceUsers_maxResults,
    listAppInstanceUsers_appInstanceArn,

    -- * Destructuring the Response
    ListAppInstanceUsersResponse (..),
    newListAppInstanceUsersResponse,

    -- * Response Lenses
    listAppInstanceUsersResponse_nextToken,
    listAppInstanceUsersResponse_appInstanceUsers,
    listAppInstanceUsersResponse_appInstanceArn,
    listAppInstanceUsersResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppInstanceUsers' smart constructor.
data ListAppInstanceUsers = ListAppInstanceUsers'
  { -- | The token passed by previous API calls until all requested users are
    -- returned.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of requests that you want returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstanceUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInstanceUsers_nextToken' - The token passed by previous API calls until all requested users are
-- returned.
--
-- 'maxResults', 'listAppInstanceUsers_maxResults' - The maximum number of requests that you want returned.
--
-- 'appInstanceArn', 'listAppInstanceUsers_appInstanceArn' - The ARN of the @AppInstance@.
newListAppInstanceUsers ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  ListAppInstanceUsers
newListAppInstanceUsers pAppInstanceArn_ =
  ListAppInstanceUsers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      appInstanceArn = pAppInstanceArn_
    }

-- | The token passed by previous API calls until all requested users are
-- returned.
listAppInstanceUsers_nextToken :: Lens.Lens' ListAppInstanceUsers (Prelude.Maybe Prelude.Text)
listAppInstanceUsers_nextToken = Lens.lens (\ListAppInstanceUsers' {nextToken} -> nextToken) (\s@ListAppInstanceUsers' {} a -> s {nextToken = a} :: ListAppInstanceUsers) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of requests that you want returned.
listAppInstanceUsers_maxResults :: Lens.Lens' ListAppInstanceUsers (Prelude.Maybe Prelude.Natural)
listAppInstanceUsers_maxResults = Lens.lens (\ListAppInstanceUsers' {maxResults} -> maxResults) (\s@ListAppInstanceUsers' {} a -> s {maxResults = a} :: ListAppInstanceUsers)

-- | The ARN of the @AppInstance@.
listAppInstanceUsers_appInstanceArn :: Lens.Lens' ListAppInstanceUsers Prelude.Text
listAppInstanceUsers_appInstanceArn = Lens.lens (\ListAppInstanceUsers' {appInstanceArn} -> appInstanceArn) (\s@ListAppInstanceUsers' {} a -> s {appInstanceArn = a} :: ListAppInstanceUsers)

instance Core.AWSRequest ListAppInstanceUsers where
  type
    AWSResponse ListAppInstanceUsers =
      ListAppInstanceUsersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppInstanceUsersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "AppInstanceUsers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "AppInstanceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppInstanceUsers where
  hashWithSalt _salt ListAppInstanceUsers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData ListAppInstanceUsers where
  rnf ListAppInstanceUsers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf appInstanceArn

instance Core.ToHeaders ListAppInstanceUsers where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAppInstanceUsers where
  toPath = Prelude.const "/app-instance-users"

instance Core.ToQuery ListAppInstanceUsers where
  toQuery ListAppInstanceUsers' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults,
        "app-instance-arn" Core.=: appInstanceArn
      ]

-- | /See:/ 'newListAppInstanceUsersResponse' smart constructor.
data ListAppInstanceUsersResponse = ListAppInstanceUsersResponse'
  { -- | The token passed by previous API calls until all requested users are
    -- returned.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The information for each requested @AppInstanceUser@.
    appInstanceUsers :: Prelude.Maybe [AppInstanceUserSummary],
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstanceUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInstanceUsersResponse_nextToken' - The token passed by previous API calls until all requested users are
-- returned.
--
-- 'appInstanceUsers', 'listAppInstanceUsersResponse_appInstanceUsers' - The information for each requested @AppInstanceUser@.
--
-- 'appInstanceArn', 'listAppInstanceUsersResponse_appInstanceArn' - The ARN of the @AppInstance@.
--
-- 'httpStatus', 'listAppInstanceUsersResponse_httpStatus' - The response's http status code.
newListAppInstanceUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppInstanceUsersResponse
newListAppInstanceUsersResponse pHttpStatus_ =
  ListAppInstanceUsersResponse'
    { nextToken =
        Prelude.Nothing,
      appInstanceUsers = Prelude.Nothing,
      appInstanceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token passed by previous API calls until all requested users are
-- returned.
listAppInstanceUsersResponse_nextToken :: Lens.Lens' ListAppInstanceUsersResponse (Prelude.Maybe Prelude.Text)
listAppInstanceUsersResponse_nextToken = Lens.lens (\ListAppInstanceUsersResponse' {nextToken} -> nextToken) (\s@ListAppInstanceUsersResponse' {} a -> s {nextToken = a} :: ListAppInstanceUsersResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The information for each requested @AppInstanceUser@.
listAppInstanceUsersResponse_appInstanceUsers :: Lens.Lens' ListAppInstanceUsersResponse (Prelude.Maybe [AppInstanceUserSummary])
listAppInstanceUsersResponse_appInstanceUsers = Lens.lens (\ListAppInstanceUsersResponse' {appInstanceUsers} -> appInstanceUsers) (\s@ListAppInstanceUsersResponse' {} a -> s {appInstanceUsers = a} :: ListAppInstanceUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the @AppInstance@.
listAppInstanceUsersResponse_appInstanceArn :: Lens.Lens' ListAppInstanceUsersResponse (Prelude.Maybe Prelude.Text)
listAppInstanceUsersResponse_appInstanceArn = Lens.lens (\ListAppInstanceUsersResponse' {appInstanceArn} -> appInstanceArn) (\s@ListAppInstanceUsersResponse' {} a -> s {appInstanceArn = a} :: ListAppInstanceUsersResponse)

-- | The response's http status code.
listAppInstanceUsersResponse_httpStatus :: Lens.Lens' ListAppInstanceUsersResponse Prelude.Int
listAppInstanceUsersResponse_httpStatus = Lens.lens (\ListAppInstanceUsersResponse' {httpStatus} -> httpStatus) (\s@ListAppInstanceUsersResponse' {} a -> s {httpStatus = a} :: ListAppInstanceUsersResponse)

instance Prelude.NFData ListAppInstanceUsersResponse where
  rnf ListAppInstanceUsersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appInstanceUsers
      `Prelude.seq` Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf httpStatus
