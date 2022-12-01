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
-- Module      : Amazonka.ChimeSDKIdentity.ListAppInstanceAdmins
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the administrators in the @AppInstance@.
module Amazonka.ChimeSDKIdentity.ListAppInstanceAdmins
  ( -- * Creating a Request
    ListAppInstanceAdmins (..),
    newListAppInstanceAdmins,

    -- * Request Lenses
    listAppInstanceAdmins_nextToken,
    listAppInstanceAdmins_maxResults,
    listAppInstanceAdmins_appInstanceArn,

    -- * Destructuring the Response
    ListAppInstanceAdminsResponse (..),
    newListAppInstanceAdminsResponse,

    -- * Response Lenses
    listAppInstanceAdminsResponse_nextToken,
    listAppInstanceAdminsResponse_appInstanceArn,
    listAppInstanceAdminsResponse_appInstanceAdmins,
    listAppInstanceAdminsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppInstanceAdmins' smart constructor.
data ListAppInstanceAdmins = ListAppInstanceAdmins'
  { -- | The token returned from previous API requests until the number of
    -- administrators is reached.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of administrators that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstanceAdmins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInstanceAdmins_nextToken' - The token returned from previous API requests until the number of
-- administrators is reached.
--
-- 'maxResults', 'listAppInstanceAdmins_maxResults' - The maximum number of administrators that you want to return.
--
-- 'appInstanceArn', 'listAppInstanceAdmins_appInstanceArn' - The ARN of the @AppInstance@.
newListAppInstanceAdmins ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  ListAppInstanceAdmins
newListAppInstanceAdmins pAppInstanceArn_ =
  ListAppInstanceAdmins'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      appInstanceArn = pAppInstanceArn_
    }

-- | The token returned from previous API requests until the number of
-- administrators is reached.
listAppInstanceAdmins_nextToken :: Lens.Lens' ListAppInstanceAdmins (Prelude.Maybe Prelude.Text)
listAppInstanceAdmins_nextToken = Lens.lens (\ListAppInstanceAdmins' {nextToken} -> nextToken) (\s@ListAppInstanceAdmins' {} a -> s {nextToken = a} :: ListAppInstanceAdmins) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of administrators that you want to return.
listAppInstanceAdmins_maxResults :: Lens.Lens' ListAppInstanceAdmins (Prelude.Maybe Prelude.Natural)
listAppInstanceAdmins_maxResults = Lens.lens (\ListAppInstanceAdmins' {maxResults} -> maxResults) (\s@ListAppInstanceAdmins' {} a -> s {maxResults = a} :: ListAppInstanceAdmins)

-- | The ARN of the @AppInstance@.
listAppInstanceAdmins_appInstanceArn :: Lens.Lens' ListAppInstanceAdmins Prelude.Text
listAppInstanceAdmins_appInstanceArn = Lens.lens (\ListAppInstanceAdmins' {appInstanceArn} -> appInstanceArn) (\s@ListAppInstanceAdmins' {} a -> s {appInstanceArn = a} :: ListAppInstanceAdmins)

instance Core.AWSRequest ListAppInstanceAdmins where
  type
    AWSResponse ListAppInstanceAdmins =
      ListAppInstanceAdminsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppInstanceAdminsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AppInstanceArn")
            Prelude.<*> ( x Core..?> "AppInstanceAdmins"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppInstanceAdmins where
  hashWithSalt _salt ListAppInstanceAdmins' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData ListAppInstanceAdmins where
  rnf ListAppInstanceAdmins' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf appInstanceArn

instance Core.ToHeaders ListAppInstanceAdmins where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAppInstanceAdmins where
  toPath ListAppInstanceAdmins' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Core.toBS appInstanceArn,
        "/admins"
      ]

instance Core.ToQuery ListAppInstanceAdmins where
  toQuery ListAppInstanceAdmins' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListAppInstanceAdminsResponse' smart constructor.
data ListAppInstanceAdminsResponse = ListAppInstanceAdminsResponse'
  { -- | The token returned from previous API requests until the number of
    -- administrators is reached.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The information for each administrator.
    appInstanceAdmins :: Prelude.Maybe [AppInstanceAdminSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstanceAdminsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInstanceAdminsResponse_nextToken' - The token returned from previous API requests until the number of
-- administrators is reached.
--
-- 'appInstanceArn', 'listAppInstanceAdminsResponse_appInstanceArn' - The ARN of the @AppInstance@.
--
-- 'appInstanceAdmins', 'listAppInstanceAdminsResponse_appInstanceAdmins' - The information for each administrator.
--
-- 'httpStatus', 'listAppInstanceAdminsResponse_httpStatus' - The response's http status code.
newListAppInstanceAdminsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppInstanceAdminsResponse
newListAppInstanceAdminsResponse pHttpStatus_ =
  ListAppInstanceAdminsResponse'
    { nextToken =
        Prelude.Nothing,
      appInstanceArn = Prelude.Nothing,
      appInstanceAdmins = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned from previous API requests until the number of
-- administrators is reached.
listAppInstanceAdminsResponse_nextToken :: Lens.Lens' ListAppInstanceAdminsResponse (Prelude.Maybe Prelude.Text)
listAppInstanceAdminsResponse_nextToken = Lens.lens (\ListAppInstanceAdminsResponse' {nextToken} -> nextToken) (\s@ListAppInstanceAdminsResponse' {} a -> s {nextToken = a} :: ListAppInstanceAdminsResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The ARN of the @AppInstance@.
listAppInstanceAdminsResponse_appInstanceArn :: Lens.Lens' ListAppInstanceAdminsResponse (Prelude.Maybe Prelude.Text)
listAppInstanceAdminsResponse_appInstanceArn = Lens.lens (\ListAppInstanceAdminsResponse' {appInstanceArn} -> appInstanceArn) (\s@ListAppInstanceAdminsResponse' {} a -> s {appInstanceArn = a} :: ListAppInstanceAdminsResponse)

-- | The information for each administrator.
listAppInstanceAdminsResponse_appInstanceAdmins :: Lens.Lens' ListAppInstanceAdminsResponse (Prelude.Maybe [AppInstanceAdminSummary])
listAppInstanceAdminsResponse_appInstanceAdmins = Lens.lens (\ListAppInstanceAdminsResponse' {appInstanceAdmins} -> appInstanceAdmins) (\s@ListAppInstanceAdminsResponse' {} a -> s {appInstanceAdmins = a} :: ListAppInstanceAdminsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAppInstanceAdminsResponse_httpStatus :: Lens.Lens' ListAppInstanceAdminsResponse Prelude.Int
listAppInstanceAdminsResponse_httpStatus = Lens.lens (\ListAppInstanceAdminsResponse' {httpStatus} -> httpStatus) (\s@ListAppInstanceAdminsResponse' {} a -> s {httpStatus = a} :: ListAppInstanceAdminsResponse)

instance Prelude.NFData ListAppInstanceAdminsResponse where
  rnf ListAppInstanceAdminsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf appInstanceAdmins
      `Prelude.seq` Prelude.rnf httpStatus
