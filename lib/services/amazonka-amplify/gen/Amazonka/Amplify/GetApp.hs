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
-- Module      : Amazonka.Amplify.GetApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an existing Amplify app by appID.
module Amazonka.Amplify.GetApp
  ( -- * Creating a Request
    GetApp (..),
    newGetApp,

    -- * Request Lenses
    getApp_appId,

    -- * Destructuring the Response
    GetAppResponse (..),
    newGetAppResponse,

    -- * Response Lenses
    getAppResponse_httpStatus,
    getAppResponse_app,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request structure for the get app request.
--
-- /See:/ 'newGetApp' smart constructor.
data GetApp = GetApp'
  { -- | The unique ID for an Amplify app.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getApp_appId' - The unique ID for an Amplify app.
newGetApp ::
  -- | 'appId'
  Prelude.Text ->
  GetApp
newGetApp pAppId_ = GetApp' {appId = pAppId_}

-- | The unique ID for an Amplify app.
getApp_appId :: Lens.Lens' GetApp Prelude.Text
getApp_appId = Lens.lens (\GetApp' {appId} -> appId) (\s@GetApp' {} a -> s {appId = a} :: GetApp)

instance Core.AWSRequest GetApp where
  type AWSResponse GetApp = GetAppResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "app")
      )

instance Prelude.Hashable GetApp where
  hashWithSalt _salt GetApp' {..} =
    _salt `Prelude.hashWithSalt` appId

instance Prelude.NFData GetApp where
  rnf GetApp' {..} = Prelude.rnf appId

instance Core.ToHeaders GetApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApp where
  toPath GetApp' {..} =
    Prelude.mconcat ["/apps/", Core.toBS appId]

instance Core.ToQuery GetApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    app :: App
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAppResponse_httpStatus' - The response's http status code.
--
-- 'app', 'getAppResponse_app' - Undocumented member.
newGetAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'app'
  App ->
  GetAppResponse
newGetAppResponse pHttpStatus_ pApp_ =
  GetAppResponse'
    { httpStatus = pHttpStatus_,
      app = pApp_
    }

-- | The response's http status code.
getAppResponse_httpStatus :: Lens.Lens' GetAppResponse Prelude.Int
getAppResponse_httpStatus = Lens.lens (\GetAppResponse' {httpStatus} -> httpStatus) (\s@GetAppResponse' {} a -> s {httpStatus = a} :: GetAppResponse)

-- | Undocumented member.
getAppResponse_app :: Lens.Lens' GetAppResponse App
getAppResponse_app = Lens.lens (\GetAppResponse' {app} -> app) (\s@GetAppResponse' {} a -> s {app = a} :: GetAppResponse)

instance Prelude.NFData GetAppResponse where
  rnf GetAppResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf app
