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
-- Module      : Network.AWS.SMS.GetApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about the specified application.
module Network.AWS.SMS.GetApp
  ( -- * Creating a Request
    GetApp (..),
    newGetApp,

    -- * Request Lenses
    getApp_appId,

    -- * Destructuring the Response
    GetAppResponse (..),
    newGetAppResponse,

    -- * Response Lenses
    getAppResponse_appSummary,
    getAppResponse_serverGroups,
    getAppResponse_tags,
    getAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newGetApp' smart constructor.
data GetApp = GetApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
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
-- 'appId', 'getApp_appId' - The ID of the application.
newGetApp ::
  GetApp
newGetApp = GetApp' {appId = Prelude.Nothing}

-- | The ID of the application.
getApp_appId :: Lens.Lens' GetApp (Prelude.Maybe Prelude.Text)
getApp_appId = Lens.lens (\GetApp' {appId} -> appId) (\s@GetApp' {} a -> s {appId = a} :: GetApp)

instance Core.AWSRequest GetApp where
  type AWSResponse GetApp = GetAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppResponse'
            Prelude.<$> (x Core..?> "appSummary")
            Prelude.<*> (x Core..?> "serverGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApp

instance Prelude.NFData GetApp

instance Core.ToHeaders GetApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetApp where
  toJSON GetApp' {..} =
    Core.object
      ( Prelude.catMaybes
          [("appId" Core..=) Prelude.<$> appId]
      )

instance Core.ToPath GetApp where
  toPath = Prelude.const "/"

instance Core.ToQuery GetApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { -- | Information about the application.
    appSummary :: Prelude.Maybe AppSummary,
    -- | The server groups that belong to the application.
    serverGroups :: Prelude.Maybe [ServerGroup],
    -- | The tags associated with the application.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appSummary', 'getAppResponse_appSummary' - Information about the application.
--
-- 'serverGroups', 'getAppResponse_serverGroups' - The server groups that belong to the application.
--
-- 'tags', 'getAppResponse_tags' - The tags associated with the application.
--
-- 'httpStatus', 'getAppResponse_httpStatus' - The response's http status code.
newGetAppResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppResponse
newGetAppResponse pHttpStatus_ =
  GetAppResponse'
    { appSummary = Prelude.Nothing,
      serverGroups = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
getAppResponse_appSummary :: Lens.Lens' GetAppResponse (Prelude.Maybe AppSummary)
getAppResponse_appSummary = Lens.lens (\GetAppResponse' {appSummary} -> appSummary) (\s@GetAppResponse' {} a -> s {appSummary = a} :: GetAppResponse)

-- | The server groups that belong to the application.
getAppResponse_serverGroups :: Lens.Lens' GetAppResponse (Prelude.Maybe [ServerGroup])
getAppResponse_serverGroups = Lens.lens (\GetAppResponse' {serverGroups} -> serverGroups) (\s@GetAppResponse' {} a -> s {serverGroups = a} :: GetAppResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags associated with the application.
getAppResponse_tags :: Lens.Lens' GetAppResponse (Prelude.Maybe [Tag])
getAppResponse_tags = Lens.lens (\GetAppResponse' {tags} -> tags) (\s@GetAppResponse' {} a -> s {tags = a} :: GetAppResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAppResponse_httpStatus :: Lens.Lens' GetAppResponse Prelude.Int
getAppResponse_httpStatus = Lens.lens (\GetAppResponse' {httpStatus} -> httpStatus) (\s@GetAppResponse' {} a -> s {httpStatus = a} :: GetAppResponse)

instance Prelude.NFData GetAppResponse
