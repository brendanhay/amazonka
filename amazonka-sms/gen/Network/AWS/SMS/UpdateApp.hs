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
-- Module      : Network.AWS.SMS.UpdateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.SMS.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_appId,
    updateApp_roleName,
    updateApp_name,
    updateApp_serverGroups,
    updateApp_tags,
    updateApp_description,

    -- * Destructuring the Response
    UpdateAppResponse (..),
    newUpdateAppResponse,

    -- * Response Lenses
    updateAppResponse_appSummary,
    updateAppResponse_serverGroups,
    updateAppResponse_tags,
    updateAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text,
    -- | The name of the service role in the customer\'s account used by AWS SMS.
    roleName :: Core.Maybe Core.Text,
    -- | The new name of the application.
    name :: Core.Maybe Core.Text,
    -- | The server groups in the application to update.
    serverGroups :: Core.Maybe [ServerGroup],
    -- | The tags to associate with the application.
    tags :: Core.Maybe [Tag],
    -- | The new description of the application.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'updateApp_appId' - The ID of the application.
--
-- 'roleName', 'updateApp_roleName' - The name of the service role in the customer\'s account used by AWS SMS.
--
-- 'name', 'updateApp_name' - The new name of the application.
--
-- 'serverGroups', 'updateApp_serverGroups' - The server groups in the application to update.
--
-- 'tags', 'updateApp_tags' - The tags to associate with the application.
--
-- 'description', 'updateApp_description' - The new description of the application.
newUpdateApp ::
  UpdateApp
newUpdateApp =
  UpdateApp'
    { appId = Core.Nothing,
      roleName = Core.Nothing,
      name = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing
    }

-- | The ID of the application.
updateApp_appId :: Lens.Lens' UpdateApp (Core.Maybe Core.Text)
updateApp_appId = Lens.lens (\UpdateApp' {appId} -> appId) (\s@UpdateApp' {} a -> s {appId = a} :: UpdateApp)

-- | The name of the service role in the customer\'s account used by AWS SMS.
updateApp_roleName :: Lens.Lens' UpdateApp (Core.Maybe Core.Text)
updateApp_roleName = Lens.lens (\UpdateApp' {roleName} -> roleName) (\s@UpdateApp' {} a -> s {roleName = a} :: UpdateApp)

-- | The new name of the application.
updateApp_name :: Lens.Lens' UpdateApp (Core.Maybe Core.Text)
updateApp_name = Lens.lens (\UpdateApp' {name} -> name) (\s@UpdateApp' {} a -> s {name = a} :: UpdateApp)

-- | The server groups in the application to update.
updateApp_serverGroups :: Lens.Lens' UpdateApp (Core.Maybe [ServerGroup])
updateApp_serverGroups = Lens.lens (\UpdateApp' {serverGroups} -> serverGroups) (\s@UpdateApp' {} a -> s {serverGroups = a} :: UpdateApp) Core.. Lens.mapping Lens._Coerce

-- | The tags to associate with the application.
updateApp_tags :: Lens.Lens' UpdateApp (Core.Maybe [Tag])
updateApp_tags = Lens.lens (\UpdateApp' {tags} -> tags) (\s@UpdateApp' {} a -> s {tags = a} :: UpdateApp) Core.. Lens.mapping Lens._Coerce

-- | The new description of the application.
updateApp_description :: Lens.Lens' UpdateApp (Core.Maybe Core.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Core.<$> (x Core..?> "appSummary")
            Core.<*> (x Core..?> "serverGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateApp

instance Core.NFData UpdateApp

instance Core.ToHeaders UpdateApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.UpdateApp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("roleName" Core..=) Core.<$> roleName,
            ("name" Core..=) Core.<$> name,
            ("serverGroups" Core..=) Core.<$> serverGroups,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateApp where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { -- | A summary description of the application.
    appSummary :: Core.Maybe AppSummary,
    -- | The updated server groups in the application.
    serverGroups :: Core.Maybe [ServerGroup],
    -- | The tags associated with the application.
    tags :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appSummary', 'updateAppResponse_appSummary' - A summary description of the application.
--
-- 'serverGroups', 'updateAppResponse_serverGroups' - The updated server groups in the application.
--
-- 'tags', 'updateAppResponse_tags' - The tags associated with the application.
--
-- 'httpStatus', 'updateAppResponse_httpStatus' - The response's http status code.
newUpdateAppResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateAppResponse
newUpdateAppResponse pHttpStatus_ =
  UpdateAppResponse'
    { appSummary = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary description of the application.
updateAppResponse_appSummary :: Lens.Lens' UpdateAppResponse (Core.Maybe AppSummary)
updateAppResponse_appSummary = Lens.lens (\UpdateAppResponse' {appSummary} -> appSummary) (\s@UpdateAppResponse' {} a -> s {appSummary = a} :: UpdateAppResponse)

-- | The updated server groups in the application.
updateAppResponse_serverGroups :: Lens.Lens' UpdateAppResponse (Core.Maybe [ServerGroup])
updateAppResponse_serverGroups = Lens.lens (\UpdateAppResponse' {serverGroups} -> serverGroups) (\s@UpdateAppResponse' {} a -> s {serverGroups = a} :: UpdateAppResponse) Core.. Lens.mapping Lens._Coerce

-- | The tags associated with the application.
updateAppResponse_tags :: Lens.Lens' UpdateAppResponse (Core.Maybe [Tag])
updateAppResponse_tags = Lens.lens (\UpdateAppResponse' {tags} -> tags) (\s@UpdateAppResponse' {} a -> s {tags = a} :: UpdateAppResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateAppResponse_httpStatus :: Lens.Lens' UpdateAppResponse Core.Int
updateAppResponse_httpStatus = Lens.lens (\UpdateAppResponse' {httpStatus} -> httpStatus) (\s@UpdateAppResponse' {} a -> s {httpStatus = a} :: UpdateAppResponse)

instance Core.NFData UpdateAppResponse
