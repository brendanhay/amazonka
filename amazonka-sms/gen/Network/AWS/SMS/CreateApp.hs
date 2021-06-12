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
-- Module      : Network.AWS.SMS.CreateApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application. An application consists of one or more server
-- groups. Each server group contain one or more servers.
module Network.AWS.SMS.CreateApp
  ( -- * Creating a Request
    CreateApp (..),
    newCreateApp,

    -- * Request Lenses
    createApp_roleName,
    createApp_name,
    createApp_serverGroups,
    createApp_tags,
    createApp_description,
    createApp_clientToken,

    -- * Destructuring the Response
    CreateAppResponse (..),
    newCreateAppResponse,

    -- * Response Lenses
    createAppResponse_appSummary,
    createAppResponse_serverGroups,
    createAppResponse_tags,
    createAppResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The name of the service role in the customer\'s account to be used by
    -- AWS SMS.
    roleName :: Core.Maybe Core.Text,
    -- | The name of the new application.
    name :: Core.Maybe Core.Text,
    -- | The server groups to include in the application.
    serverGroups :: Core.Maybe [ServerGroup],
    -- | The tags to be associated with the application.
    tags :: Core.Maybe [Tag],
    -- | The description of the new application
    description :: Core.Maybe Core.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of application creation.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleName', 'createApp_roleName' - The name of the service role in the customer\'s account to be used by
-- AWS SMS.
--
-- 'name', 'createApp_name' - The name of the new application.
--
-- 'serverGroups', 'createApp_serverGroups' - The server groups to include in the application.
--
-- 'tags', 'createApp_tags' - The tags to be associated with the application.
--
-- 'description', 'createApp_description' - The description of the new application
--
-- 'clientToken', 'createApp_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of application creation.
newCreateApp ::
  CreateApp
newCreateApp =
  CreateApp'
    { roleName = Core.Nothing,
      name = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The name of the service role in the customer\'s account to be used by
-- AWS SMS.
createApp_roleName :: Lens.Lens' CreateApp (Core.Maybe Core.Text)
createApp_roleName = Lens.lens (\CreateApp' {roleName} -> roleName) (\s@CreateApp' {} a -> s {roleName = a} :: CreateApp)

-- | The name of the new application.
createApp_name :: Lens.Lens' CreateApp (Core.Maybe Core.Text)
createApp_name = Lens.lens (\CreateApp' {name} -> name) (\s@CreateApp' {} a -> s {name = a} :: CreateApp)

-- | The server groups to include in the application.
createApp_serverGroups :: Lens.Lens' CreateApp (Core.Maybe [ServerGroup])
createApp_serverGroups = Lens.lens (\CreateApp' {serverGroups} -> serverGroups) (\s@CreateApp' {} a -> s {serverGroups = a} :: CreateApp) Core.. Lens.mapping Lens._Coerce

-- | The tags to be associated with the application.
createApp_tags :: Lens.Lens' CreateApp (Core.Maybe [Tag])
createApp_tags = Lens.lens (\CreateApp' {tags} -> tags) (\s@CreateApp' {} a -> s {tags = a} :: CreateApp) Core.. Lens.mapping Lens._Coerce

-- | The description of the new application
createApp_description :: Lens.Lens' CreateApp (Core.Maybe Core.Text)
createApp_description = Lens.lens (\CreateApp' {description} -> description) (\s@CreateApp' {} a -> s {description = a} :: CreateApp)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of application creation.
createApp_clientToken :: Lens.Lens' CreateApp (Core.Maybe Core.Text)
createApp_clientToken = Lens.lens (\CreateApp' {clientToken} -> clientToken) (\s@CreateApp' {} a -> s {clientToken = a} :: CreateApp)

instance Core.AWSRequest CreateApp where
  type AWSResponse CreateApp = CreateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Core.<$> (x Core..?> "appSummary")
            Core.<*> (x Core..?> "serverGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateApp

instance Core.NFData CreateApp

instance Core.ToHeaders CreateApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.CreateApp" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleName" Core..=) Core.<$> roleName,
            ("name" Core..=) Core.<$> name,
            ("serverGroups" Core..=) Core.<$> serverGroups,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("clientToken" Core..=) Core.<$> clientToken
          ]
      )

instance Core.ToPath CreateApp where
  toPath = Core.const "/"

instance Core.ToQuery CreateApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | A summary description of the application.
    appSummary :: Core.Maybe AppSummary,
    -- | The server groups included in the application.
    serverGroups :: Core.Maybe [ServerGroup],
    -- | The tags associated with the application.
    tags :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appSummary', 'createAppResponse_appSummary' - A summary description of the application.
--
-- 'serverGroups', 'createAppResponse_serverGroups' - The server groups included in the application.
--
-- 'tags', 'createAppResponse_tags' - The tags associated with the application.
--
-- 'httpStatus', 'createAppResponse_httpStatus' - The response's http status code.
newCreateAppResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateAppResponse
newCreateAppResponse pHttpStatus_ =
  CreateAppResponse'
    { appSummary = Core.Nothing,
      serverGroups = Core.Nothing,
      tags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary description of the application.
createAppResponse_appSummary :: Lens.Lens' CreateAppResponse (Core.Maybe AppSummary)
createAppResponse_appSummary = Lens.lens (\CreateAppResponse' {appSummary} -> appSummary) (\s@CreateAppResponse' {} a -> s {appSummary = a} :: CreateAppResponse)

-- | The server groups included in the application.
createAppResponse_serverGroups :: Lens.Lens' CreateAppResponse (Core.Maybe [ServerGroup])
createAppResponse_serverGroups = Lens.lens (\CreateAppResponse' {serverGroups} -> serverGroups) (\s@CreateAppResponse' {} a -> s {serverGroups = a} :: CreateAppResponse) Core.. Lens.mapping Lens._Coerce

-- | The tags associated with the application.
createAppResponse_tags :: Lens.Lens' CreateAppResponse (Core.Maybe [Tag])
createAppResponse_tags = Lens.lens (\CreateAppResponse' {tags} -> tags) (\s@CreateAppResponse' {} a -> s {tags = a} :: CreateAppResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Core.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

instance Core.NFData CreateAppResponse
