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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newCreateApp' smart constructor.
data CreateApp = CreateApp'
  { -- | The name of the service role in the customer\'s account to be used by
    -- AWS SMS.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The name of the new application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The server groups to include in the application.
    serverGroups :: Prelude.Maybe [ServerGroup],
    -- | The tags to be associated with the application.
    tags :: Prelude.Maybe [Tag],
    -- | The description of the new application
    description :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of application creation.
    clientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { roleName = Prelude.Nothing,
      name = Prelude.Nothing,
      serverGroups = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      clientToken = Prelude.Nothing
    }

-- | The name of the service role in the customer\'s account to be used by
-- AWS SMS.
createApp_roleName :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_roleName = Lens.lens (\CreateApp' {roleName} -> roleName) (\s@CreateApp' {} a -> s {roleName = a} :: CreateApp)

-- | The name of the new application.
createApp_name :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_name = Lens.lens (\CreateApp' {name} -> name) (\s@CreateApp' {} a -> s {name = a} :: CreateApp)

-- | The server groups to include in the application.
createApp_serverGroups :: Lens.Lens' CreateApp (Prelude.Maybe [ServerGroup])
createApp_serverGroups = Lens.lens (\CreateApp' {serverGroups} -> serverGroups) (\s@CreateApp' {} a -> s {serverGroups = a} :: CreateApp) Prelude.. Lens.mapping Lens._Coerce

-- | The tags to be associated with the application.
createApp_tags :: Lens.Lens' CreateApp (Prelude.Maybe [Tag])
createApp_tags = Lens.lens (\CreateApp' {tags} -> tags) (\s@CreateApp' {} a -> s {tags = a} :: CreateApp) Prelude.. Lens.mapping Lens._Coerce

-- | The description of the new application
createApp_description :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_description = Lens.lens (\CreateApp' {description} -> description) (\s@CreateApp' {} a -> s {description = a} :: CreateApp)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of application creation.
createApp_clientToken :: Lens.Lens' CreateApp (Prelude.Maybe Prelude.Text)
createApp_clientToken = Lens.lens (\CreateApp' {clientToken} -> clientToken) (\s@CreateApp' {} a -> s {clientToken = a} :: CreateApp)

instance Core.AWSRequest CreateApp where
  type AWSResponse CreateApp = CreateAppResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppResponse'
            Prelude.<$> (x Core..?> "appSummary")
            Prelude.<*> (x Core..?> "serverGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApp

instance Prelude.NFData CreateApp

instance Core.ToHeaders CreateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.CreateApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApp where
  toJSON CreateApp' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("roleName" Core..=) Prelude.<$> roleName,
            ("name" Core..=) Prelude.<$> name,
            ("serverGroups" Core..=) Prelude.<$> serverGroups,
            ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description,
            ("clientToken" Core..=) Prelude.<$> clientToken
          ]
      )

instance Core.ToPath CreateApp where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { -- | A summary description of the application.
    appSummary :: Prelude.Maybe AppSummary,
    -- | The server groups included in the application.
    serverGroups :: Prelude.Maybe [ServerGroup],
    -- | The tags associated with the application.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateAppResponse
newCreateAppResponse pHttpStatus_ =
  CreateAppResponse'
    { appSummary = Prelude.Nothing,
      serverGroups = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary description of the application.
createAppResponse_appSummary :: Lens.Lens' CreateAppResponse (Prelude.Maybe AppSummary)
createAppResponse_appSummary = Lens.lens (\CreateAppResponse' {appSummary} -> appSummary) (\s@CreateAppResponse' {} a -> s {appSummary = a} :: CreateAppResponse)

-- | The server groups included in the application.
createAppResponse_serverGroups :: Lens.Lens' CreateAppResponse (Prelude.Maybe [ServerGroup])
createAppResponse_serverGroups = Lens.lens (\CreateAppResponse' {serverGroups} -> serverGroups) (\s@CreateAppResponse' {} a -> s {serverGroups = a} :: CreateAppResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The tags associated with the application.
createAppResponse_tags :: Lens.Lens' CreateAppResponse (Prelude.Maybe [Tag])
createAppResponse_tags = Lens.lens (\CreateAppResponse' {tags} -> tags) (\s@CreateAppResponse' {} a -> s {tags = a} :: CreateAppResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createAppResponse_httpStatus :: Lens.Lens' CreateAppResponse Prelude.Int
createAppResponse_httpStatus = Lens.lens (\CreateAppResponse' {httpStatus} -> httpStatus) (\s@CreateAppResponse' {} a -> s {httpStatus = a} :: CreateAppResponse)

instance Prelude.NFData CreateAppResponse
