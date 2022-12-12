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
-- Module      : Amazonka.SMS.UpdateApp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Amazonka.SMS.UpdateApp
  ( -- * Creating a Request
    UpdateApp (..),
    newUpdateApp,

    -- * Request Lenses
    updateApp_appId,
    updateApp_description,
    updateApp_name,
    updateApp_roleName,
    updateApp_serverGroups,
    updateApp_tags,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newUpdateApp' smart constructor.
data UpdateApp = UpdateApp'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text,
    -- | The new description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name of the service role in the customer\'s account used by Server
    -- Migration Service.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The server groups in the application to update.
    serverGroups :: Prelude.Maybe [ServerGroup],
    -- | The tags to associate with the application.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'updateApp_description' - The new description of the application.
--
-- 'name', 'updateApp_name' - The new name of the application.
--
-- 'roleName', 'updateApp_roleName' - The name of the service role in the customer\'s account used by Server
-- Migration Service.
--
-- 'serverGroups', 'updateApp_serverGroups' - The server groups in the application to update.
--
-- 'tags', 'updateApp_tags' - The tags to associate with the application.
newUpdateApp ::
  UpdateApp
newUpdateApp =
  UpdateApp'
    { appId = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      roleName = Prelude.Nothing,
      serverGroups = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the application.
updateApp_appId :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_appId = Lens.lens (\UpdateApp' {appId} -> appId) (\s@UpdateApp' {} a -> s {appId = a} :: UpdateApp)

-- | The new description of the application.
updateApp_description :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_description = Lens.lens (\UpdateApp' {description} -> description) (\s@UpdateApp' {} a -> s {description = a} :: UpdateApp)

-- | The new name of the application.
updateApp_name :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_name = Lens.lens (\UpdateApp' {name} -> name) (\s@UpdateApp' {} a -> s {name = a} :: UpdateApp)

-- | The name of the service role in the customer\'s account used by Server
-- Migration Service.
updateApp_roleName :: Lens.Lens' UpdateApp (Prelude.Maybe Prelude.Text)
updateApp_roleName = Lens.lens (\UpdateApp' {roleName} -> roleName) (\s@UpdateApp' {} a -> s {roleName = a} :: UpdateApp)

-- | The server groups in the application to update.
updateApp_serverGroups :: Lens.Lens' UpdateApp (Prelude.Maybe [ServerGroup])
updateApp_serverGroups = Lens.lens (\UpdateApp' {serverGroups} -> serverGroups) (\s@UpdateApp' {} a -> s {serverGroups = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

-- | The tags to associate with the application.
updateApp_tags :: Lens.Lens' UpdateApp (Prelude.Maybe [Tag])
updateApp_tags = Lens.lens (\UpdateApp' {tags} -> tags) (\s@UpdateApp' {} a -> s {tags = a} :: UpdateApp) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest UpdateApp where
  type AWSResponse UpdateApp = UpdateAppResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAppResponse'
            Prelude.<$> (x Data..?> "appSummary")
            Prelude.<*> (x Data..?> "serverGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApp where
  hashWithSalt _salt UpdateApp' {..} =
    _salt `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` serverGroups
      `Prelude.hashWithSalt` tags

instance Prelude.NFData UpdateApp where
  rnf UpdateApp' {..} =
    Prelude.rnf appId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf serverGroups
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders UpdateApp where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.UpdateApp" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApp where
  toJSON UpdateApp' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appId" Data..=) Prelude.<$> appId,
            ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            ("roleName" Data..=) Prelude.<$> roleName,
            ("serverGroups" Data..=) Prelude.<$> serverGroups,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath UpdateApp where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApp where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAppResponse' smart constructor.
data UpdateAppResponse = UpdateAppResponse'
  { -- | A summary description of the application.
    appSummary :: Prelude.Maybe AppSummary,
    -- | The updated server groups in the application.
    serverGroups :: Prelude.Maybe [ServerGroup],
    -- | The tags associated with the application.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateAppResponse
newUpdateAppResponse pHttpStatus_ =
  UpdateAppResponse'
    { appSummary = Prelude.Nothing,
      serverGroups = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary description of the application.
updateAppResponse_appSummary :: Lens.Lens' UpdateAppResponse (Prelude.Maybe AppSummary)
updateAppResponse_appSummary = Lens.lens (\UpdateAppResponse' {appSummary} -> appSummary) (\s@UpdateAppResponse' {} a -> s {appSummary = a} :: UpdateAppResponse)

-- | The updated server groups in the application.
updateAppResponse_serverGroups :: Lens.Lens' UpdateAppResponse (Prelude.Maybe [ServerGroup])
updateAppResponse_serverGroups = Lens.lens (\UpdateAppResponse' {serverGroups} -> serverGroups) (\s@UpdateAppResponse' {} a -> s {serverGroups = a} :: UpdateAppResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags associated with the application.
updateAppResponse_tags :: Lens.Lens' UpdateAppResponse (Prelude.Maybe [Tag])
updateAppResponse_tags = Lens.lens (\UpdateAppResponse' {tags} -> tags) (\s@UpdateAppResponse' {} a -> s {tags = a} :: UpdateAppResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateAppResponse_httpStatus :: Lens.Lens' UpdateAppResponse Prelude.Int
updateAppResponse_httpStatus = Lens.lens (\UpdateAppResponse' {httpStatus} -> httpStatus) (\s@UpdateAppResponse' {} a -> s {httpStatus = a} :: UpdateAppResponse)

instance Prelude.NFData UpdateAppResponse where
  rnf UpdateAppResponse' {..} =
    Prelude.rnf appSummary
      `Prelude.seq` Prelude.rnf serverGroups
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
