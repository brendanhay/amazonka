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
-- Module      : Network.AWS.OpsWorks.DeleteApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified app.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DeleteApp
  ( -- * Creating a Request
    DeleteApp (..),
    newDeleteApp,

    -- * Request Lenses
    deleteApp_appId,

    -- * Destructuring the Response
    DeleteAppResponse (..),
    newDeleteAppResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApp' smart constructor.
data DeleteApp = DeleteApp'
  { -- | The app ID.
    appId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteApp_appId' - The app ID.
newDeleteApp ::
  -- | 'appId'
  Core.Text ->
  DeleteApp
newDeleteApp pAppId_ = DeleteApp' {appId = pAppId_}

-- | The app ID.
deleteApp_appId :: Lens.Lens' DeleteApp Core.Text
deleteApp_appId = Lens.lens (\DeleteApp' {appId} -> appId) (\s@DeleteApp' {} a -> s {appId = a} :: DeleteApp)

instance Core.AWSRequest DeleteApp where
  type AWSResponse DeleteApp = DeleteAppResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteAppResponse'

instance Core.Hashable DeleteApp

instance Core.NFData DeleteApp

instance Core.ToHeaders DeleteApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("OpsWorks_20130218.DeleteApp" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteApp where
  toJSON DeleteApp' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AppId" Core..= appId)])

instance Core.ToPath DeleteApp where
  toPath = Core.const "/"

instance Core.ToQuery DeleteApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAppResponse' smart constructor.
data DeleteAppResponse = DeleteAppResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAppResponse ::
  DeleteAppResponse
newDeleteAppResponse = DeleteAppResponse'

instance Core.NFData DeleteAppResponse
