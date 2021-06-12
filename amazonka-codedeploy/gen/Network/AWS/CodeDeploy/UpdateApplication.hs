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
-- Module      : Network.AWS.CodeDeploy.UpdateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the name of an application.
module Network.AWS.CodeDeploy.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_newApplicationName,
    updateApplication_applicationName,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an @UpdateApplication@ operation.
--
-- /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The new name to give the application.
    newApplicationName' :: Core.Maybe Core.Text,
    -- | The current name of the application you want to change.
    applicationName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newApplicationName'', 'updateApplication_newApplicationName' - The new name to give the application.
--
-- 'applicationName', 'updateApplication_applicationName' - The current name of the application you want to change.
newUpdateApplication ::
  UpdateApplication
newUpdateApplication =
  UpdateApplication'
    { newApplicationName' =
        Core.Nothing,
      applicationName = Core.Nothing
    }

-- | The new name to give the application.
updateApplication_newApplicationName :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
updateApplication_newApplicationName = Lens.lens (\UpdateApplication' {newApplicationName'} -> newApplicationName') (\s@UpdateApplication' {} a -> s {newApplicationName' = a} :: UpdateApplication)

-- | The current name of the application you want to change.
updateApplication_applicationName :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
updateApplication_applicationName = Lens.lens (\UpdateApplication' {applicationName} -> applicationName) (\s@UpdateApplication' {} a -> s {applicationName = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateApplicationResponse'

instance Core.Hashable UpdateApplication

instance Core.NFData UpdateApplication

instance Core.ToHeaders UpdateApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.UpdateApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ ("newApplicationName" Core..=)
              Core.<$> newApplicationName',
            ("applicationName" Core..=)
              Core.<$> applicationName
          ]
      )

instance Core.ToPath UpdateApplication where
  toPath = Core.const "/"

instance Core.ToQuery UpdateApplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateApplicationResponse ::
  UpdateApplicationResponse
newUpdateApplicationResponse =
  UpdateApplicationResponse'

instance Core.NFData UpdateApplicationResponse
