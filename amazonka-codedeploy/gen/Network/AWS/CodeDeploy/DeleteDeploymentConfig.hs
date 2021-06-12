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
-- Module      : Network.AWS.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment configuration.
--
-- A deployment configuration cannot be deleted if it is currently in use.
-- Predefined configurations cannot be deleted.
module Network.AWS.CodeDeploy.DeleteDeploymentConfig
  ( -- * Creating a Request
    DeleteDeploymentConfig (..),
    newDeleteDeploymentConfig,

    -- * Request Lenses
    deleteDeploymentConfig_deploymentConfigName,

    -- * Destructuring the Response
    DeleteDeploymentConfigResponse (..),
    newDeleteDeploymentConfigResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentConfig@ operation.
--
-- /See:/ 'newDeleteDeploymentConfig' smart constructor.
data DeleteDeploymentConfig = DeleteDeploymentConfig'
  { -- | The name of a deployment configuration associated with the IAM user or
    -- AWS account.
    deploymentConfigName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigName', 'deleteDeploymentConfig_deploymentConfigName' - The name of a deployment configuration associated with the IAM user or
-- AWS account.
newDeleteDeploymentConfig ::
  -- | 'deploymentConfigName'
  Core.Text ->
  DeleteDeploymentConfig
newDeleteDeploymentConfig pDeploymentConfigName_ =
  DeleteDeploymentConfig'
    { deploymentConfigName =
        pDeploymentConfigName_
    }

-- | The name of a deployment configuration associated with the IAM user or
-- AWS account.
deleteDeploymentConfig_deploymentConfigName :: Lens.Lens' DeleteDeploymentConfig Core.Text
deleteDeploymentConfig_deploymentConfigName = Lens.lens (\DeleteDeploymentConfig' {deploymentConfigName} -> deploymentConfigName) (\s@DeleteDeploymentConfig' {} a -> s {deploymentConfigName = a} :: DeleteDeploymentConfig)

instance Core.AWSRequest DeleteDeploymentConfig where
  type
    AWSResponse DeleteDeploymentConfig =
      DeleteDeploymentConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteDeploymentConfigResponse'

instance Core.Hashable DeleteDeploymentConfig

instance Core.NFData DeleteDeploymentConfig

instance Core.ToHeaders DeleteDeploymentConfig where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.DeleteDeploymentConfig" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDeploymentConfig where
  toJSON DeleteDeploymentConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "deploymentConfigName"
                  Core..= deploymentConfigName
              )
          ]
      )

instance Core.ToPath DeleteDeploymentConfig where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDeploymentConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeploymentConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeploymentConfigResponse ::
  DeleteDeploymentConfigResponse
newDeleteDeploymentConfigResponse =
  DeleteDeploymentConfigResponse'

instance Core.NFData DeleteDeploymentConfigResponse
