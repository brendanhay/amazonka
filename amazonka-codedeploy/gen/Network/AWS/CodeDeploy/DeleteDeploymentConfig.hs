{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteDeploymentConfig@ operation.
--
-- /See:/ 'newDeleteDeploymentConfig' smart constructor.
data DeleteDeploymentConfig = DeleteDeploymentConfig'
  { -- | The name of a deployment configuration associated with the IAM user or
    -- AWS account.
    deploymentConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDeploymentConfig
newDeleteDeploymentConfig pDeploymentConfigName_ =
  DeleteDeploymentConfig'
    { deploymentConfigName =
        pDeploymentConfigName_
    }

-- | The name of a deployment configuration associated with the IAM user or
-- AWS account.
deleteDeploymentConfig_deploymentConfigName :: Lens.Lens' DeleteDeploymentConfig Prelude.Text
deleteDeploymentConfig_deploymentConfigName = Lens.lens (\DeleteDeploymentConfig' {deploymentConfigName} -> deploymentConfigName) (\s@DeleteDeploymentConfig' {} a -> s {deploymentConfigName = a} :: DeleteDeploymentConfig)

instance Prelude.AWSRequest DeleteDeploymentConfig where
  type
    Rs DeleteDeploymentConfig =
      DeleteDeploymentConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeleteDeploymentConfigResponse'

instance Prelude.Hashable DeleteDeploymentConfig

instance Prelude.NFData DeleteDeploymentConfig

instance Prelude.ToHeaders DeleteDeploymentConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.DeleteDeploymentConfig" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteDeploymentConfig where
  toJSON DeleteDeploymentConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "deploymentConfigName"
                  Prelude..= deploymentConfigName
              )
          ]
      )

instance Prelude.ToPath DeleteDeploymentConfig where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDeploymentConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeploymentConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeploymentConfigResponse ::
  DeleteDeploymentConfigResponse
newDeleteDeploymentConfigResponse =
  DeleteDeploymentConfigResponse'

instance
  Prelude.NFData
    DeleteDeploymentConfigResponse
