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
-- Module      : Amazonka.CodeDeploy.DeleteDeploymentConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a deployment configuration.
--
-- A deployment configuration cannot be deleted if it is currently in use.
-- Predefined configurations cannot be deleted.
module Amazonka.CodeDeploy.DeleteDeploymentConfig
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @DeleteDeploymentConfig@ operation.
--
-- /See:/ 'newDeleteDeploymentConfig' smart constructor.
data DeleteDeploymentConfig = DeleteDeploymentConfig'
  { -- | The name of a deployment configuration associated with the IAM user or
    -- Amazon Web Services account.
    deploymentConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigName', 'deleteDeploymentConfig_deploymentConfigName' - The name of a deployment configuration associated with the IAM user or
-- Amazon Web Services account.
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
-- Amazon Web Services account.
deleteDeploymentConfig_deploymentConfigName :: Lens.Lens' DeleteDeploymentConfig Prelude.Text
deleteDeploymentConfig_deploymentConfigName = Lens.lens (\DeleteDeploymentConfig' {deploymentConfigName} -> deploymentConfigName) (\s@DeleteDeploymentConfig' {} a -> s {deploymentConfigName = a} :: DeleteDeploymentConfig)

instance Core.AWSRequest DeleteDeploymentConfig where
  type
    AWSResponse DeleteDeploymentConfig =
      DeleteDeploymentConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteDeploymentConfigResponse'

instance Prelude.Hashable DeleteDeploymentConfig where
  hashWithSalt _salt DeleteDeploymentConfig' {..} =
    _salt `Prelude.hashWithSalt` deploymentConfigName

instance Prelude.NFData DeleteDeploymentConfig where
  rnf DeleteDeploymentConfig' {..} =
    Prelude.rnf deploymentConfigName

instance Data.ToHeaders DeleteDeploymentConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.DeleteDeploymentConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDeploymentConfig where
  toJSON DeleteDeploymentConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "deploymentConfigName"
                  Data..= deploymentConfigName
              )
          ]
      )

instance Data.ToPath DeleteDeploymentConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDeploymentConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeploymentConfigResponse' smart constructor.
data DeleteDeploymentConfigResponse = DeleteDeploymentConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
