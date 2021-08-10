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
-- Module      : Network.AWS.CodeDeploy.GetDeploymentConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment configuration.
module Network.AWS.CodeDeploy.GetDeploymentConfig
  ( -- * Creating a Request
    GetDeploymentConfig (..),
    newGetDeploymentConfig,

    -- * Request Lenses
    getDeploymentConfig_deploymentConfigName,

    -- * Destructuring the Response
    GetDeploymentConfigResponse (..),
    newGetDeploymentConfigResponse,

    -- * Response Lenses
    getDeploymentConfigResponse_deploymentConfigInfo,
    getDeploymentConfigResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeploymentConfig@ operation.
--
-- /See:/ 'newGetDeploymentConfig' smart constructor.
data GetDeploymentConfig = GetDeploymentConfig'
  { -- | The name of a deployment configuration associated with the IAM user or
    -- AWS account.
    deploymentConfigName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigName', 'getDeploymentConfig_deploymentConfigName' - The name of a deployment configuration associated with the IAM user or
-- AWS account.
newGetDeploymentConfig ::
  -- | 'deploymentConfigName'
  Prelude.Text ->
  GetDeploymentConfig
newGetDeploymentConfig pDeploymentConfigName_ =
  GetDeploymentConfig'
    { deploymentConfigName =
        pDeploymentConfigName_
    }

-- | The name of a deployment configuration associated with the IAM user or
-- AWS account.
getDeploymentConfig_deploymentConfigName :: Lens.Lens' GetDeploymentConfig Prelude.Text
getDeploymentConfig_deploymentConfigName = Lens.lens (\GetDeploymentConfig' {deploymentConfigName} -> deploymentConfigName) (\s@GetDeploymentConfig' {} a -> s {deploymentConfigName = a} :: GetDeploymentConfig)

instance Core.AWSRequest GetDeploymentConfig where
  type
    AWSResponse GetDeploymentConfig =
      GetDeploymentConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentConfigResponse'
            Prelude.<$> (x Core..?> "deploymentConfigInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeploymentConfig

instance Prelude.NFData GetDeploymentConfig

instance Core.ToHeaders GetDeploymentConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetDeploymentConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDeploymentConfig where
  toJSON GetDeploymentConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "deploymentConfigName"
                  Core..= deploymentConfigName
              )
          ]
      )

instance Core.ToPath GetDeploymentConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDeploymentConfig where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetDeploymentConfig@ operation.
--
-- /See:/ 'newGetDeploymentConfigResponse' smart constructor.
data GetDeploymentConfigResponse = GetDeploymentConfigResponse'
  { -- | Information about the deployment configuration.
    deploymentConfigInfo :: Prelude.Maybe DeploymentConfigInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentConfigInfo', 'getDeploymentConfigResponse_deploymentConfigInfo' - Information about the deployment configuration.
--
-- 'httpStatus', 'getDeploymentConfigResponse_httpStatus' - The response's http status code.
newGetDeploymentConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentConfigResponse
newGetDeploymentConfigResponse pHttpStatus_ =
  GetDeploymentConfigResponse'
    { deploymentConfigInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deployment configuration.
getDeploymentConfigResponse_deploymentConfigInfo :: Lens.Lens' GetDeploymentConfigResponse (Prelude.Maybe DeploymentConfigInfo)
getDeploymentConfigResponse_deploymentConfigInfo = Lens.lens (\GetDeploymentConfigResponse' {deploymentConfigInfo} -> deploymentConfigInfo) (\s@GetDeploymentConfigResponse' {} a -> s {deploymentConfigInfo = a} :: GetDeploymentConfigResponse)

-- | The response's http status code.
getDeploymentConfigResponse_httpStatus :: Lens.Lens' GetDeploymentConfigResponse Prelude.Int
getDeploymentConfigResponse_httpStatus = Lens.lens (\GetDeploymentConfigResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentConfigResponse' {} a -> s {httpStatus = a} :: GetDeploymentConfigResponse)

instance Prelude.NFData GetDeploymentConfigResponse
