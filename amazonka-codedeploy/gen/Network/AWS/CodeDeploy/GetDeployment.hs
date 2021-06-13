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
-- Module      : Network.AWS.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a deployment.
--
-- The @content@ property of the @appSpecContent@ object in the returned
-- revision is always null. Use @GetApplicationRevision@ and the @sha256@
-- property of the returned @appSpecContent@ object to get the content of
-- the deployment’s AppSpec file.
module Network.AWS.CodeDeploy.GetDeployment
  ( -- * Creating a Request
    GetDeployment (..),
    newGetDeployment,

    -- * Request Lenses
    getDeployment_deploymentId,

    -- * Destructuring the Response
    GetDeploymentResponse (..),
    newGetDeploymentResponse,

    -- * Response Lenses
    getDeploymentResponse_deploymentInfo,
    getDeploymentResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetDeployment@ operation.
--
-- /See:/ 'newGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | The unique ID of a deployment associated with the IAM user or AWS
    -- account.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'getDeployment_deploymentId' - The unique ID of a deployment associated with the IAM user or AWS
-- account.
newGetDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  GetDeployment
newGetDeployment pDeploymentId_ =
  GetDeployment' {deploymentId = pDeploymentId_}

-- | The unique ID of a deployment associated with the IAM user or AWS
-- account.
getDeployment_deploymentId :: Lens.Lens' GetDeployment Prelude.Text
getDeployment_deploymentId = Lens.lens (\GetDeployment' {deploymentId} -> deploymentId) (\s@GetDeployment' {} a -> s {deploymentId = a} :: GetDeployment)

instance Core.AWSRequest GetDeployment where
  type
    AWSResponse GetDeployment =
      GetDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Prelude.<$> (x Core..?> "deploymentInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployment

instance Prelude.NFData GetDeployment

instance Core.ToHeaders GetDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDeployment where
  toJSON GetDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("deploymentId" Core..= deploymentId)]
      )

instance Core.ToPath GetDeployment where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetDeployment@ operation.
--
-- /See:/ 'newGetDeploymentResponse' smart constructor.
data GetDeploymentResponse = GetDeploymentResponse'
  { -- | Information about the deployment.
    deploymentInfo :: Prelude.Maybe DeploymentInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentInfo', 'getDeploymentResponse_deploymentInfo' - Information about the deployment.
--
-- 'httpStatus', 'getDeploymentResponse_httpStatus' - The response's http status code.
newGetDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentResponse
newGetDeploymentResponse pHttpStatus_ =
  GetDeploymentResponse'
    { deploymentInfo =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deployment.
getDeploymentResponse_deploymentInfo :: Lens.Lens' GetDeploymentResponse (Prelude.Maybe DeploymentInfo)
getDeploymentResponse_deploymentInfo = Lens.lens (\GetDeploymentResponse' {deploymentInfo} -> deploymentInfo) (\s@GetDeploymentResponse' {} a -> s {deploymentInfo = a} :: GetDeploymentResponse)

-- | The response's http status code.
getDeploymentResponse_httpStatus :: Lens.Lens' GetDeploymentResponse Prelude.Int
getDeploymentResponse_httpStatus = Lens.lens (\GetDeploymentResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentResponse' {} a -> s {httpStatus = a} :: GetDeploymentResponse)

instance Prelude.NFData GetDeploymentResponse
