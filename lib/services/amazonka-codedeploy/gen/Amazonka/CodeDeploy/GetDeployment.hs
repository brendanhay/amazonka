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
-- Module      : Amazonka.CodeDeploy.GetDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CodeDeploy.GetDeployment
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetDeployment@ operation.
--
-- /See:/ 'newGetDeployment' smart constructor.
data GetDeployment = GetDeployment'
  { -- | The unique ID of a deployment associated with the IAM user or Amazon Web
    -- Services account.
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
-- 'deploymentId', 'getDeployment_deploymentId' - The unique ID of a deployment associated with the IAM user or Amazon Web
-- Services account.
newGetDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  GetDeployment
newGetDeployment pDeploymentId_ =
  GetDeployment' {deploymentId = pDeploymentId_}

-- | The unique ID of a deployment associated with the IAM user or Amazon Web
-- Services account.
getDeployment_deploymentId :: Lens.Lens' GetDeployment Prelude.Text
getDeployment_deploymentId = Lens.lens (\GetDeployment' {deploymentId} -> deploymentId) (\s@GetDeployment' {} a -> s {deploymentId = a} :: GetDeployment)

instance Core.AWSRequest GetDeployment where
  type
    AWSResponse GetDeployment =
      GetDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentResponse'
            Prelude.<$> (x Data..?> "deploymentInfo")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeployment where
  hashWithSalt _salt GetDeployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData GetDeployment where
  rnf GetDeployment' {..} = Prelude.rnf deploymentId

instance Data.ToHeaders GetDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.GetDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDeployment where
  toJSON GetDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("deploymentId" Data..= deploymentId)]
      )

instance Data.ToPath GetDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDeployment where
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

instance Prelude.NFData GetDeploymentResponse where
  rnf GetDeploymentResponse' {..} =
    Prelude.rnf deploymentInfo
      `Prelude.seq` Prelude.rnf httpStatus
