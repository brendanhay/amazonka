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
-- Module      : Amazonka.Greengrass.GetDeploymentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the status of a deployment.
module Amazonka.Greengrass.GetDeploymentStatus
  ( -- * Creating a Request
    GetDeploymentStatus (..),
    newGetDeploymentStatus,

    -- * Request Lenses
    getDeploymentStatus_groupId,
    getDeploymentStatus_deploymentId,

    -- * Destructuring the Response
    GetDeploymentStatusResponse (..),
    newGetDeploymentStatusResponse,

    -- * Response Lenses
    getDeploymentStatusResponse_deploymentStatus,
    getDeploymentStatusResponse_deploymentType,
    getDeploymentStatusResponse_errorDetails,
    getDeploymentStatusResponse_errorMessage,
    getDeploymentStatusResponse_updatedAt,
    getDeploymentStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDeploymentStatus' smart constructor.
data GetDeploymentStatus = GetDeploymentStatus'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text,
    -- | The ID of the deployment.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getDeploymentStatus_groupId' - The ID of the Greengrass group.
--
-- 'deploymentId', 'getDeploymentStatus_deploymentId' - The ID of the deployment.
newGetDeploymentStatus ::
  -- | 'groupId'
  Prelude.Text ->
  -- | 'deploymentId'
  Prelude.Text ->
  GetDeploymentStatus
newGetDeploymentStatus pGroupId_ pDeploymentId_ =
  GetDeploymentStatus'
    { groupId = pGroupId_,
      deploymentId = pDeploymentId_
    }

-- | The ID of the Greengrass group.
getDeploymentStatus_groupId :: Lens.Lens' GetDeploymentStatus Prelude.Text
getDeploymentStatus_groupId = Lens.lens (\GetDeploymentStatus' {groupId} -> groupId) (\s@GetDeploymentStatus' {} a -> s {groupId = a} :: GetDeploymentStatus)

-- | The ID of the deployment.
getDeploymentStatus_deploymentId :: Lens.Lens' GetDeploymentStatus Prelude.Text
getDeploymentStatus_deploymentId = Lens.lens (\GetDeploymentStatus' {deploymentId} -> deploymentId) (\s@GetDeploymentStatus' {} a -> s {deploymentId = a} :: GetDeploymentStatus)

instance Core.AWSRequest GetDeploymentStatus where
  type
    AWSResponse GetDeploymentStatus =
      GetDeploymentStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeploymentStatusResponse'
            Prelude.<$> (x Data..?> "DeploymentStatus")
            Prelude.<*> (x Data..?> "DeploymentType")
            Prelude.<*> (x Data..?> "ErrorDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "UpdatedAt")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDeploymentStatus where
  hashWithSalt _salt GetDeploymentStatus' {..} =
    _salt
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData GetDeploymentStatus where
  rnf GetDeploymentStatus' {..} =
    Prelude.rnf groupId `Prelude.seq`
      Prelude.rnf deploymentId

instance Data.ToHeaders GetDeploymentStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDeploymentStatus where
  toPath GetDeploymentStatus' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/deployments/",
        Data.toBS deploymentId,
        "/status"
      ]

instance Data.ToQuery GetDeploymentStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeploymentStatusResponse' smart constructor.
data GetDeploymentStatusResponse = GetDeploymentStatusResponse'
  { -- | The status of the deployment: \'\'InProgress\'\', \'\'Building\'\',
    -- \'\'Success\'\', or \'\'Failure\'\'.
    deploymentStatus :: Prelude.Maybe Prelude.Text,
    -- | The type of the deployment.
    deploymentType :: Prelude.Maybe DeploymentType,
    -- | Error details
    errorDetails :: Prelude.Maybe [ErrorDetail],
    -- | Error message
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the deployment status
    -- was updated.
    updatedAt :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatus', 'getDeploymentStatusResponse_deploymentStatus' - The status of the deployment: \'\'InProgress\'\', \'\'Building\'\',
-- \'\'Success\'\', or \'\'Failure\'\'.
--
-- 'deploymentType', 'getDeploymentStatusResponse_deploymentType' - The type of the deployment.
--
-- 'errorDetails', 'getDeploymentStatusResponse_errorDetails' - Error details
--
-- 'errorMessage', 'getDeploymentStatusResponse_errorMessage' - Error message
--
-- 'updatedAt', 'getDeploymentStatusResponse_updatedAt' - The time, in milliseconds since the epoch, when the deployment status
-- was updated.
--
-- 'httpStatus', 'getDeploymentStatusResponse_httpStatus' - The response's http status code.
newGetDeploymentStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeploymentStatusResponse
newGetDeploymentStatusResponse pHttpStatus_ =
  GetDeploymentStatusResponse'
    { deploymentStatus =
        Prelude.Nothing,
      deploymentType = Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the deployment: \'\'InProgress\'\', \'\'Building\'\',
-- \'\'Success\'\', or \'\'Failure\'\'.
getDeploymentStatusResponse_deploymentStatus :: Lens.Lens' GetDeploymentStatusResponse (Prelude.Maybe Prelude.Text)
getDeploymentStatusResponse_deploymentStatus = Lens.lens (\GetDeploymentStatusResponse' {deploymentStatus} -> deploymentStatus) (\s@GetDeploymentStatusResponse' {} a -> s {deploymentStatus = a} :: GetDeploymentStatusResponse)

-- | The type of the deployment.
getDeploymentStatusResponse_deploymentType :: Lens.Lens' GetDeploymentStatusResponse (Prelude.Maybe DeploymentType)
getDeploymentStatusResponse_deploymentType = Lens.lens (\GetDeploymentStatusResponse' {deploymentType} -> deploymentType) (\s@GetDeploymentStatusResponse' {} a -> s {deploymentType = a} :: GetDeploymentStatusResponse)

-- | Error details
getDeploymentStatusResponse_errorDetails :: Lens.Lens' GetDeploymentStatusResponse (Prelude.Maybe [ErrorDetail])
getDeploymentStatusResponse_errorDetails = Lens.lens (\GetDeploymentStatusResponse' {errorDetails} -> errorDetails) (\s@GetDeploymentStatusResponse' {} a -> s {errorDetails = a} :: GetDeploymentStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | Error message
getDeploymentStatusResponse_errorMessage :: Lens.Lens' GetDeploymentStatusResponse (Prelude.Maybe Prelude.Text)
getDeploymentStatusResponse_errorMessage = Lens.lens (\GetDeploymentStatusResponse' {errorMessage} -> errorMessage) (\s@GetDeploymentStatusResponse' {} a -> s {errorMessage = a} :: GetDeploymentStatusResponse)

-- | The time, in milliseconds since the epoch, when the deployment status
-- was updated.
getDeploymentStatusResponse_updatedAt :: Lens.Lens' GetDeploymentStatusResponse (Prelude.Maybe Prelude.Text)
getDeploymentStatusResponse_updatedAt = Lens.lens (\GetDeploymentStatusResponse' {updatedAt} -> updatedAt) (\s@GetDeploymentStatusResponse' {} a -> s {updatedAt = a} :: GetDeploymentStatusResponse)

-- | The response's http status code.
getDeploymentStatusResponse_httpStatus :: Lens.Lens' GetDeploymentStatusResponse Prelude.Int
getDeploymentStatusResponse_httpStatus = Lens.lens (\GetDeploymentStatusResponse' {httpStatus} -> httpStatus) (\s@GetDeploymentStatusResponse' {} a -> s {httpStatus = a} :: GetDeploymentStatusResponse)

instance Prelude.NFData GetDeploymentStatusResponse where
  rnf GetDeploymentStatusResponse' {..} =
    Prelude.rnf deploymentStatus `Prelude.seq`
      Prelude.rnf deploymentType `Prelude.seq`
        Prelude.rnf errorDetails `Prelude.seq`
          Prelude.rnf errorMessage `Prelude.seq`
            Prelude.rnf updatedAt `Prelude.seq`
              Prelude.rnf httpStatus
