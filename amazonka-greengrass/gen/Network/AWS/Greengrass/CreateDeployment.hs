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
-- Module      : Network.AWS.Greengrass.CreateDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a deployment. \'\'CreateDeployment\'\' requests are idempotent
-- with respect to the \'\'X-Amzn-Client-Token\'\' token and the request
-- parameters.
module Network.AWS.Greengrass.CreateDeployment
  ( -- * Creating a Request
    CreateDeployment (..),
    newCreateDeployment,

    -- * Request Lenses
    createDeployment_deploymentId,
    createDeployment_groupVersionId,
    createDeployment_amznClientToken,
    createDeployment_groupId,
    createDeployment_deploymentType,

    -- * Destructuring the Response
    CreateDeploymentResponse (..),
    newCreateDeploymentResponse,

    -- * Response Lenses
    createDeploymentResponse_deploymentId,
    createDeploymentResponse_deploymentArn,
    createDeploymentResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDeployment' smart constructor.
data CreateDeployment = CreateDeployment'
  { -- | The ID of the deployment if you wish to redeploy a previous deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group version to be deployed.
    groupVersionId :: Prelude.Maybe Prelude.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text,
    -- | The type of deployment. When used for \'\'CreateDeployment\'\', only
    -- \'\'NewDeployment\'\' and \'\'Redeployment\'\' are valid.
    deploymentType :: DeploymentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'createDeployment_deploymentId' - The ID of the deployment if you wish to redeploy a previous deployment.
--
-- 'groupVersionId', 'createDeployment_groupVersionId' - The ID of the group version to be deployed.
--
-- 'amznClientToken', 'createDeployment_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'groupId', 'createDeployment_groupId' - The ID of the Greengrass group.
--
-- 'deploymentType', 'createDeployment_deploymentType' - The type of deployment. When used for \'\'CreateDeployment\'\', only
-- \'\'NewDeployment\'\' and \'\'Redeployment\'\' are valid.
newCreateDeployment ::
  -- | 'groupId'
  Prelude.Text ->
  -- | 'deploymentType'
  DeploymentType ->
  CreateDeployment
newCreateDeployment pGroupId_ pDeploymentType_ =
  CreateDeployment'
    { deploymentId = Prelude.Nothing,
      groupVersionId = Prelude.Nothing,
      amznClientToken = Prelude.Nothing,
      groupId = pGroupId_,
      deploymentType = pDeploymentType_
    }

-- | The ID of the deployment if you wish to redeploy a previous deployment.
createDeployment_deploymentId :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_deploymentId = Lens.lens (\CreateDeployment' {deploymentId} -> deploymentId) (\s@CreateDeployment' {} a -> s {deploymentId = a} :: CreateDeployment)

-- | The ID of the group version to be deployed.
createDeployment_groupVersionId :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_groupVersionId = Lens.lens (\CreateDeployment' {groupVersionId} -> groupVersionId) (\s@CreateDeployment' {} a -> s {groupVersionId = a} :: CreateDeployment)

-- | A client token used to correlate requests and responses.
createDeployment_amznClientToken :: Lens.Lens' CreateDeployment (Prelude.Maybe Prelude.Text)
createDeployment_amznClientToken = Lens.lens (\CreateDeployment' {amznClientToken} -> amznClientToken) (\s@CreateDeployment' {} a -> s {amznClientToken = a} :: CreateDeployment)

-- | The ID of the Greengrass group.
createDeployment_groupId :: Lens.Lens' CreateDeployment Prelude.Text
createDeployment_groupId = Lens.lens (\CreateDeployment' {groupId} -> groupId) (\s@CreateDeployment' {} a -> s {groupId = a} :: CreateDeployment)

-- | The type of deployment. When used for \'\'CreateDeployment\'\', only
-- \'\'NewDeployment\'\' and \'\'Redeployment\'\' are valid.
createDeployment_deploymentType :: Lens.Lens' CreateDeployment DeploymentType
createDeployment_deploymentType = Lens.lens (\CreateDeployment' {deploymentType} -> deploymentType) (\s@CreateDeployment' {} a -> s {deploymentType = a} :: CreateDeployment)

instance Prelude.AWSRequest CreateDeployment where
  type Rs CreateDeployment = CreateDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDeploymentResponse'
            Prelude.<$> (x Prelude..?> "DeploymentId")
            Prelude.<*> (x Prelude..?> "DeploymentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDeployment

instance Prelude.NFData CreateDeployment

instance Prelude.ToHeaders CreateDeployment where
  toHeaders CreateDeployment' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToJSON CreateDeployment where
  toJSON CreateDeployment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeploymentId" Prelude..=)
              Prelude.<$> deploymentId,
            ("GroupVersionId" Prelude..=)
              Prelude.<$> groupVersionId,
            Prelude.Just
              ("DeploymentType" Prelude..= deploymentType)
          ]
      )

instance Prelude.ToPath CreateDeployment where
  toPath CreateDeployment' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/deployments"
      ]

instance Prelude.ToQuery CreateDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDeploymentResponse' smart constructor.
data CreateDeploymentResponse = CreateDeploymentResponse'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the deployment.
    deploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'createDeploymentResponse_deploymentId' - The ID of the deployment.
--
-- 'deploymentArn', 'createDeploymentResponse_deploymentArn' - The ARN of the deployment.
--
-- 'httpStatus', 'createDeploymentResponse_httpStatus' - The response's http status code.
newCreateDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDeploymentResponse
newCreateDeploymentResponse pHttpStatus_ =
  CreateDeploymentResponse'
    { deploymentId =
        Prelude.Nothing,
      deploymentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deployment.
createDeploymentResponse_deploymentId :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentId = Lens.lens (\CreateDeploymentResponse' {deploymentId} -> deploymentId) (\s@CreateDeploymentResponse' {} a -> s {deploymentId = a} :: CreateDeploymentResponse)

-- | The ARN of the deployment.
createDeploymentResponse_deploymentArn :: Lens.Lens' CreateDeploymentResponse (Prelude.Maybe Prelude.Text)
createDeploymentResponse_deploymentArn = Lens.lens (\CreateDeploymentResponse' {deploymentArn} -> deploymentArn) (\s@CreateDeploymentResponse' {} a -> s {deploymentArn = a} :: CreateDeploymentResponse)

-- | The response's http status code.
createDeploymentResponse_httpStatus :: Lens.Lens' CreateDeploymentResponse Prelude.Int
createDeploymentResponse_httpStatus = Lens.lens (\CreateDeploymentResponse' {httpStatus} -> httpStatus) (\s@CreateDeploymentResponse' {} a -> s {httpStatus = a} :: CreateDeploymentResponse)

instance Prelude.NFData CreateDeploymentResponse
