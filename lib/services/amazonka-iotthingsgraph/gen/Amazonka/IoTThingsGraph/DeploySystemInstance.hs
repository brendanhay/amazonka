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
-- Module      : Amazonka.IoTThingsGraph.DeploySystemInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Greengrass and Cloud Deployments__
--
-- Deploys the system instance to the target specified in
-- @CreateSystemInstance@.
--
-- __Greengrass Deployments__
--
-- If the system or any workflows and entities have been updated before
-- this action is called, then the deployment will create a new Amazon
-- Simple Storage Service resource file and then deploy it.
--
-- Since this action creates a Greengrass deployment on the caller\'s
-- behalf, the calling identity must have write permissions to the
-- specified Greengrass group. Otherwise, the call will fail with an
-- authorization error.
--
-- For information about the artifacts that get added to your Greengrass
-- core device when you use this API, see
-- <https://docs.aws.amazon.com/thingsgraph/latest/ug/iot-tg-greengrass.html AWS IoT Things Graph and AWS IoT Greengrass>.
module Amazonka.IoTThingsGraph.DeploySystemInstance
  ( -- * Creating a Request
    DeploySystemInstance (..),
    newDeploySystemInstance,

    -- * Request Lenses
    deploySystemInstance_id,

    -- * Destructuring the Response
    DeploySystemInstanceResponse (..),
    newDeploySystemInstanceResponse,

    -- * Response Lenses
    deploySystemInstanceResponse_greengrassDeploymentId,
    deploySystemInstanceResponse_httpStatus,
    deploySystemInstanceResponse_summary,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeploySystemInstance' smart constructor.
data DeploySystemInstance = DeploySystemInstance'
  { -- | The ID of the system instance. This value is returned by the
    -- @CreateSystemInstance@ action.
    --
    -- The ID should be in the following format.
    --
    -- @urn:tdm:REGION\/ACCOUNT ID\/default:deployment:DEPLOYMENTNAME@
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploySystemInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deploySystemInstance_id' - The ID of the system instance. This value is returned by the
-- @CreateSystemInstance@ action.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:deployment:DEPLOYMENTNAME@
newDeploySystemInstance ::
  DeploySystemInstance
newDeploySystemInstance =
  DeploySystemInstance' {id = Prelude.Nothing}

-- | The ID of the system instance. This value is returned by the
-- @CreateSystemInstance@ action.
--
-- The ID should be in the following format.
--
-- @urn:tdm:REGION\/ACCOUNT ID\/default:deployment:DEPLOYMENTNAME@
deploySystemInstance_id :: Lens.Lens' DeploySystemInstance (Prelude.Maybe Prelude.Text)
deploySystemInstance_id = Lens.lens (\DeploySystemInstance' {id} -> id) (\s@DeploySystemInstance' {} a -> s {id = a} :: DeploySystemInstance)

instance Core.AWSRequest DeploySystemInstance where
  type
    AWSResponse DeploySystemInstance =
      DeploySystemInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeploySystemInstanceResponse'
            Prelude.<$> (x Core..?> "greengrassDeploymentId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "summary")
      )

instance Prelude.Hashable DeploySystemInstance where
  hashWithSalt _salt DeploySystemInstance' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeploySystemInstance where
  rnf DeploySystemInstance' {..} = Prelude.rnf id

instance Core.ToHeaders DeploySystemInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.DeploySystemInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeploySystemInstance where
  toJSON DeploySystemInstance' {..} =
    Core.object
      (Prelude.catMaybes [("id" Core..=) Prelude.<$> id])

instance Core.ToPath DeploySystemInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery DeploySystemInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeploySystemInstanceResponse' smart constructor.
data DeploySystemInstanceResponse = DeploySystemInstanceResponse'
  { -- | The ID of the Greengrass deployment used to deploy the system instance.
    greengrassDeploymentId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that contains summary information about a system instance that
    -- was deployed.
    summary :: SystemInstanceSummary
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploySystemInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'greengrassDeploymentId', 'deploySystemInstanceResponse_greengrassDeploymentId' - The ID of the Greengrass deployment used to deploy the system instance.
--
-- 'httpStatus', 'deploySystemInstanceResponse_httpStatus' - The response's http status code.
--
-- 'summary', 'deploySystemInstanceResponse_summary' - An object that contains summary information about a system instance that
-- was deployed.
newDeploySystemInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'summary'
  SystemInstanceSummary ->
  DeploySystemInstanceResponse
newDeploySystemInstanceResponse
  pHttpStatus_
  pSummary_ =
    DeploySystemInstanceResponse'
      { greengrassDeploymentId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        summary = pSummary_
      }

-- | The ID of the Greengrass deployment used to deploy the system instance.
deploySystemInstanceResponse_greengrassDeploymentId :: Lens.Lens' DeploySystemInstanceResponse (Prelude.Maybe Prelude.Text)
deploySystemInstanceResponse_greengrassDeploymentId = Lens.lens (\DeploySystemInstanceResponse' {greengrassDeploymentId} -> greengrassDeploymentId) (\s@DeploySystemInstanceResponse' {} a -> s {greengrassDeploymentId = a} :: DeploySystemInstanceResponse)

-- | The response's http status code.
deploySystemInstanceResponse_httpStatus :: Lens.Lens' DeploySystemInstanceResponse Prelude.Int
deploySystemInstanceResponse_httpStatus = Lens.lens (\DeploySystemInstanceResponse' {httpStatus} -> httpStatus) (\s@DeploySystemInstanceResponse' {} a -> s {httpStatus = a} :: DeploySystemInstanceResponse)

-- | An object that contains summary information about a system instance that
-- was deployed.
deploySystemInstanceResponse_summary :: Lens.Lens' DeploySystemInstanceResponse SystemInstanceSummary
deploySystemInstanceResponse_summary = Lens.lens (\DeploySystemInstanceResponse' {summary} -> summary) (\s@DeploySystemInstanceResponse' {} a -> s {summary = a} :: DeploySystemInstanceResponse)

instance Prelude.NFData DeploySystemInstanceResponse where
  rnf DeploySystemInstanceResponse' {..} =
    Prelude.rnf greengrassDeploymentId
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf summary
