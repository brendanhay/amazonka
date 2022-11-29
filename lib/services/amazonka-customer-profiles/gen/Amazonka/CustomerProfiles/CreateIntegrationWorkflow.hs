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
-- Module      : Amazonka.CustomerProfiles.CreateIntegrationWorkflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an integration workflow. An integration workflow is an async
-- process which ingests historic data and sets up an integration for
-- ongoing updates. The supported Amazon AppFlow sources are Salesforce,
-- ServiceNow, and Marketo.
module Amazonka.CustomerProfiles.CreateIntegrationWorkflow
  ( -- * Creating a Request
    CreateIntegrationWorkflow (..),
    newCreateIntegrationWorkflow,

    -- * Request Lenses
    createIntegrationWorkflow_tags,
    createIntegrationWorkflow_domainName,
    createIntegrationWorkflow_workflowType,
    createIntegrationWorkflow_integrationConfig,
    createIntegrationWorkflow_objectTypeName,
    createIntegrationWorkflow_roleArn,

    -- * Destructuring the Response
    CreateIntegrationWorkflowResponse (..),
    newCreateIntegrationWorkflowResponse,

    -- * Response Lenses
    createIntegrationWorkflowResponse_httpStatus,
    createIntegrationWorkflowResponse_workflowId,
    createIntegrationWorkflowResponse_message,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateIntegrationWorkflow' smart constructor.
data CreateIntegrationWorkflow = CreateIntegrationWorkflow'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
    workflowType :: WorkflowType,
    -- | Configuration data for integration workflow.
    integrationConfig :: IntegrationConfig,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role. Customer Profiles
    -- assumes this role to create resources on your behalf as part of workflow
    -- execution.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createIntegrationWorkflow_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'createIntegrationWorkflow_domainName' - The unique name of the domain.
--
-- 'workflowType', 'createIntegrationWorkflow_workflowType' - The type of workflow. The only supported value is APPFLOW_INTEGRATION.
--
-- 'integrationConfig', 'createIntegrationWorkflow_integrationConfig' - Configuration data for integration workflow.
--
-- 'objectTypeName', 'createIntegrationWorkflow_objectTypeName' - The name of the profile object type.
--
-- 'roleArn', 'createIntegrationWorkflow_roleArn' - The Amazon Resource Name (ARN) of the IAM role. Customer Profiles
-- assumes this role to create resources on your behalf as part of workflow
-- execution.
newCreateIntegrationWorkflow ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'integrationConfig'
  IntegrationConfig ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateIntegrationWorkflow
newCreateIntegrationWorkflow
  pDomainName_
  pWorkflowType_
  pIntegrationConfig_
  pObjectTypeName_
  pRoleArn_ =
    CreateIntegrationWorkflow'
      { tags = Prelude.Nothing,
        domainName = pDomainName_,
        workflowType = pWorkflowType_,
        integrationConfig = pIntegrationConfig_,
        objectTypeName = pObjectTypeName_,
        roleArn = pRoleArn_
      }

-- | The tags used to organize, track, or control access for this resource.
createIntegrationWorkflow_tags :: Lens.Lens' CreateIntegrationWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createIntegrationWorkflow_tags = Lens.lens (\CreateIntegrationWorkflow' {tags} -> tags) (\s@CreateIntegrationWorkflow' {} a -> s {tags = a} :: CreateIntegrationWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
createIntegrationWorkflow_domainName :: Lens.Lens' CreateIntegrationWorkflow Prelude.Text
createIntegrationWorkflow_domainName = Lens.lens (\CreateIntegrationWorkflow' {domainName} -> domainName) (\s@CreateIntegrationWorkflow' {} a -> s {domainName = a} :: CreateIntegrationWorkflow)

-- | The type of workflow. The only supported value is APPFLOW_INTEGRATION.
createIntegrationWorkflow_workflowType :: Lens.Lens' CreateIntegrationWorkflow WorkflowType
createIntegrationWorkflow_workflowType = Lens.lens (\CreateIntegrationWorkflow' {workflowType} -> workflowType) (\s@CreateIntegrationWorkflow' {} a -> s {workflowType = a} :: CreateIntegrationWorkflow)

-- | Configuration data for integration workflow.
createIntegrationWorkflow_integrationConfig :: Lens.Lens' CreateIntegrationWorkflow IntegrationConfig
createIntegrationWorkflow_integrationConfig = Lens.lens (\CreateIntegrationWorkflow' {integrationConfig} -> integrationConfig) (\s@CreateIntegrationWorkflow' {} a -> s {integrationConfig = a} :: CreateIntegrationWorkflow)

-- | The name of the profile object type.
createIntegrationWorkflow_objectTypeName :: Lens.Lens' CreateIntegrationWorkflow Prelude.Text
createIntegrationWorkflow_objectTypeName = Lens.lens (\CreateIntegrationWorkflow' {objectTypeName} -> objectTypeName) (\s@CreateIntegrationWorkflow' {} a -> s {objectTypeName = a} :: CreateIntegrationWorkflow)

-- | The Amazon Resource Name (ARN) of the IAM role. Customer Profiles
-- assumes this role to create resources on your behalf as part of workflow
-- execution.
createIntegrationWorkflow_roleArn :: Lens.Lens' CreateIntegrationWorkflow Prelude.Text
createIntegrationWorkflow_roleArn = Lens.lens (\CreateIntegrationWorkflow' {roleArn} -> roleArn) (\s@CreateIntegrationWorkflow' {} a -> s {roleArn = a} :: CreateIntegrationWorkflow)

instance Core.AWSRequest CreateIntegrationWorkflow where
  type
    AWSResponse CreateIntegrationWorkflow =
      CreateIntegrationWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateIntegrationWorkflowResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "WorkflowId")
            Prelude.<*> (x Core..:> "Message")
      )

instance Prelude.Hashable CreateIntegrationWorkflow where
  hashWithSalt _salt CreateIntegrationWorkflow' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` workflowType
      `Prelude.hashWithSalt` integrationConfig
      `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateIntegrationWorkflow where
  rnf CreateIntegrationWorkflow' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf workflowType
      `Prelude.seq` Prelude.rnf integrationConfig
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf roleArn

instance Core.ToHeaders CreateIntegrationWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateIntegrationWorkflow where
  toJSON CreateIntegrationWorkflow' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("WorkflowType" Core..= workflowType),
            Prelude.Just
              ("IntegrationConfig" Core..= integrationConfig),
            Prelude.Just
              ("ObjectTypeName" Core..= objectTypeName),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateIntegrationWorkflow where
  toPath CreateIntegrationWorkflow' {..} =
    Prelude.mconcat
      [ "/domains/",
        Core.toBS domainName,
        "/workflows/integrations"
      ]

instance Core.ToQuery CreateIntegrationWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateIntegrationWorkflowResponse' smart constructor.
data CreateIntegrationWorkflowResponse = CreateIntegrationWorkflowResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Unique identifier for the workflow.
    workflowId :: Prelude.Text,
    -- | A message indicating create request was received.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateIntegrationWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createIntegrationWorkflowResponse_httpStatus' - The response's http status code.
--
-- 'workflowId', 'createIntegrationWorkflowResponse_workflowId' - Unique identifier for the workflow.
--
-- 'message', 'createIntegrationWorkflowResponse_message' - A message indicating create request was received.
newCreateIntegrationWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'message'
  Prelude.Text ->
  CreateIntegrationWorkflowResponse
newCreateIntegrationWorkflowResponse
  pHttpStatus_
  pWorkflowId_
  pMessage_ =
    CreateIntegrationWorkflowResponse'
      { httpStatus =
          pHttpStatus_,
        workflowId = pWorkflowId_,
        message = pMessage_
      }

-- | The response's http status code.
createIntegrationWorkflowResponse_httpStatus :: Lens.Lens' CreateIntegrationWorkflowResponse Prelude.Int
createIntegrationWorkflowResponse_httpStatus = Lens.lens (\CreateIntegrationWorkflowResponse' {httpStatus} -> httpStatus) (\s@CreateIntegrationWorkflowResponse' {} a -> s {httpStatus = a} :: CreateIntegrationWorkflowResponse)

-- | Unique identifier for the workflow.
createIntegrationWorkflowResponse_workflowId :: Lens.Lens' CreateIntegrationWorkflowResponse Prelude.Text
createIntegrationWorkflowResponse_workflowId = Lens.lens (\CreateIntegrationWorkflowResponse' {workflowId} -> workflowId) (\s@CreateIntegrationWorkflowResponse' {} a -> s {workflowId = a} :: CreateIntegrationWorkflowResponse)

-- | A message indicating create request was received.
createIntegrationWorkflowResponse_message :: Lens.Lens' CreateIntegrationWorkflowResponse Prelude.Text
createIntegrationWorkflowResponse_message = Lens.lens (\CreateIntegrationWorkflowResponse' {message} -> message) (\s@CreateIntegrationWorkflowResponse' {} a -> s {message = a} :: CreateIntegrationWorkflowResponse)

instance
  Prelude.NFData
    CreateIntegrationWorkflowResponse
  where
  rnf CreateIntegrationWorkflowResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf message
