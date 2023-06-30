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
-- Module      : Amazonka.MigrationHubOrchestrator.CreateWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a workflow to orchestrate your migrations.
module Amazonka.MigrationHubOrchestrator.CreateWorkflow
  ( -- * Creating a Request
    CreateWorkflow (..),
    newCreateWorkflow,

    -- * Request Lenses
    createWorkflow_description,
    createWorkflow_stepTargets,
    createWorkflow_tags,
    createWorkflow_name,
    createWorkflow_templateId,
    createWorkflow_applicationConfigurationId,
    createWorkflow_inputParameters,

    -- * Destructuring the Response
    CreateWorkflowResponse (..),
    newCreateWorkflowResponse,

    -- * Response Lenses
    createWorkflowResponse_adsApplicationConfigurationId,
    createWorkflowResponse_arn,
    createWorkflowResponse_creationTime,
    createWorkflowResponse_description,
    createWorkflowResponse_id,
    createWorkflowResponse_name,
    createWorkflowResponse_status,
    createWorkflowResponse_stepTargets,
    createWorkflowResponse_tags,
    createWorkflowResponse_templateId,
    createWorkflowResponse_workflowInputs,
    createWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkflow' smart constructor.
data CreateWorkflow = CreateWorkflow'
  { -- | The description of the migration workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The servers on which a step will be run.
    stepTargets :: Prelude.Maybe [Prelude.Text],
    -- | The tags to add on a migration workflow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the migration workflow.
    name :: Prelude.Text,
    -- | The ID of the template.
    templateId :: Prelude.Text,
    -- | The configuration ID of the application configured in Application
    -- Discovery Service.
    applicationConfigurationId :: Prelude.Text,
    -- | The input parameters required to create a migration workflow.
    inputParameters :: Data.Sensitive (Prelude.HashMap Prelude.Text StepInput)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createWorkflow_description' - The description of the migration workflow.
--
-- 'stepTargets', 'createWorkflow_stepTargets' - The servers on which a step will be run.
--
-- 'tags', 'createWorkflow_tags' - The tags to add on a migration workflow.
--
-- 'name', 'createWorkflow_name' - The name of the migration workflow.
--
-- 'templateId', 'createWorkflow_templateId' - The ID of the template.
--
-- 'applicationConfigurationId', 'createWorkflow_applicationConfigurationId' - The configuration ID of the application configured in Application
-- Discovery Service.
--
-- 'inputParameters', 'createWorkflow_inputParameters' - The input parameters required to create a migration workflow.
newCreateWorkflow ::
  -- | 'name'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  -- | 'applicationConfigurationId'
  Prelude.Text ->
  CreateWorkflow
newCreateWorkflow
  pName_
  pTemplateId_
  pApplicationConfigurationId_ =
    CreateWorkflow'
      { description = Prelude.Nothing,
        stepTargets = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        templateId = pTemplateId_,
        applicationConfigurationId =
          pApplicationConfigurationId_,
        inputParameters = Prelude.mempty
      }

-- | The description of the migration workflow.
createWorkflow_description :: Lens.Lens' CreateWorkflow (Prelude.Maybe Prelude.Text)
createWorkflow_description = Lens.lens (\CreateWorkflow' {description} -> description) (\s@CreateWorkflow' {} a -> s {description = a} :: CreateWorkflow)

-- | The servers on which a step will be run.
createWorkflow_stepTargets :: Lens.Lens' CreateWorkflow (Prelude.Maybe [Prelude.Text])
createWorkflow_stepTargets = Lens.lens (\CreateWorkflow' {stepTargets} -> stepTargets) (\s@CreateWorkflow' {} a -> s {stepTargets = a} :: CreateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | The tags to add on a migration workflow.
createWorkflow_tags :: Lens.Lens' CreateWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkflow_tags = Lens.lens (\CreateWorkflow' {tags} -> tags) (\s@CreateWorkflow' {} a -> s {tags = a} :: CreateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | The name of the migration workflow.
createWorkflow_name :: Lens.Lens' CreateWorkflow Prelude.Text
createWorkflow_name = Lens.lens (\CreateWorkflow' {name} -> name) (\s@CreateWorkflow' {} a -> s {name = a} :: CreateWorkflow)

-- | The ID of the template.
createWorkflow_templateId :: Lens.Lens' CreateWorkflow Prelude.Text
createWorkflow_templateId = Lens.lens (\CreateWorkflow' {templateId} -> templateId) (\s@CreateWorkflow' {} a -> s {templateId = a} :: CreateWorkflow)

-- | The configuration ID of the application configured in Application
-- Discovery Service.
createWorkflow_applicationConfigurationId :: Lens.Lens' CreateWorkflow Prelude.Text
createWorkflow_applicationConfigurationId = Lens.lens (\CreateWorkflow' {applicationConfigurationId} -> applicationConfigurationId) (\s@CreateWorkflow' {} a -> s {applicationConfigurationId = a} :: CreateWorkflow)

-- | The input parameters required to create a migration workflow.
createWorkflow_inputParameters :: Lens.Lens' CreateWorkflow (Prelude.HashMap Prelude.Text StepInput)
createWorkflow_inputParameters = Lens.lens (\CreateWorkflow' {inputParameters} -> inputParameters) (\s@CreateWorkflow' {} a -> s {inputParameters = a} :: CreateWorkflow) Prelude.. Data._Sensitive Prelude.. Lens.coerced

instance Core.AWSRequest CreateWorkflow where
  type
    AWSResponse CreateWorkflow =
      CreateWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowResponse'
            Prelude.<$> (x Data..?> "adsApplicationConfigurationId")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "stepTargets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "templateId")
            Prelude.<*> (x Data..?> "workflowInputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkflow where
  hashWithSalt _salt CreateWorkflow' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stepTargets
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` applicationConfigurationId
      `Prelude.hashWithSalt` inputParameters

instance Prelude.NFData CreateWorkflow where
  rnf CreateWorkflow' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf stepTargets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf applicationConfigurationId
      `Prelude.seq` Prelude.rnf inputParameters

instance Data.ToHeaders CreateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkflow where
  toJSON CreateWorkflow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("stepTargets" Data..=) Prelude.<$> stepTargets,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("templateId" Data..= templateId),
            Prelude.Just
              ( "applicationConfigurationId"
                  Data..= applicationConfigurationId
              ),
            Prelude.Just
              ("inputParameters" Data..= inputParameters)
          ]
      )

instance Data.ToPath CreateWorkflow where
  toPath = Prelude.const "/migrationworkflow/"

instance Data.ToQuery CreateWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkflowResponse' smart constructor.
data CreateWorkflowResponse = CreateWorkflowResponse'
  { -- | The configuration ID of the application configured in Application
    -- Discovery Service.
    adsApplicationConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the migration workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the migration workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The servers on which a step will be run.
    stepTargets :: Prelude.Maybe [Prelude.Text],
    -- | The tags to add on a migration workflow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The inputs for creating a migration workflow.
    workflowInputs :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text StepInput)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adsApplicationConfigurationId', 'createWorkflowResponse_adsApplicationConfigurationId' - The configuration ID of the application configured in Application
-- Discovery Service.
--
-- 'arn', 'createWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'creationTime', 'createWorkflowResponse_creationTime' - The time at which the migration workflow was created.
--
-- 'description', 'createWorkflowResponse_description' - The description of the migration workflow.
--
-- 'id', 'createWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'name', 'createWorkflowResponse_name' - The name of the migration workflow.
--
-- 'status', 'createWorkflowResponse_status' - The status of the migration workflow.
--
-- 'stepTargets', 'createWorkflowResponse_stepTargets' - The servers on which a step will be run.
--
-- 'tags', 'createWorkflowResponse_tags' - The tags to add on a migration workflow.
--
-- 'templateId', 'createWorkflowResponse_templateId' - The ID of the template.
--
-- 'workflowInputs', 'createWorkflowResponse_workflowInputs' - The inputs for creating a migration workflow.
--
-- 'httpStatus', 'createWorkflowResponse_httpStatus' - The response's http status code.
newCreateWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkflowResponse
newCreateWorkflowResponse pHttpStatus_ =
  CreateWorkflowResponse'
    { adsApplicationConfigurationId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      stepTargets = Prelude.Nothing,
      tags = Prelude.Nothing,
      templateId = Prelude.Nothing,
      workflowInputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration ID of the application configured in Application
-- Discovery Service.
createWorkflowResponse_adsApplicationConfigurationId :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_adsApplicationConfigurationId = Lens.lens (\CreateWorkflowResponse' {adsApplicationConfigurationId} -> adsApplicationConfigurationId) (\s@CreateWorkflowResponse' {} a -> s {adsApplicationConfigurationId = a} :: CreateWorkflowResponse)

-- | The Amazon Resource Name (ARN) of the migration workflow.
createWorkflowResponse_arn :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_arn = Lens.lens (\CreateWorkflowResponse' {arn} -> arn) (\s@CreateWorkflowResponse' {} a -> s {arn = a} :: CreateWorkflowResponse)

-- | The time at which the migration workflow was created.
createWorkflowResponse_creationTime :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
createWorkflowResponse_creationTime = Lens.lens (\CreateWorkflowResponse' {creationTime} -> creationTime) (\s@CreateWorkflowResponse' {} a -> s {creationTime = a} :: CreateWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the migration workflow.
createWorkflowResponse_description :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_description = Lens.lens (\CreateWorkflowResponse' {description} -> description) (\s@CreateWorkflowResponse' {} a -> s {description = a} :: CreateWorkflowResponse)

-- | The ID of the migration workflow.
createWorkflowResponse_id :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_id = Lens.lens (\CreateWorkflowResponse' {id} -> id) (\s@CreateWorkflowResponse' {} a -> s {id = a} :: CreateWorkflowResponse)

-- | The name of the migration workflow.
createWorkflowResponse_name :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_name = Lens.lens (\CreateWorkflowResponse' {name} -> name) (\s@CreateWorkflowResponse' {} a -> s {name = a} :: CreateWorkflowResponse)

-- | The status of the migration workflow.
createWorkflowResponse_status :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
createWorkflowResponse_status = Lens.lens (\CreateWorkflowResponse' {status} -> status) (\s@CreateWorkflowResponse' {} a -> s {status = a} :: CreateWorkflowResponse)

-- | The servers on which a step will be run.
createWorkflowResponse_stepTargets :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe [Prelude.Text])
createWorkflowResponse_stepTargets = Lens.lens (\CreateWorkflowResponse' {stepTargets} -> stepTargets) (\s@CreateWorkflowResponse' {} a -> s {stepTargets = a} :: CreateWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags to add on a migration workflow.
createWorkflowResponse_tags :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkflowResponse_tags = Lens.lens (\CreateWorkflowResponse' {tags} -> tags) (\s@CreateWorkflowResponse' {} a -> s {tags = a} :: CreateWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the template.
createWorkflowResponse_templateId :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe Prelude.Text)
createWorkflowResponse_templateId = Lens.lens (\CreateWorkflowResponse' {templateId} -> templateId) (\s@CreateWorkflowResponse' {} a -> s {templateId = a} :: CreateWorkflowResponse)

-- | The inputs for creating a migration workflow.
createWorkflowResponse_workflowInputs :: Lens.Lens' CreateWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text StepInput))
createWorkflowResponse_workflowInputs = Lens.lens (\CreateWorkflowResponse' {workflowInputs} -> workflowInputs) (\s@CreateWorkflowResponse' {} a -> s {workflowInputs = a} :: CreateWorkflowResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
createWorkflowResponse_httpStatus :: Lens.Lens' CreateWorkflowResponse Prelude.Int
createWorkflowResponse_httpStatus = Lens.lens (\CreateWorkflowResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowResponse' {} a -> s {httpStatus = a} :: CreateWorkflowResponse)

instance Prelude.NFData CreateWorkflowResponse where
  rnf CreateWorkflowResponse' {..} =
    Prelude.rnf adsApplicationConfigurationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stepTargets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf workflowInputs
      `Prelude.seq` Prelude.rnf httpStatus
