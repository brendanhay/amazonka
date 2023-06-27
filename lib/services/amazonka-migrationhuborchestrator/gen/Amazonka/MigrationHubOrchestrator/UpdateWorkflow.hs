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
-- Module      : Amazonka.MigrationHubOrchestrator.UpdateWorkflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a migration workflow.
module Amazonka.MigrationHubOrchestrator.UpdateWorkflow
  ( -- * Creating a Request
    UpdateWorkflow (..),
    newUpdateWorkflow,

    -- * Request Lenses
    updateWorkflow_description,
    updateWorkflow_inputParameters,
    updateWorkflow_name,
    updateWorkflow_stepTargets,
    updateWorkflow_id,

    -- * Destructuring the Response
    UpdateWorkflowResponse (..),
    newUpdateWorkflowResponse,

    -- * Response Lenses
    updateWorkflowResponse_adsApplicationConfigurationId,
    updateWorkflowResponse_arn,
    updateWorkflowResponse_creationTime,
    updateWorkflowResponse_description,
    updateWorkflowResponse_id,
    updateWorkflowResponse_lastModifiedTime,
    updateWorkflowResponse_name,
    updateWorkflowResponse_status,
    updateWorkflowResponse_stepTargets,
    updateWorkflowResponse_tags,
    updateWorkflowResponse_templateId,
    updateWorkflowResponse_workflowInputs,
    updateWorkflowResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkflow' smart constructor.
data UpdateWorkflow = UpdateWorkflow'
  { -- | The description of the migration workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The input parameters required to update a migration workflow.
    inputParameters :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text StepInput)),
    -- | The name of the migration workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The servers on which a step will be run.
    stepTargets :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the migration workflow.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateWorkflow_description' - The description of the migration workflow.
--
-- 'inputParameters', 'updateWorkflow_inputParameters' - The input parameters required to update a migration workflow.
--
-- 'name', 'updateWorkflow_name' - The name of the migration workflow.
--
-- 'stepTargets', 'updateWorkflow_stepTargets' - The servers on which a step will be run.
--
-- 'id', 'updateWorkflow_id' - The ID of the migration workflow.
newUpdateWorkflow ::
  -- | 'id'
  Prelude.Text ->
  UpdateWorkflow
newUpdateWorkflow pId_ =
  UpdateWorkflow'
    { description = Prelude.Nothing,
      inputParameters = Prelude.Nothing,
      name = Prelude.Nothing,
      stepTargets = Prelude.Nothing,
      id = pId_
    }

-- | The description of the migration workflow.
updateWorkflow_description :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Text)
updateWorkflow_description = Lens.lens (\UpdateWorkflow' {description} -> description) (\s@UpdateWorkflow' {} a -> s {description = a} :: UpdateWorkflow)

-- | The input parameters required to update a migration workflow.
updateWorkflow_inputParameters :: Lens.Lens' UpdateWorkflow (Prelude.Maybe (Prelude.HashMap Prelude.Text StepInput))
updateWorkflow_inputParameters = Lens.lens (\UpdateWorkflow' {inputParameters} -> inputParameters) (\s@UpdateWorkflow' {} a -> s {inputParameters = a} :: UpdateWorkflow) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The name of the migration workflow.
updateWorkflow_name :: Lens.Lens' UpdateWorkflow (Prelude.Maybe Prelude.Text)
updateWorkflow_name = Lens.lens (\UpdateWorkflow' {name} -> name) (\s@UpdateWorkflow' {} a -> s {name = a} :: UpdateWorkflow)

-- | The servers on which a step will be run.
updateWorkflow_stepTargets :: Lens.Lens' UpdateWorkflow (Prelude.Maybe [Prelude.Text])
updateWorkflow_stepTargets = Lens.lens (\UpdateWorkflow' {stepTargets} -> stepTargets) (\s@UpdateWorkflow' {} a -> s {stepTargets = a} :: UpdateWorkflow) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the migration workflow.
updateWorkflow_id :: Lens.Lens' UpdateWorkflow Prelude.Text
updateWorkflow_id = Lens.lens (\UpdateWorkflow' {id} -> id) (\s@UpdateWorkflow' {} a -> s {id = a} :: UpdateWorkflow)

instance Core.AWSRequest UpdateWorkflow where
  type
    AWSResponse UpdateWorkflow =
      UpdateWorkflowResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkflowResponse'
            Prelude.<$> (x Data..?> "adsApplicationConfigurationId")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "stepTargets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "templateId")
            Prelude.<*> (x Data..?> "workflowInputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkflow where
  hashWithSalt _salt UpdateWorkflow' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` inputParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stepTargets
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateWorkflow where
  rnf UpdateWorkflow' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf inputParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stepTargets
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateWorkflow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkflow where
  toJSON UpdateWorkflow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("inputParameters" Data..=)
              Prelude.<$> inputParameters,
            ("name" Data..=) Prelude.<$> name,
            ("stepTargets" Data..=) Prelude.<$> stepTargets
          ]
      )

instance Data.ToPath UpdateWorkflow where
  toPath UpdateWorkflow' {..} =
    Prelude.mconcat
      ["/migrationworkflow/", Data.toBS id]

instance Data.ToQuery UpdateWorkflow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkflowResponse' smart constructor.
data UpdateWorkflowResponse = UpdateWorkflowResponse'
  { -- | The ID of the application configured in Application Discovery Service.
    adsApplicationConfigurationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the migration workflow.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the migration workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time at which the migration workflow was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the migration workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The servers on which a step will be run.
    stepTargets :: Prelude.Maybe [Prelude.Text],
    -- | The tags added to the migration workflow.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The inputs required to update a migration workflow.
    workflowInputs :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text StepInput)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adsApplicationConfigurationId', 'updateWorkflowResponse_adsApplicationConfigurationId' - The ID of the application configured in Application Discovery Service.
--
-- 'arn', 'updateWorkflowResponse_arn' - The Amazon Resource Name (ARN) of the migration workflow.
--
-- 'creationTime', 'updateWorkflowResponse_creationTime' - The time at which the migration workflow was created.
--
-- 'description', 'updateWorkflowResponse_description' - The description of the migration workflow.
--
-- 'id', 'updateWorkflowResponse_id' - The ID of the migration workflow.
--
-- 'lastModifiedTime', 'updateWorkflowResponse_lastModifiedTime' - The time at which the migration workflow was last modified.
--
-- 'name', 'updateWorkflowResponse_name' - The name of the migration workflow.
--
-- 'status', 'updateWorkflowResponse_status' - The status of the migration workflow.
--
-- 'stepTargets', 'updateWorkflowResponse_stepTargets' - The servers on which a step will be run.
--
-- 'tags', 'updateWorkflowResponse_tags' - The tags added to the migration workflow.
--
-- 'templateId', 'updateWorkflowResponse_templateId' - The ID of the template.
--
-- 'workflowInputs', 'updateWorkflowResponse_workflowInputs' - The inputs required to update a migration workflow.
--
-- 'httpStatus', 'updateWorkflowResponse_httpStatus' - The response's http status code.
newUpdateWorkflowResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkflowResponse
newUpdateWorkflowResponse pHttpStatus_ =
  UpdateWorkflowResponse'
    { adsApplicationConfigurationId =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      stepTargets = Prelude.Nothing,
      tags = Prelude.Nothing,
      templateId = Prelude.Nothing,
      workflowInputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the application configured in Application Discovery Service.
updateWorkflowResponse_adsApplicationConfigurationId :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_adsApplicationConfigurationId = Lens.lens (\UpdateWorkflowResponse' {adsApplicationConfigurationId} -> adsApplicationConfigurationId) (\s@UpdateWorkflowResponse' {} a -> s {adsApplicationConfigurationId = a} :: UpdateWorkflowResponse)

-- | The Amazon Resource Name (ARN) of the migration workflow.
updateWorkflowResponse_arn :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_arn = Lens.lens (\UpdateWorkflowResponse' {arn} -> arn) (\s@UpdateWorkflowResponse' {} a -> s {arn = a} :: UpdateWorkflowResponse)

-- | The time at which the migration workflow was created.
updateWorkflowResponse_creationTime :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
updateWorkflowResponse_creationTime = Lens.lens (\UpdateWorkflowResponse' {creationTime} -> creationTime) (\s@UpdateWorkflowResponse' {} a -> s {creationTime = a} :: UpdateWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the migration workflow.
updateWorkflowResponse_description :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_description = Lens.lens (\UpdateWorkflowResponse' {description} -> description) (\s@UpdateWorkflowResponse' {} a -> s {description = a} :: UpdateWorkflowResponse)

-- | The ID of the migration workflow.
updateWorkflowResponse_id :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_id = Lens.lens (\UpdateWorkflowResponse' {id} -> id) (\s@UpdateWorkflowResponse' {} a -> s {id = a} :: UpdateWorkflowResponse)

-- | The time at which the migration workflow was last modified.
updateWorkflowResponse_lastModifiedTime :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.UTCTime)
updateWorkflowResponse_lastModifiedTime = Lens.lens (\UpdateWorkflowResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateWorkflowResponse' {} a -> s {lastModifiedTime = a} :: UpdateWorkflowResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the migration workflow.
updateWorkflowResponse_name :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_name = Lens.lens (\UpdateWorkflowResponse' {name} -> name) (\s@UpdateWorkflowResponse' {} a -> s {name = a} :: UpdateWorkflowResponse)

-- | The status of the migration workflow.
updateWorkflowResponse_status :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe MigrationWorkflowStatusEnum)
updateWorkflowResponse_status = Lens.lens (\UpdateWorkflowResponse' {status} -> status) (\s@UpdateWorkflowResponse' {} a -> s {status = a} :: UpdateWorkflowResponse)

-- | The servers on which a step will be run.
updateWorkflowResponse_stepTargets :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe [Prelude.Text])
updateWorkflowResponse_stepTargets = Lens.lens (\UpdateWorkflowResponse' {stepTargets} -> stepTargets) (\s@UpdateWorkflowResponse' {} a -> s {stepTargets = a} :: UpdateWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The tags added to the migration workflow.
updateWorkflowResponse_tags :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateWorkflowResponse_tags = Lens.lens (\UpdateWorkflowResponse' {tags} -> tags) (\s@UpdateWorkflowResponse' {} a -> s {tags = a} :: UpdateWorkflowResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the template.
updateWorkflowResponse_templateId :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe Prelude.Text)
updateWorkflowResponse_templateId = Lens.lens (\UpdateWorkflowResponse' {templateId} -> templateId) (\s@UpdateWorkflowResponse' {} a -> s {templateId = a} :: UpdateWorkflowResponse)

-- | The inputs required to update a migration workflow.
updateWorkflowResponse_workflowInputs :: Lens.Lens' UpdateWorkflowResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text StepInput))
updateWorkflowResponse_workflowInputs = Lens.lens (\UpdateWorkflowResponse' {workflowInputs} -> workflowInputs) (\s@UpdateWorkflowResponse' {} a -> s {workflowInputs = a} :: UpdateWorkflowResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
updateWorkflowResponse_httpStatus :: Lens.Lens' UpdateWorkflowResponse Prelude.Int
updateWorkflowResponse_httpStatus = Lens.lens (\UpdateWorkflowResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkflowResponse' {} a -> s {httpStatus = a} :: UpdateWorkflowResponse)

instance Prelude.NFData UpdateWorkflowResponse where
  rnf UpdateWorkflowResponse' {..} =
    Prelude.rnf adsApplicationConfigurationId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stepTargets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf workflowInputs
      `Prelude.seq` Prelude.rnf httpStatus
