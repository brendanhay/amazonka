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
-- Module      : Amazonka.MigrationHubOrchestrator.GetTemplateStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a specific step in a template.
module Amazonka.MigrationHubOrchestrator.GetTemplateStep
  ( -- * Creating a Request
    GetTemplateStep (..),
    newGetTemplateStep,

    -- * Request Lenses
    getTemplateStep_id,
    getTemplateStep_templateId,
    getTemplateStep_stepGroupId,

    -- * Destructuring the Response
    GetTemplateStepResponse (..),
    newGetTemplateStepResponse,

    -- * Response Lenses
    getTemplateStepResponse_creationTime,
    getTemplateStepResponse_description,
    getTemplateStepResponse_id,
    getTemplateStepResponse_name,
    getTemplateStepResponse_next,
    getTemplateStepResponse_outputs,
    getTemplateStepResponse_previous,
    getTemplateStepResponse_stepActionType,
    getTemplateStepResponse_stepAutomationConfiguration,
    getTemplateStepResponse_stepGroupId,
    getTemplateStepResponse_templateId,
    getTemplateStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTemplateStep' smart constructor.
data GetTemplateStep = GetTemplateStep'
  { -- | The ID of the step.
    id :: Prelude.Text,
    -- | The ID of the template.
    templateId :: Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getTemplateStep_id' - The ID of the step.
--
-- 'templateId', 'getTemplateStep_templateId' - The ID of the template.
--
-- 'stepGroupId', 'getTemplateStep_stepGroupId' - The ID of the step group.
newGetTemplateStep ::
  -- | 'id'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  GetTemplateStep
newGetTemplateStep pId_ pTemplateId_ pStepGroupId_ =
  GetTemplateStep'
    { id = pId_,
      templateId = pTemplateId_,
      stepGroupId = pStepGroupId_
    }

-- | The ID of the step.
getTemplateStep_id :: Lens.Lens' GetTemplateStep Prelude.Text
getTemplateStep_id = Lens.lens (\GetTemplateStep' {id} -> id) (\s@GetTemplateStep' {} a -> s {id = a} :: GetTemplateStep)

-- | The ID of the template.
getTemplateStep_templateId :: Lens.Lens' GetTemplateStep Prelude.Text
getTemplateStep_templateId = Lens.lens (\GetTemplateStep' {templateId} -> templateId) (\s@GetTemplateStep' {} a -> s {templateId = a} :: GetTemplateStep)

-- | The ID of the step group.
getTemplateStep_stepGroupId :: Lens.Lens' GetTemplateStep Prelude.Text
getTemplateStep_stepGroupId = Lens.lens (\GetTemplateStep' {stepGroupId} -> stepGroupId) (\s@GetTemplateStep' {} a -> s {stepGroupId = a} :: GetTemplateStep)

instance Core.AWSRequest GetTemplateStep where
  type
    AWSResponse GetTemplateStep =
      GetTemplateStepResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTemplateStepResponse'
            Prelude.<$> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "next" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "previous" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "stepActionType")
            Prelude.<*> (x Data..?> "stepAutomationConfiguration")
            Prelude.<*> (x Data..?> "stepGroupId")
            Prelude.<*> (x Data..?> "templateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTemplateStep where
  hashWithSalt _salt GetTemplateStep' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` stepGroupId

instance Prelude.NFData GetTemplateStep where
  rnf GetTemplateStep' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf stepGroupId

instance Data.ToHeaders GetTemplateStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTemplateStep where
  toPath GetTemplateStep' {..} =
    Prelude.mconcat ["/templatestep/", Data.toBS id]

instance Data.ToQuery GetTemplateStep where
  toQuery GetTemplateStep' {..} =
    Prelude.mconcat
      [ "templateId" Data.=: templateId,
        "stepGroupId" Data.=: stepGroupId
      ]

-- | /See:/ 'newGetTemplateStepResponse' smart constructor.
data GetTemplateStepResponse = GetTemplateStepResponse'
  { -- | The time at which the step was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The description of the step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The outputs of the step.
    outputs :: Prelude.Maybe [StepOutput],
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The action type of the step. You must run and update the status of a
    -- manual step for the workflow to continue after the completion of the
    -- step.
    stepActionType :: Prelude.Maybe StepActionType,
    -- | The custom script to run tests on source or target environments.
    stepAutomationConfiguration :: Prelude.Maybe StepAutomationConfiguration,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTemplateStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'getTemplateStepResponse_creationTime' - The time at which the step was created.
--
-- 'description', 'getTemplateStepResponse_description' - The description of the step.
--
-- 'id', 'getTemplateStepResponse_id' - The ID of the step.
--
-- 'name', 'getTemplateStepResponse_name' - The name of the step.
--
-- 'next', 'getTemplateStepResponse_next' - The next step.
--
-- 'outputs', 'getTemplateStepResponse_outputs' - The outputs of the step.
--
-- 'previous', 'getTemplateStepResponse_previous' - The previous step.
--
-- 'stepActionType', 'getTemplateStepResponse_stepActionType' - The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
--
-- 'stepAutomationConfiguration', 'getTemplateStepResponse_stepAutomationConfiguration' - The custom script to run tests on source or target environments.
--
-- 'stepGroupId', 'getTemplateStepResponse_stepGroupId' - The ID of the step group.
--
-- 'templateId', 'getTemplateStepResponse_templateId' - The ID of the template.
--
-- 'httpStatus', 'getTemplateStepResponse_httpStatus' - The response's http status code.
newGetTemplateStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTemplateStepResponse
newGetTemplateStepResponse pHttpStatus_ =
  GetTemplateStepResponse'
    { creationTime =
        Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      outputs = Prelude.Nothing,
      previous = Prelude.Nothing,
      stepActionType = Prelude.Nothing,
      stepAutomationConfiguration = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      templateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time at which the step was created.
getTemplateStepResponse_creationTime :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe Prelude.Text)
getTemplateStepResponse_creationTime = Lens.lens (\GetTemplateStepResponse' {creationTime} -> creationTime) (\s@GetTemplateStepResponse' {} a -> s {creationTime = a} :: GetTemplateStepResponse)

-- | The description of the step.
getTemplateStepResponse_description :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe Prelude.Text)
getTemplateStepResponse_description = Lens.lens (\GetTemplateStepResponse' {description} -> description) (\s@GetTemplateStepResponse' {} a -> s {description = a} :: GetTemplateStepResponse)

-- | The ID of the step.
getTemplateStepResponse_id :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe Prelude.Text)
getTemplateStepResponse_id = Lens.lens (\GetTemplateStepResponse' {id} -> id) (\s@GetTemplateStepResponse' {} a -> s {id = a} :: GetTemplateStepResponse)

-- | The name of the step.
getTemplateStepResponse_name :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe Prelude.Text)
getTemplateStepResponse_name = Lens.lens (\GetTemplateStepResponse' {name} -> name) (\s@GetTemplateStepResponse' {} a -> s {name = a} :: GetTemplateStepResponse)

-- | The next step.
getTemplateStepResponse_next :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe [Prelude.Text])
getTemplateStepResponse_next = Lens.lens (\GetTemplateStepResponse' {next} -> next) (\s@GetTemplateStepResponse' {} a -> s {next = a} :: GetTemplateStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The outputs of the step.
getTemplateStepResponse_outputs :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe [StepOutput])
getTemplateStepResponse_outputs = Lens.lens (\GetTemplateStepResponse' {outputs} -> outputs) (\s@GetTemplateStepResponse' {} a -> s {outputs = a} :: GetTemplateStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The previous step.
getTemplateStepResponse_previous :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe [Prelude.Text])
getTemplateStepResponse_previous = Lens.lens (\GetTemplateStepResponse' {previous} -> previous) (\s@GetTemplateStepResponse' {} a -> s {previous = a} :: GetTemplateStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
getTemplateStepResponse_stepActionType :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe StepActionType)
getTemplateStepResponse_stepActionType = Lens.lens (\GetTemplateStepResponse' {stepActionType} -> stepActionType) (\s@GetTemplateStepResponse' {} a -> s {stepActionType = a} :: GetTemplateStepResponse)

-- | The custom script to run tests on source or target environments.
getTemplateStepResponse_stepAutomationConfiguration :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe StepAutomationConfiguration)
getTemplateStepResponse_stepAutomationConfiguration = Lens.lens (\GetTemplateStepResponse' {stepAutomationConfiguration} -> stepAutomationConfiguration) (\s@GetTemplateStepResponse' {} a -> s {stepAutomationConfiguration = a} :: GetTemplateStepResponse)

-- | The ID of the step group.
getTemplateStepResponse_stepGroupId :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe Prelude.Text)
getTemplateStepResponse_stepGroupId = Lens.lens (\GetTemplateStepResponse' {stepGroupId} -> stepGroupId) (\s@GetTemplateStepResponse' {} a -> s {stepGroupId = a} :: GetTemplateStepResponse)

-- | The ID of the template.
getTemplateStepResponse_templateId :: Lens.Lens' GetTemplateStepResponse (Prelude.Maybe Prelude.Text)
getTemplateStepResponse_templateId = Lens.lens (\GetTemplateStepResponse' {templateId} -> templateId) (\s@GetTemplateStepResponse' {} a -> s {templateId = a} :: GetTemplateStepResponse)

-- | The response's http status code.
getTemplateStepResponse_httpStatus :: Lens.Lens' GetTemplateStepResponse Prelude.Int
getTemplateStepResponse_httpStatus = Lens.lens (\GetTemplateStepResponse' {httpStatus} -> httpStatus) (\s@GetTemplateStepResponse' {} a -> s {httpStatus = a} :: GetTemplateStepResponse)

instance Prelude.NFData GetTemplateStepResponse where
  rnf GetTemplateStepResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf stepActionType
      `Prelude.seq` Prelude.rnf stepAutomationConfiguration
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf httpStatus
