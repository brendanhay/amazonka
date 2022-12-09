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
-- Module      : Amazonka.FIS.CreateExperimentTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an experiment template.
--
-- An experiment template includes the following components:
--
-- -   __Targets__: A target can be a specific resource in your Amazon Web
--     Services environment, or one or more resources that match criteria
--     that you specify, for example, resources that have specific tags.
--
-- -   __Actions__: The actions to carry out on the target. You can specify
--     multiple actions, the duration of each action, and when to start
--     each action during an experiment.
--
-- -   __Stop conditions__: If a stop condition is triggered while an
--     experiment is running, the experiment is automatically stopped. You
--     can define a stop condition as a CloudWatch alarm.
--
-- For more information, see
-- <https://docs.aws.amazon.com/fis/latest/userguide/experiment-templates.html Experiment templates>
-- in the /Fault Injection Simulator User Guide/.
module Amazonka.FIS.CreateExperimentTemplate
  ( -- * Creating a Request
    CreateExperimentTemplate (..),
    newCreateExperimentTemplate,

    -- * Request Lenses
    createExperimentTemplate_logConfiguration,
    createExperimentTemplate_tags,
    createExperimentTemplate_targets,
    createExperimentTemplate_clientToken,
    createExperimentTemplate_description,
    createExperimentTemplate_stopConditions,
    createExperimentTemplate_actions,
    createExperimentTemplate_roleArn,

    -- * Destructuring the Response
    CreateExperimentTemplateResponse (..),
    newCreateExperimentTemplateResponse,

    -- * Response Lenses
    createExperimentTemplateResponse_experimentTemplate,
    createExperimentTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExperimentTemplate' smart constructor.
data CreateExperimentTemplate = CreateExperimentTemplate'
  { -- | The configuration for experiment logging.
    logConfiguration :: Prelude.Maybe CreateExperimentTemplateLogConfigurationInput,
    -- | The tags to apply to the experiment template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The targets for the experiment.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text CreateExperimentTemplateTargetInput),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text,
    -- | A description for the experiment template.
    description :: Prelude.Text,
    -- | The stop conditions.
    stopConditions :: [CreateExperimentTemplateStopConditionInput],
    -- | The actions for the experiment.
    actions :: Prelude.HashMap Prelude.Text CreateExperimentTemplateActionInput,
    -- | The Amazon Resource Name (ARN) of an IAM role that grants the FIS
    -- service permission to perform service actions on your behalf.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logConfiguration', 'createExperimentTemplate_logConfiguration' - The configuration for experiment logging.
--
-- 'tags', 'createExperimentTemplate_tags' - The tags to apply to the experiment template.
--
-- 'targets', 'createExperimentTemplate_targets' - The targets for the experiment.
--
-- 'clientToken', 'createExperimentTemplate_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createExperimentTemplate_description' - A description for the experiment template.
--
-- 'stopConditions', 'createExperimentTemplate_stopConditions' - The stop conditions.
--
-- 'actions', 'createExperimentTemplate_actions' - The actions for the experiment.
--
-- 'roleArn', 'createExperimentTemplate_roleArn' - The Amazon Resource Name (ARN) of an IAM role that grants the FIS
-- service permission to perform service actions on your behalf.
newCreateExperimentTemplate ::
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateExperimentTemplate
newCreateExperimentTemplate
  pClientToken_
  pDescription_
  pRoleArn_ =
    CreateExperimentTemplate'
      { logConfiguration =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        targets = Prelude.Nothing,
        clientToken = pClientToken_,
        description = pDescription_,
        stopConditions = Prelude.mempty,
        actions = Prelude.mempty,
        roleArn = pRoleArn_
      }

-- | The configuration for experiment logging.
createExperimentTemplate_logConfiguration :: Lens.Lens' CreateExperimentTemplate (Prelude.Maybe CreateExperimentTemplateLogConfigurationInput)
createExperimentTemplate_logConfiguration = Lens.lens (\CreateExperimentTemplate' {logConfiguration} -> logConfiguration) (\s@CreateExperimentTemplate' {} a -> s {logConfiguration = a} :: CreateExperimentTemplate)

-- | The tags to apply to the experiment template.
createExperimentTemplate_tags :: Lens.Lens' CreateExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperimentTemplate_tags = Lens.lens (\CreateExperimentTemplate' {tags} -> tags) (\s@CreateExperimentTemplate' {} a -> s {tags = a} :: CreateExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The targets for the experiment.
createExperimentTemplate_targets :: Lens.Lens' CreateExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text CreateExperimentTemplateTargetInput))
createExperimentTemplate_targets = Lens.lens (\CreateExperimentTemplate' {targets} -> targets) (\s@CreateExperimentTemplate' {} a -> s {targets = a} :: CreateExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createExperimentTemplate_clientToken :: Lens.Lens' CreateExperimentTemplate Prelude.Text
createExperimentTemplate_clientToken = Lens.lens (\CreateExperimentTemplate' {clientToken} -> clientToken) (\s@CreateExperimentTemplate' {} a -> s {clientToken = a} :: CreateExperimentTemplate)

-- | A description for the experiment template.
createExperimentTemplate_description :: Lens.Lens' CreateExperimentTemplate Prelude.Text
createExperimentTemplate_description = Lens.lens (\CreateExperimentTemplate' {description} -> description) (\s@CreateExperimentTemplate' {} a -> s {description = a} :: CreateExperimentTemplate)

-- | The stop conditions.
createExperimentTemplate_stopConditions :: Lens.Lens' CreateExperimentTemplate [CreateExperimentTemplateStopConditionInput]
createExperimentTemplate_stopConditions = Lens.lens (\CreateExperimentTemplate' {stopConditions} -> stopConditions) (\s@CreateExperimentTemplate' {} a -> s {stopConditions = a} :: CreateExperimentTemplate) Prelude.. Lens.coerced

-- | The actions for the experiment.
createExperimentTemplate_actions :: Lens.Lens' CreateExperimentTemplate (Prelude.HashMap Prelude.Text CreateExperimentTemplateActionInput)
createExperimentTemplate_actions = Lens.lens (\CreateExperimentTemplate' {actions} -> actions) (\s@CreateExperimentTemplate' {} a -> s {actions = a} :: CreateExperimentTemplate) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role that grants the FIS
-- service permission to perform service actions on your behalf.
createExperimentTemplate_roleArn :: Lens.Lens' CreateExperimentTemplate Prelude.Text
createExperimentTemplate_roleArn = Lens.lens (\CreateExperimentTemplate' {roleArn} -> roleArn) (\s@CreateExperimentTemplate' {} a -> s {roleArn = a} :: CreateExperimentTemplate)

instance Core.AWSRequest CreateExperimentTemplate where
  type
    AWSResponse CreateExperimentTemplate =
      CreateExperimentTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExperimentTemplateResponse'
            Prelude.<$> (x Data..?> "experimentTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExperimentTemplate where
  hashWithSalt _salt CreateExperimentTemplate' {..} =
    _salt `Prelude.hashWithSalt` logConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` stopConditions
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateExperimentTemplate where
  rnf CreateExperimentTemplate' {..} =
    Prelude.rnf logConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf stopConditions
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateExperimentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExperimentTemplate where
  toJSON CreateExperimentTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("logConfiguration" Data..=)
              Prelude.<$> logConfiguration,
            ("tags" Data..=) Prelude.<$> tags,
            ("targets" Data..=) Prelude.<$> targets,
            Prelude.Just ("clientToken" Data..= clientToken),
            Prelude.Just ("description" Data..= description),
            Prelude.Just
              ("stopConditions" Data..= stopConditions),
            Prelude.Just ("actions" Data..= actions),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateExperimentTemplate where
  toPath = Prelude.const "/experimentTemplates"

instance Data.ToQuery CreateExperimentTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExperimentTemplateResponse' smart constructor.
data CreateExperimentTemplateResponse = CreateExperimentTemplateResponse'
  { -- | Information about the experiment template.
    experimentTemplate :: Prelude.Maybe ExperimentTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExperimentTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentTemplate', 'createExperimentTemplateResponse_experimentTemplate' - Information about the experiment template.
--
-- 'httpStatus', 'createExperimentTemplateResponse_httpStatus' - The response's http status code.
newCreateExperimentTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateExperimentTemplateResponse
newCreateExperimentTemplateResponse pHttpStatus_ =
  CreateExperimentTemplateResponse'
    { experimentTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the experiment template.
createExperimentTemplateResponse_experimentTemplate :: Lens.Lens' CreateExperimentTemplateResponse (Prelude.Maybe ExperimentTemplate)
createExperimentTemplateResponse_experimentTemplate = Lens.lens (\CreateExperimentTemplateResponse' {experimentTemplate} -> experimentTemplate) (\s@CreateExperimentTemplateResponse' {} a -> s {experimentTemplate = a} :: CreateExperimentTemplateResponse)

-- | The response's http status code.
createExperimentTemplateResponse_httpStatus :: Lens.Lens' CreateExperimentTemplateResponse Prelude.Int
createExperimentTemplateResponse_httpStatus = Lens.lens (\CreateExperimentTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateExperimentTemplateResponse' {} a -> s {httpStatus = a} :: CreateExperimentTemplateResponse)

instance
  Prelude.NFData
    CreateExperimentTemplateResponse
  where
  rnf CreateExperimentTemplateResponse' {..} =
    Prelude.rnf experimentTemplate
      `Prelude.seq` Prelude.rnf httpStatus
