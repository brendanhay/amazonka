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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an experiment template.
--
-- To create a template, specify the following information:
--
-- -   __Targets__: A target can be a specific resource in your AWS
--     environment, or one or more resources that match criteria that you
--     specify, for example, resources that have specific tags.
--
-- -   __Actions__: The actions to carry out on the target. You can specify
--     multiple actions, the duration of each action, and when to start
--     each action during an experiment.
--
-- -   __Stop conditions__: If a stop condition is triggered while an
--     experiment is running, the experiment is automatically stopped. You
--     can define a stop condition as a CloudWatch alarm.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/fis/latest/userguide/ AWS Fault Injection Simulator User Guide>.
module Amazonka.FIS.CreateExperimentTemplate
  ( -- * Creating a Request
    CreateExperimentTemplate (..),
    newCreateExperimentTemplate,

    -- * Request Lenses
    createExperimentTemplate_targets,
    createExperimentTemplate_tags,
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
import Amazonka.FIS.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExperimentTemplate' smart constructor.
data CreateExperimentTemplate = CreateExperimentTemplate'
  { -- | The targets for the experiment.
    targets :: Prelude.Maybe (Prelude.HashMap Prelude.Text CreateExperimentTemplateTargetInput),
    -- | The tags to apply to the experiment template.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text,
    -- | A description for the experiment template. Can contain up to 64 letters
    -- (A-Z and a-z).
    description :: Prelude.Text,
    -- | The stop conditions.
    stopConditions :: [CreateExperimentTemplateStopConditionInput],
    -- | The actions for the experiment.
    actions :: Prelude.HashMap Prelude.Text CreateExperimentTemplateActionInput,
    -- | The Amazon Resource Name (ARN) of an IAM role that grants the AWS FIS
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
-- 'targets', 'createExperimentTemplate_targets' - The targets for the experiment.
--
-- 'tags', 'createExperimentTemplate_tags' - The tags to apply to the experiment template.
--
-- 'clientToken', 'createExperimentTemplate_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'description', 'createExperimentTemplate_description' - A description for the experiment template. Can contain up to 64 letters
-- (A-Z and a-z).
--
-- 'stopConditions', 'createExperimentTemplate_stopConditions' - The stop conditions.
--
-- 'actions', 'createExperimentTemplate_actions' - The actions for the experiment.
--
-- 'roleArn', 'createExperimentTemplate_roleArn' - The Amazon Resource Name (ARN) of an IAM role that grants the AWS FIS
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
      { targets =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        clientToken = pClientToken_,
        description = pDescription_,
        stopConditions = Prelude.mempty,
        actions = Prelude.mempty,
        roleArn = pRoleArn_
      }

-- | The targets for the experiment.
createExperimentTemplate_targets :: Lens.Lens' CreateExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text CreateExperimentTemplateTargetInput))
createExperimentTemplate_targets = Lens.lens (\CreateExperimentTemplate' {targets} -> targets) (\s@CreateExperimentTemplate' {} a -> s {targets = a} :: CreateExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to the experiment template.
createExperimentTemplate_tags :: Lens.Lens' CreateExperimentTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createExperimentTemplate_tags = Lens.lens (\CreateExperimentTemplate' {tags} -> tags) (\s@CreateExperimentTemplate' {} a -> s {tags = a} :: CreateExperimentTemplate) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createExperimentTemplate_clientToken :: Lens.Lens' CreateExperimentTemplate Prelude.Text
createExperimentTemplate_clientToken = Lens.lens (\CreateExperimentTemplate' {clientToken} -> clientToken) (\s@CreateExperimentTemplate' {} a -> s {clientToken = a} :: CreateExperimentTemplate)

-- | A description for the experiment template. Can contain up to 64 letters
-- (A-Z and a-z).
createExperimentTemplate_description :: Lens.Lens' CreateExperimentTemplate Prelude.Text
createExperimentTemplate_description = Lens.lens (\CreateExperimentTemplate' {description} -> description) (\s@CreateExperimentTemplate' {} a -> s {description = a} :: CreateExperimentTemplate)

-- | The stop conditions.
createExperimentTemplate_stopConditions :: Lens.Lens' CreateExperimentTemplate [CreateExperimentTemplateStopConditionInput]
createExperimentTemplate_stopConditions = Lens.lens (\CreateExperimentTemplate' {stopConditions} -> stopConditions) (\s@CreateExperimentTemplate' {} a -> s {stopConditions = a} :: CreateExperimentTemplate) Prelude.. Lens.coerced

-- | The actions for the experiment.
createExperimentTemplate_actions :: Lens.Lens' CreateExperimentTemplate (Prelude.HashMap Prelude.Text CreateExperimentTemplateActionInput)
createExperimentTemplate_actions = Lens.lens (\CreateExperimentTemplate' {actions} -> actions) (\s@CreateExperimentTemplate' {} a -> s {actions = a} :: CreateExperimentTemplate) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of an IAM role that grants the AWS FIS
-- service permission to perform service actions on your behalf.
createExperimentTemplate_roleArn :: Lens.Lens' CreateExperimentTemplate Prelude.Text
createExperimentTemplate_roleArn = Lens.lens (\CreateExperimentTemplate' {roleArn} -> roleArn) (\s@CreateExperimentTemplate' {} a -> s {roleArn = a} :: CreateExperimentTemplate)

instance Core.AWSRequest CreateExperimentTemplate where
  type
    AWSResponse CreateExperimentTemplate =
      CreateExperimentTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExperimentTemplateResponse'
            Prelude.<$> (x Core..?> "experimentTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExperimentTemplate where
  hashWithSalt salt' CreateExperimentTemplate' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` stopConditions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targets

instance Prelude.NFData CreateExperimentTemplate where
  rnf CreateExperimentTemplate' {..} =
    Prelude.rnf targets
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf stopConditions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags

instance Core.ToHeaders CreateExperimentTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateExperimentTemplate where
  toJSON CreateExperimentTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("targets" Core..=) Prelude.<$> targets,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("clientToken" Core..= clientToken),
            Prelude.Just ("description" Core..= description),
            Prelude.Just
              ("stopConditions" Core..= stopConditions),
            Prelude.Just ("actions" Core..= actions),
            Prelude.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateExperimentTemplate where
  toPath = Prelude.const "/experimentTemplates"

instance Core.ToQuery CreateExperimentTemplate where
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
