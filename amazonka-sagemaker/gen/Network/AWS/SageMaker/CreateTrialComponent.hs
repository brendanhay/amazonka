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
-- Module      : Network.AWS.SageMaker.CreateTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a /trial component/, which is a stage of a machine learning
-- /trial/. A trial is composed of one or more trial components. A trial
-- component can be used in multiple trials.
--
-- Trial components include pre-processing jobs, training jobs, and batch
-- transform jobs.
--
-- When you use Amazon SageMaker Studio or the Amazon SageMaker Python SDK,
-- all experiments, trials, and trial components are automatically tracked,
-- logged, and indexed. When you use the AWS SDK for Python (Boto), you
-- must use the logging APIs provided by the SDK.
--
-- You can add tags to a trial component and then use the Search API to
-- search for the tags.
--
-- @CreateTrialComponent@ can only be invoked from within an Amazon
-- SageMaker managed environment. This includes Amazon SageMaker training
-- jobs, processing jobs, transform jobs, and Amazon SageMaker notebooks. A
-- call to @CreateTrialComponent@ from outside one of these environments
-- results in an error.
module Network.AWS.SageMaker.CreateTrialComponent
  ( -- * Creating a Request
    CreateTrialComponent (..),
    newCreateTrialComponent,

    -- * Request Lenses
    createTrialComponent_status,
    createTrialComponent_metadataProperties,
    createTrialComponent_startTime,
    createTrialComponent_endTime,
    createTrialComponent_tags,
    createTrialComponent_inputArtifacts,
    createTrialComponent_displayName,
    createTrialComponent_parameters,
    createTrialComponent_outputArtifacts,
    createTrialComponent_trialComponentName,

    -- * Destructuring the Response
    CreateTrialComponentResponse (..),
    newCreateTrialComponentResponse,

    -- * Response Lenses
    createTrialComponentResponse_trialComponentArn,
    createTrialComponentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateTrialComponent' smart constructor.
data CreateTrialComponent = CreateTrialComponent'
  { -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Core.Maybe TrialComponentStatus,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the component started.
    startTime :: Core.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | A list of tags to associate with the component. You can use Search API
    -- to search on the tags.
    tags :: Core.Maybe [Tag],
    -- | The input artifacts for the component. Examples of input artifacts are
    -- datasets, algorithms, hyperparameters, source code, and instance types.
    inputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The hyperparameters for the component.
    parameters :: Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue),
    -- | The output artifacts for the component. Examples of output artifacts are
    -- metrics, snapshots, logs, and images.
    outputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | The name of the component. The name must be unique in your AWS account
    -- and is not case-sensitive.
    trialComponentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createTrialComponent_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'metadataProperties', 'createTrialComponent_metadataProperties' - Undocumented member.
--
-- 'startTime', 'createTrialComponent_startTime' - When the component started.
--
-- 'endTime', 'createTrialComponent_endTime' - When the component ended.
--
-- 'tags', 'createTrialComponent_tags' - A list of tags to associate with the component. You can use Search API
-- to search on the tags.
--
-- 'inputArtifacts', 'createTrialComponent_inputArtifacts' - The input artifacts for the component. Examples of input artifacts are
-- datasets, algorithms, hyperparameters, source code, and instance types.
--
-- 'displayName', 'createTrialComponent_displayName' - The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
--
-- 'parameters', 'createTrialComponent_parameters' - The hyperparameters for the component.
--
-- 'outputArtifacts', 'createTrialComponent_outputArtifacts' - The output artifacts for the component. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
--
-- 'trialComponentName', 'createTrialComponent_trialComponentName' - The name of the component. The name must be unique in your AWS account
-- and is not case-sensitive.
newCreateTrialComponent ::
  -- | 'trialComponentName'
  Core.Text ->
  CreateTrialComponent
newCreateTrialComponent pTrialComponentName_ =
  CreateTrialComponent'
    { status = Core.Nothing,
      metadataProperties = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      tags = Core.Nothing,
      inputArtifacts = Core.Nothing,
      displayName = Core.Nothing,
      parameters = Core.Nothing,
      outputArtifacts = Core.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
createTrialComponent_status :: Lens.Lens' CreateTrialComponent (Core.Maybe TrialComponentStatus)
createTrialComponent_status = Lens.lens (\CreateTrialComponent' {status} -> status) (\s@CreateTrialComponent' {} a -> s {status = a} :: CreateTrialComponent)

-- | Undocumented member.
createTrialComponent_metadataProperties :: Lens.Lens' CreateTrialComponent (Core.Maybe MetadataProperties)
createTrialComponent_metadataProperties = Lens.lens (\CreateTrialComponent' {metadataProperties} -> metadataProperties) (\s@CreateTrialComponent' {} a -> s {metadataProperties = a} :: CreateTrialComponent)

-- | When the component started.
createTrialComponent_startTime :: Lens.Lens' CreateTrialComponent (Core.Maybe Core.UTCTime)
createTrialComponent_startTime = Lens.lens (\CreateTrialComponent' {startTime} -> startTime) (\s@CreateTrialComponent' {} a -> s {startTime = a} :: CreateTrialComponent) Core.. Lens.mapping Core._Time

-- | When the component ended.
createTrialComponent_endTime :: Lens.Lens' CreateTrialComponent (Core.Maybe Core.UTCTime)
createTrialComponent_endTime = Lens.lens (\CreateTrialComponent' {endTime} -> endTime) (\s@CreateTrialComponent' {} a -> s {endTime = a} :: CreateTrialComponent) Core.. Lens.mapping Core._Time

-- | A list of tags to associate with the component. You can use Search API
-- to search on the tags.
createTrialComponent_tags :: Lens.Lens' CreateTrialComponent (Core.Maybe [Tag])
createTrialComponent_tags = Lens.lens (\CreateTrialComponent' {tags} -> tags) (\s@CreateTrialComponent' {} a -> s {tags = a} :: CreateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The input artifacts for the component. Examples of input artifacts are
-- datasets, algorithms, hyperparameters, source code, and instance types.
createTrialComponent_inputArtifacts :: Lens.Lens' CreateTrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
createTrialComponent_inputArtifacts = Lens.lens (\CreateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@CreateTrialComponent' {} a -> s {inputArtifacts = a} :: CreateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
createTrialComponent_displayName :: Lens.Lens' CreateTrialComponent (Core.Maybe Core.Text)
createTrialComponent_displayName = Lens.lens (\CreateTrialComponent' {displayName} -> displayName) (\s@CreateTrialComponent' {} a -> s {displayName = a} :: CreateTrialComponent)

-- | The hyperparameters for the component.
createTrialComponent_parameters :: Lens.Lens' CreateTrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue))
createTrialComponent_parameters = Lens.lens (\CreateTrialComponent' {parameters} -> parameters) (\s@CreateTrialComponent' {} a -> s {parameters = a} :: CreateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The output artifacts for the component. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
createTrialComponent_outputArtifacts :: Lens.Lens' CreateTrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
createTrialComponent_outputArtifacts = Lens.lens (\CreateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@CreateTrialComponent' {} a -> s {outputArtifacts = a} :: CreateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The name of the component. The name must be unique in your AWS account
-- and is not case-sensitive.
createTrialComponent_trialComponentName :: Lens.Lens' CreateTrialComponent Core.Text
createTrialComponent_trialComponentName = Lens.lens (\CreateTrialComponent' {trialComponentName} -> trialComponentName) (\s@CreateTrialComponent' {} a -> s {trialComponentName = a} :: CreateTrialComponent)

instance Core.AWSRequest CreateTrialComponent where
  type
    AWSResponse CreateTrialComponent =
      CreateTrialComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrialComponentResponse'
            Core.<$> (x Core..?> "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTrialComponent

instance Core.NFData CreateTrialComponent

instance Core.ToHeaders CreateTrialComponent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateTrialComponent" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTrialComponent where
  toJSON CreateTrialComponent' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Status" Core..=) Core.<$> status,
            ("MetadataProperties" Core..=)
              Core.<$> metadataProperties,
            ("StartTime" Core..=) Core.<$> startTime,
            ("EndTime" Core..=) Core.<$> endTime,
            ("Tags" Core..=) Core.<$> tags,
            ("InputArtifacts" Core..=) Core.<$> inputArtifacts,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("Parameters" Core..=) Core.<$> parameters,
            ("OutputArtifacts" Core..=) Core.<$> outputArtifacts,
            Core.Just
              ("TrialComponentName" Core..= trialComponentName)
          ]
      )

instance Core.ToPath CreateTrialComponent where
  toPath = Core.const "/"

instance Core.ToQuery CreateTrialComponent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTrialComponentResponse' smart constructor.
data CreateTrialComponentResponse = CreateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentArn', 'createTrialComponentResponse_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'httpStatus', 'createTrialComponentResponse_httpStatus' - The response's http status code.
newCreateTrialComponentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTrialComponentResponse
newCreateTrialComponentResponse pHttpStatus_ =
  CreateTrialComponentResponse'
    { trialComponentArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
createTrialComponentResponse_trialComponentArn :: Lens.Lens' CreateTrialComponentResponse (Core.Maybe Core.Text)
createTrialComponentResponse_trialComponentArn = Lens.lens (\CreateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@CreateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: CreateTrialComponentResponse)

-- | The response's http status code.
createTrialComponentResponse_httpStatus :: Lens.Lens' CreateTrialComponentResponse Core.Int
createTrialComponentResponse_httpStatus = Lens.lens (\CreateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@CreateTrialComponentResponse' {} a -> s {httpStatus = a} :: CreateTrialComponentResponse)

instance Core.NFData CreateTrialComponentResponse
