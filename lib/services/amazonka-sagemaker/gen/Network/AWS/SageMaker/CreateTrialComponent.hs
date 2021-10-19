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
-- When you use SageMaker Studio or the SageMaker Python SDK, all
-- experiments, trials, and trial components are automatically tracked,
-- logged, and indexed. When you use the Amazon Web Services SDK for Python
-- (Boto), you must use the logging APIs provided by the SDK.
--
-- You can add tags to a trial component and then use the Search API to
-- search for the tags.
module Network.AWS.SageMaker.CreateTrialComponent
  ( -- * Creating a Request
    CreateTrialComponent (..),
    newCreateTrialComponent,

    -- * Request Lenses
    createTrialComponent_metadataProperties,
    createTrialComponent_status,
    createTrialComponent_outputArtifacts,
    createTrialComponent_startTime,
    createTrialComponent_endTime,
    createTrialComponent_parameters,
    createTrialComponent_displayName,
    createTrialComponent_inputArtifacts,
    createTrialComponent_tags,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newCreateTrialComponent' smart constructor.
data CreateTrialComponent = CreateTrialComponent'
  { metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Prelude.Maybe TrialComponentStatus,
    -- | The output artifacts for the component. Examples of output artifacts are
    -- metrics, snapshots, logs, and images.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The hyperparameters for the component.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The input artifacts for the component. Examples of input artifacts are
    -- datasets, algorithms, hyperparameters, source code, and instance types.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | A list of tags to associate with the component. You can use Search API
    -- to search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the component. The name must be unique in your Amazon Web
    -- Services account and is not case-sensitive.
    trialComponentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataProperties', 'createTrialComponent_metadataProperties' - Undocumented member.
--
-- 'status', 'createTrialComponent_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'outputArtifacts', 'createTrialComponent_outputArtifacts' - The output artifacts for the component. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
--
-- 'startTime', 'createTrialComponent_startTime' - When the component started.
--
-- 'endTime', 'createTrialComponent_endTime' - When the component ended.
--
-- 'parameters', 'createTrialComponent_parameters' - The hyperparameters for the component.
--
-- 'displayName', 'createTrialComponent_displayName' - The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
--
-- 'inputArtifacts', 'createTrialComponent_inputArtifacts' - The input artifacts for the component. Examples of input artifacts are
-- datasets, algorithms, hyperparameters, source code, and instance types.
--
-- 'tags', 'createTrialComponent_tags' - A list of tags to associate with the component. You can use Search API
-- to search on the tags.
--
-- 'trialComponentName', 'createTrialComponent_trialComponentName' - The name of the component. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
newCreateTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  CreateTrialComponent
newCreateTrialComponent pTrialComponentName_ =
  CreateTrialComponent'
    { metadataProperties =
        Prelude.Nothing,
      status = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      parameters = Prelude.Nothing,
      displayName = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      tags = Prelude.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | Undocumented member.
createTrialComponent_metadataProperties :: Lens.Lens' CreateTrialComponent (Prelude.Maybe MetadataProperties)
createTrialComponent_metadataProperties = Lens.lens (\CreateTrialComponent' {metadataProperties} -> metadataProperties) (\s@CreateTrialComponent' {} a -> s {metadataProperties = a} :: CreateTrialComponent)

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
createTrialComponent_status :: Lens.Lens' CreateTrialComponent (Prelude.Maybe TrialComponentStatus)
createTrialComponent_status = Lens.lens (\CreateTrialComponent' {status} -> status) (\s@CreateTrialComponent' {} a -> s {status = a} :: CreateTrialComponent)

-- | The output artifacts for the component. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
createTrialComponent_outputArtifacts :: Lens.Lens' CreateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
createTrialComponent_outputArtifacts = Lens.lens (\CreateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@CreateTrialComponent' {} a -> s {outputArtifacts = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component started.
createTrialComponent_startTime :: Lens.Lens' CreateTrialComponent (Prelude.Maybe Prelude.UTCTime)
createTrialComponent_startTime = Lens.lens (\CreateTrialComponent' {startTime} -> startTime) (\s@CreateTrialComponent' {} a -> s {startTime = a} :: CreateTrialComponent) Prelude.. Lens.mapping Core._Time

-- | When the component ended.
createTrialComponent_endTime :: Lens.Lens' CreateTrialComponent (Prelude.Maybe Prelude.UTCTime)
createTrialComponent_endTime = Lens.lens (\CreateTrialComponent' {endTime} -> endTime) (\s@CreateTrialComponent' {} a -> s {endTime = a} :: CreateTrialComponent) Prelude.. Lens.mapping Core._Time

-- | The hyperparameters for the component.
createTrialComponent_parameters :: Lens.Lens' CreateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
createTrialComponent_parameters = Lens.lens (\CreateTrialComponent' {parameters} -> parameters) (\s@CreateTrialComponent' {} a -> s {parameters = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
createTrialComponent_displayName :: Lens.Lens' CreateTrialComponent (Prelude.Maybe Prelude.Text)
createTrialComponent_displayName = Lens.lens (\CreateTrialComponent' {displayName} -> displayName) (\s@CreateTrialComponent' {} a -> s {displayName = a} :: CreateTrialComponent)

-- | The input artifacts for the component. Examples of input artifacts are
-- datasets, algorithms, hyperparameters, source code, and instance types.
createTrialComponent_inputArtifacts :: Lens.Lens' CreateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
createTrialComponent_inputArtifacts = Lens.lens (\CreateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@CreateTrialComponent' {} a -> s {inputArtifacts = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags to associate with the component. You can use Search API
-- to search on the tags.
createTrialComponent_tags :: Lens.Lens' CreateTrialComponent (Prelude.Maybe [Tag])
createTrialComponent_tags = Lens.lens (\CreateTrialComponent' {tags} -> tags) (\s@CreateTrialComponent' {} a -> s {tags = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the component. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
createTrialComponent_trialComponentName :: Lens.Lens' CreateTrialComponent Prelude.Text
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
            Prelude.<$> (x Core..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrialComponent

instance Prelude.NFData CreateTrialComponent

instance Core.ToHeaders CreateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateTrialComponent where
  toJSON CreateTrialComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MetadataProperties" Core..=)
              Prelude.<$> metadataProperties,
            ("Status" Core..=) Prelude.<$> status,
            ("OutputArtifacts" Core..=)
              Prelude.<$> outputArtifacts,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("InputArtifacts" Core..=)
              Prelude.<$> inputArtifacts,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("TrialComponentName" Core..= trialComponentName)
          ]
      )

instance Core.ToPath CreateTrialComponent where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTrialComponentResponse' smart constructor.
data CreateTrialComponentResponse = CreateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateTrialComponentResponse
newCreateTrialComponentResponse pHttpStatus_ =
  CreateTrialComponentResponse'
    { trialComponentArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
createTrialComponentResponse_trialComponentArn :: Lens.Lens' CreateTrialComponentResponse (Prelude.Maybe Prelude.Text)
createTrialComponentResponse_trialComponentArn = Lens.lens (\CreateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@CreateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: CreateTrialComponentResponse)

-- | The response's http status code.
createTrialComponentResponse_httpStatus :: Lens.Lens' CreateTrialComponentResponse Prelude.Int
createTrialComponentResponse_httpStatus = Lens.lens (\CreateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@CreateTrialComponentResponse' {} a -> s {httpStatus = a} :: CreateTrialComponentResponse)

instance Prelude.NFData CreateTrialComponentResponse
