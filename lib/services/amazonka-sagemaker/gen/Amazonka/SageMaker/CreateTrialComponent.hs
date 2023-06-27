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
-- Module      : Amazonka.SageMaker.CreateTrialComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- You can add tags to a trial component and then use the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_Search.html Search>
-- API to search for the tags.
module Amazonka.SageMaker.CreateTrialComponent
  ( -- * Creating a Request
    CreateTrialComponent (..),
    newCreateTrialComponent,

    -- * Request Lenses
    createTrialComponent_displayName,
    createTrialComponent_endTime,
    createTrialComponent_inputArtifacts,
    createTrialComponent_metadataProperties,
    createTrialComponent_outputArtifacts,
    createTrialComponent_parameters,
    createTrialComponent_startTime,
    createTrialComponent_status,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateTrialComponent' smart constructor.
data CreateTrialComponent = CreateTrialComponent'
  { -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | When the component ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The input artifacts for the component. Examples of input artifacts are
    -- datasets, algorithms, hyperparameters, source code, and instance types.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The output artifacts for the component. Examples of output artifacts are
    -- metrics, snapshots, logs, and images.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The hyperparameters for the component.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | When the component started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Prelude.Maybe TrialComponentStatus,
    -- | A list of tags to associate with the component. You can use
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_Search.html Search>
    -- API to search on the tags.
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
-- 'displayName', 'createTrialComponent_displayName' - The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
--
-- 'endTime', 'createTrialComponent_endTime' - When the component ended.
--
-- 'inputArtifacts', 'createTrialComponent_inputArtifacts' - The input artifacts for the component. Examples of input artifacts are
-- datasets, algorithms, hyperparameters, source code, and instance types.
--
-- 'metadataProperties', 'createTrialComponent_metadataProperties' - Undocumented member.
--
-- 'outputArtifacts', 'createTrialComponent_outputArtifacts' - The output artifacts for the component. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
--
-- 'parameters', 'createTrialComponent_parameters' - The hyperparameters for the component.
--
-- 'startTime', 'createTrialComponent_startTime' - When the component started.
--
-- 'status', 'createTrialComponent_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'tags', 'createTrialComponent_tags' - A list of tags to associate with the component. You can use
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_Search.html Search>
-- API to search on the tags.
--
-- 'trialComponentName', 'createTrialComponent_trialComponentName' - The name of the component. The name must be unique in your Amazon Web
-- Services account and is not case-sensitive.
newCreateTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  CreateTrialComponent
newCreateTrialComponent pTrialComponentName_ =
  CreateTrialComponent'
    { displayName =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      parameters = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
createTrialComponent_displayName :: Lens.Lens' CreateTrialComponent (Prelude.Maybe Prelude.Text)
createTrialComponent_displayName = Lens.lens (\CreateTrialComponent' {displayName} -> displayName) (\s@CreateTrialComponent' {} a -> s {displayName = a} :: CreateTrialComponent)

-- | When the component ended.
createTrialComponent_endTime :: Lens.Lens' CreateTrialComponent (Prelude.Maybe Prelude.UTCTime)
createTrialComponent_endTime = Lens.lens (\CreateTrialComponent' {endTime} -> endTime) (\s@CreateTrialComponent' {} a -> s {endTime = a} :: CreateTrialComponent) Prelude.. Lens.mapping Data._Time

-- | The input artifacts for the component. Examples of input artifacts are
-- datasets, algorithms, hyperparameters, source code, and instance types.
createTrialComponent_inputArtifacts :: Lens.Lens' CreateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
createTrialComponent_inputArtifacts = Lens.lens (\CreateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@CreateTrialComponent' {} a -> s {inputArtifacts = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createTrialComponent_metadataProperties :: Lens.Lens' CreateTrialComponent (Prelude.Maybe MetadataProperties)
createTrialComponent_metadataProperties = Lens.lens (\CreateTrialComponent' {metadataProperties} -> metadataProperties) (\s@CreateTrialComponent' {} a -> s {metadataProperties = a} :: CreateTrialComponent)

-- | The output artifacts for the component. Examples of output artifacts are
-- metrics, snapshots, logs, and images.
createTrialComponent_outputArtifacts :: Lens.Lens' CreateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
createTrialComponent_outputArtifacts = Lens.lens (\CreateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@CreateTrialComponent' {} a -> s {outputArtifacts = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The hyperparameters for the component.
createTrialComponent_parameters :: Lens.Lens' CreateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
createTrialComponent_parameters = Lens.lens (\CreateTrialComponent' {parameters} -> parameters) (\s@CreateTrialComponent' {} a -> s {parameters = a} :: CreateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component started.
createTrialComponent_startTime :: Lens.Lens' CreateTrialComponent (Prelude.Maybe Prelude.UTCTime)
createTrialComponent_startTime = Lens.lens (\CreateTrialComponent' {startTime} -> startTime) (\s@CreateTrialComponent' {} a -> s {startTime = a} :: CreateTrialComponent) Prelude.. Lens.mapping Data._Time

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
createTrialComponent_status :: Lens.Lens' CreateTrialComponent (Prelude.Maybe TrialComponentStatus)
createTrialComponent_status = Lens.lens (\CreateTrialComponent' {status} -> status) (\s@CreateTrialComponent' {} a -> s {status = a} :: CreateTrialComponent)

-- | A list of tags to associate with the component. You can use
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_Search.html Search>
-- API to search on the tags.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTrialComponentResponse'
            Prelude.<$> (x Data..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrialComponent where
  hashWithSalt _salt CreateTrialComponent' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trialComponentName

instance Prelude.NFData CreateTrialComponent where
  rnf CreateTrialComponent' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trialComponentName

instance Data.ToHeaders CreateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTrialComponent where
  toJSON CreateTrialComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("EndTime" Data..=) Prelude.<$> endTime,
            ("InputArtifacts" Data..=)
              Prelude.<$> inputArtifacts,
            ("MetadataProperties" Data..=)
              Prelude.<$> metadataProperties,
            ("OutputArtifacts" Data..=)
              Prelude.<$> outputArtifacts,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("StartTime" Data..=) Prelude.<$> startTime,
            ("Status" Data..=) Prelude.<$> status,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("TrialComponentName" Data..= trialComponentName)
          ]
      )

instance Data.ToPath CreateTrialComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTrialComponent where
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

instance Prelude.NFData CreateTrialComponentResponse where
  rnf CreateTrialComponentResponse' {..} =
    Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf httpStatus
