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
-- Module      : Amazonka.SageMaker.DescribeTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trials component\'s properties.
module Amazonka.SageMaker.DescribeTrialComponent
  ( -- * Creating a Request
    DescribeTrialComponent (..),
    newDescribeTrialComponent,

    -- * Request Lenses
    describeTrialComponent_trialComponentName,

    -- * Destructuring the Response
    DescribeTrialComponentResponse (..),
    newDescribeTrialComponentResponse,

    -- * Response Lenses
    describeTrialComponentResponse_creationTime,
    describeTrialComponentResponse_metadataProperties,
    describeTrialComponentResponse_status,
    describeTrialComponentResponse_metrics,
    describeTrialComponentResponse_outputArtifacts,
    describeTrialComponentResponse_startTime,
    describeTrialComponentResponse_createdBy,
    describeTrialComponentResponse_lastModifiedTime,
    describeTrialComponentResponse_endTime,
    describeTrialComponentResponse_trialComponentName,
    describeTrialComponentResponse_parameters,
    describeTrialComponentResponse_source,
    describeTrialComponentResponse_displayName,
    describeTrialComponentResponse_lastModifiedBy,
    describeTrialComponentResponse_trialComponentArn,
    describeTrialComponentResponse_inputArtifacts,
    describeTrialComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeTrialComponent' smart constructor.
data DescribeTrialComponent = DescribeTrialComponent'
  { -- | The name of the trial component to describe.
    trialComponentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentName', 'describeTrialComponent_trialComponentName' - The name of the trial component to describe.
newDescribeTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  DescribeTrialComponent
newDescribeTrialComponent pTrialComponentName_ =
  DescribeTrialComponent'
    { trialComponentName =
        pTrialComponentName_
    }

-- | The name of the trial component to describe.
describeTrialComponent_trialComponentName :: Lens.Lens' DescribeTrialComponent Prelude.Text
describeTrialComponent_trialComponentName = Lens.lens (\DescribeTrialComponent' {trialComponentName} -> trialComponentName) (\s@DescribeTrialComponent' {} a -> s {trialComponentName = a} :: DescribeTrialComponent)

instance Core.AWSRequest DescribeTrialComponent where
  type
    AWSResponse DescribeTrialComponent =
      DescribeTrialComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTrialComponentResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "MetadataProperties")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Metrics" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "OutputArtifacts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "StartTime")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "EndTime")
            Prelude.<*> (x Core..?> "TrialComponentName")
            Prelude.<*> (x Core..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Source")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (x Core..?> "LastModifiedBy")
            Prelude.<*> (x Core..?> "TrialComponentArn")
            Prelude.<*> (x Core..?> "InputArtifacts" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTrialComponent where
  hashWithSalt _salt DescribeTrialComponent' {..} =
    _salt `Prelude.hashWithSalt` trialComponentName

instance Prelude.NFData DescribeTrialComponent where
  rnf DescribeTrialComponent' {..} =
    Prelude.rnf trialComponentName

instance Core.ToHeaders DescribeTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTrialComponent where
  toJSON DescribeTrialComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrialComponentName" Core..= trialComponentName)
          ]
      )

instance Core.ToPath DescribeTrialComponent where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTrialComponentResponse' smart constructor.
data DescribeTrialComponentResponse = DescribeTrialComponentResponse'
  { -- | When the component was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Prelude.Maybe TrialComponentStatus,
    -- | The metrics for the component.
    metrics :: Prelude.Maybe [TrialComponentMetricSummary],
    -- | The output artifacts of the component.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | Who created the trial component.
    createdBy :: Prelude.Maybe UserContext,
    -- | When the component was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text,
    -- | The hyperparameters of the component.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job
    -- type.
    source :: Prelude.Maybe TrialComponentSource,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Who last modified the component.
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The input artifacts of the component.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeTrialComponentResponse_creationTime' - When the component was created.
--
-- 'metadataProperties', 'describeTrialComponentResponse_metadataProperties' - Undocumented member.
--
-- 'status', 'describeTrialComponentResponse_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'metrics', 'describeTrialComponentResponse_metrics' - The metrics for the component.
--
-- 'outputArtifacts', 'describeTrialComponentResponse_outputArtifacts' - The output artifacts of the component.
--
-- 'startTime', 'describeTrialComponentResponse_startTime' - When the component started.
--
-- 'createdBy', 'describeTrialComponentResponse_createdBy' - Who created the trial component.
--
-- 'lastModifiedTime', 'describeTrialComponentResponse_lastModifiedTime' - When the component was last modified.
--
-- 'endTime', 'describeTrialComponentResponse_endTime' - When the component ended.
--
-- 'trialComponentName', 'describeTrialComponentResponse_trialComponentName' - The name of the trial component.
--
-- 'parameters', 'describeTrialComponentResponse_parameters' - The hyperparameters of the component.
--
-- 'source', 'describeTrialComponentResponse_source' - The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
--
-- 'displayName', 'describeTrialComponentResponse_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'lastModifiedBy', 'describeTrialComponentResponse_lastModifiedBy' - Who last modified the component.
--
-- 'trialComponentArn', 'describeTrialComponentResponse_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'inputArtifacts', 'describeTrialComponentResponse_inputArtifacts' - The input artifacts of the component.
--
-- 'httpStatus', 'describeTrialComponentResponse_httpStatus' - The response's http status code.
newDescribeTrialComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTrialComponentResponse
newDescribeTrialComponentResponse pHttpStatus_ =
  DescribeTrialComponentResponse'
    { creationTime =
        Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      status = Prelude.Nothing,
      metrics = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      startTime = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      trialComponentName = Prelude.Nothing,
      parameters = Prelude.Nothing,
      source = Prelude.Nothing,
      displayName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      trialComponentArn = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When the component was created.
describeTrialComponentResponse_creationTime :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialComponentResponse_creationTime = Lens.lens (\DescribeTrialComponentResponse' {creationTime} -> creationTime) (\s@DescribeTrialComponentResponse' {} a -> s {creationTime = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeTrialComponentResponse_metadataProperties :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe MetadataProperties)
describeTrialComponentResponse_metadataProperties = Lens.lens (\DescribeTrialComponentResponse' {metadataProperties} -> metadataProperties) (\s@DescribeTrialComponentResponse' {} a -> s {metadataProperties = a} :: DescribeTrialComponentResponse)

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
describeTrialComponentResponse_status :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe TrialComponentStatus)
describeTrialComponentResponse_status = Lens.lens (\DescribeTrialComponentResponse' {status} -> status) (\s@DescribeTrialComponentResponse' {} a -> s {status = a} :: DescribeTrialComponentResponse)

-- | The metrics for the component.
describeTrialComponentResponse_metrics :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe [TrialComponentMetricSummary])
describeTrialComponentResponse_metrics = Lens.lens (\DescribeTrialComponentResponse' {metrics} -> metrics) (\s@DescribeTrialComponentResponse' {} a -> s {metrics = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The output artifacts of the component.
describeTrialComponentResponse_outputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
describeTrialComponentResponse_outputArtifacts = Lens.lens (\DescribeTrialComponentResponse' {outputArtifacts} -> outputArtifacts) (\s@DescribeTrialComponentResponse' {} a -> s {outputArtifacts = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | When the component started.
describeTrialComponentResponse_startTime :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialComponentResponse_startTime = Lens.lens (\DescribeTrialComponentResponse' {startTime} -> startTime) (\s@DescribeTrialComponentResponse' {} a -> s {startTime = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Core._Time

-- | Who created the trial component.
describeTrialComponentResponse_createdBy :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe UserContext)
describeTrialComponentResponse_createdBy = Lens.lens (\DescribeTrialComponentResponse' {createdBy} -> createdBy) (\s@DescribeTrialComponentResponse' {} a -> s {createdBy = a} :: DescribeTrialComponentResponse)

-- | When the component was last modified.
describeTrialComponentResponse_lastModifiedTime :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialComponentResponse_lastModifiedTime = Lens.lens (\DescribeTrialComponentResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrialComponentResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Core._Time

-- | When the component ended.
describeTrialComponentResponse_endTime :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.UTCTime)
describeTrialComponentResponse_endTime = Lens.lens (\DescribeTrialComponentResponse' {endTime} -> endTime) (\s@DescribeTrialComponentResponse' {} a -> s {endTime = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the trial component.
describeTrialComponentResponse_trialComponentName :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.Text)
describeTrialComponentResponse_trialComponentName = Lens.lens (\DescribeTrialComponentResponse' {trialComponentName} -> trialComponentName) (\s@DescribeTrialComponentResponse' {} a -> s {trialComponentName = a} :: DescribeTrialComponentResponse)

-- | The hyperparameters of the component.
describeTrialComponentResponse_parameters :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
describeTrialComponentResponse_parameters = Lens.lens (\DescribeTrialComponentResponse' {parameters} -> parameters) (\s@DescribeTrialComponentResponse' {} a -> s {parameters = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
describeTrialComponentResponse_source :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe TrialComponentSource)
describeTrialComponentResponse_source = Lens.lens (\DescribeTrialComponentResponse' {source} -> source) (\s@DescribeTrialComponentResponse' {} a -> s {source = a} :: DescribeTrialComponentResponse)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
describeTrialComponentResponse_displayName :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.Text)
describeTrialComponentResponse_displayName = Lens.lens (\DescribeTrialComponentResponse' {displayName} -> displayName) (\s@DescribeTrialComponentResponse' {} a -> s {displayName = a} :: DescribeTrialComponentResponse)

-- | Who last modified the component.
describeTrialComponentResponse_lastModifiedBy :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe UserContext)
describeTrialComponentResponse_lastModifiedBy = Lens.lens (\DescribeTrialComponentResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeTrialComponentResponse' {} a -> s {lastModifiedBy = a} :: DescribeTrialComponentResponse)

-- | The Amazon Resource Name (ARN) of the trial component.
describeTrialComponentResponse_trialComponentArn :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe Prelude.Text)
describeTrialComponentResponse_trialComponentArn = Lens.lens (\DescribeTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@DescribeTrialComponentResponse' {} a -> s {trialComponentArn = a} :: DescribeTrialComponentResponse)

-- | The input artifacts of the component.
describeTrialComponentResponse_inputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
describeTrialComponentResponse_inputArtifacts = Lens.lens (\DescribeTrialComponentResponse' {inputArtifacts} -> inputArtifacts) (\s@DescribeTrialComponentResponse' {} a -> s {inputArtifacts = a} :: DescribeTrialComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTrialComponentResponse_httpStatus :: Lens.Lens' DescribeTrialComponentResponse Prelude.Int
describeTrialComponentResponse_httpStatus = Lens.lens (\DescribeTrialComponentResponse' {httpStatus} -> httpStatus) (\s@DescribeTrialComponentResponse' {} a -> s {httpStatus = a} :: DescribeTrialComponentResponse)

instance
  Prelude.NFData
    DescribeTrialComponentResponse
  where
  rnf DescribeTrialComponentResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf metrics
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf trialComponentName
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf httpStatus
