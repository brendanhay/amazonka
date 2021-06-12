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
-- Module      : Network.AWS.SageMaker.DescribeTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of a trials component\'s properties.
module Network.AWS.SageMaker.DescribeTrialComponent
  ( -- * Creating a Request
    DescribeTrialComponent (..),
    newDescribeTrialComponent,

    -- * Request Lenses
    describeTrialComponent_trialComponentName,

    -- * Destructuring the Response
    DescribeTrialComponentResponse (..),
    newDescribeTrialComponentResponse,

    -- * Response Lenses
    describeTrialComponentResponse_status,
    describeTrialComponentResponse_metadataProperties,
    describeTrialComponentResponse_creationTime,
    describeTrialComponentResponse_trialComponentArn,
    describeTrialComponentResponse_startTime,
    describeTrialComponentResponse_source,
    describeTrialComponentResponse_endTime,
    describeTrialComponentResponse_metrics,
    describeTrialComponentResponse_lastModifiedTime,
    describeTrialComponentResponse_inputArtifacts,
    describeTrialComponentResponse_createdBy,
    describeTrialComponentResponse_lastModifiedBy,
    describeTrialComponentResponse_displayName,
    describeTrialComponentResponse_parameters,
    describeTrialComponentResponse_outputArtifacts,
    describeTrialComponentResponse_trialComponentName,
    describeTrialComponentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeTrialComponent' smart constructor.
data DescribeTrialComponent = DescribeTrialComponent'
  { -- | The name of the trial component to describe.
    trialComponentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeTrialComponent
newDescribeTrialComponent pTrialComponentName_ =
  DescribeTrialComponent'
    { trialComponentName =
        pTrialComponentName_
    }

-- | The name of the trial component to describe.
describeTrialComponent_trialComponentName :: Lens.Lens' DescribeTrialComponent Core.Text
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
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "MetadataProperties")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "TrialComponentArn")
            Core.<*> (x Core..?> "StartTime")
            Core.<*> (x Core..?> "Source")
            Core.<*> (x Core..?> "EndTime")
            Core.<*> (x Core..?> "Metrics" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "InputArtifacts" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "CreatedBy")
            Core.<*> (x Core..?> "LastModifiedBy")
            Core.<*> (x Core..?> "DisplayName")
            Core.<*> (x Core..?> "Parameters" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "OutputArtifacts" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TrialComponentName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTrialComponent

instance Core.NFData DescribeTrialComponent

instance Core.ToHeaders DescribeTrialComponent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeTrialComponent" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTrialComponent where
  toJSON DescribeTrialComponent' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("TrialComponentName" Core..= trialComponentName)
          ]
      )

instance Core.ToPath DescribeTrialComponent where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTrialComponent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTrialComponentResponse' smart constructor.
data DescribeTrialComponentResponse = DescribeTrialComponentResponse'
  { -- | The status of the component. States include:
    --
    -- -   InProgress
    --
    -- -   Completed
    --
    -- -   Failed
    status :: Core.Maybe TrialComponentStatus,
    metadataProperties :: Core.Maybe MetadataProperties,
    -- | When the component was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Core.Text,
    -- | When the component started.
    startTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the source and, optionally, the job
    -- type.
    source :: Core.Maybe TrialComponentSource,
    -- | When the component ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | The metrics for the component.
    metrics :: Core.Maybe [TrialComponentMetricSummary],
    -- | When the component was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The input artifacts of the component.
    inputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | Who created the component.
    createdBy :: Core.Maybe UserContext,
    -- | Who last modified the component.
    lastModifiedBy :: Core.Maybe UserContext,
    -- | The name of the component as displayed. If @DisplayName@ isn\'t
    -- specified, @TrialComponentName@ is displayed.
    displayName :: Core.Maybe Core.Text,
    -- | The hyperparameters of the component.
    parameters :: Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue),
    -- | The output artifacts of the component.
    outputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | The name of the trial component.
    trialComponentName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeTrialComponentResponse_status' - The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
--
-- 'metadataProperties', 'describeTrialComponentResponse_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'describeTrialComponentResponse_creationTime' - When the component was created.
--
-- 'trialComponentArn', 'describeTrialComponentResponse_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'startTime', 'describeTrialComponentResponse_startTime' - When the component started.
--
-- 'source', 'describeTrialComponentResponse_source' - The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
--
-- 'endTime', 'describeTrialComponentResponse_endTime' - When the component ended.
--
-- 'metrics', 'describeTrialComponentResponse_metrics' - The metrics for the component.
--
-- 'lastModifiedTime', 'describeTrialComponentResponse_lastModifiedTime' - When the component was last modified.
--
-- 'inputArtifacts', 'describeTrialComponentResponse_inputArtifacts' - The input artifacts of the component.
--
-- 'createdBy', 'describeTrialComponentResponse_createdBy' - Who created the component.
--
-- 'lastModifiedBy', 'describeTrialComponentResponse_lastModifiedBy' - Who last modified the component.
--
-- 'displayName', 'describeTrialComponentResponse_displayName' - The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
--
-- 'parameters', 'describeTrialComponentResponse_parameters' - The hyperparameters of the component.
--
-- 'outputArtifacts', 'describeTrialComponentResponse_outputArtifacts' - The output artifacts of the component.
--
-- 'trialComponentName', 'describeTrialComponentResponse_trialComponentName' - The name of the trial component.
--
-- 'httpStatus', 'describeTrialComponentResponse_httpStatus' - The response's http status code.
newDescribeTrialComponentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTrialComponentResponse
newDescribeTrialComponentResponse pHttpStatus_ =
  DescribeTrialComponentResponse'
    { status =
        Core.Nothing,
      metadataProperties = Core.Nothing,
      creationTime = Core.Nothing,
      trialComponentArn = Core.Nothing,
      startTime = Core.Nothing,
      source = Core.Nothing,
      endTime = Core.Nothing,
      metrics = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      inputArtifacts = Core.Nothing,
      createdBy = Core.Nothing,
      lastModifiedBy = Core.Nothing,
      displayName = Core.Nothing,
      parameters = Core.Nothing,
      outputArtifacts = Core.Nothing,
      trialComponentName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the component. States include:
--
-- -   InProgress
--
-- -   Completed
--
-- -   Failed
describeTrialComponentResponse_status :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe TrialComponentStatus)
describeTrialComponentResponse_status = Lens.lens (\DescribeTrialComponentResponse' {status} -> status) (\s@DescribeTrialComponentResponse' {} a -> s {status = a} :: DescribeTrialComponentResponse)

-- | Undocumented member.
describeTrialComponentResponse_metadataProperties :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe MetadataProperties)
describeTrialComponentResponse_metadataProperties = Lens.lens (\DescribeTrialComponentResponse' {metadataProperties} -> metadataProperties) (\s@DescribeTrialComponentResponse' {} a -> s {metadataProperties = a} :: DescribeTrialComponentResponse)

-- | When the component was created.
describeTrialComponentResponse_creationTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.UTCTime)
describeTrialComponentResponse_creationTime = Lens.lens (\DescribeTrialComponentResponse' {creationTime} -> creationTime) (\s@DescribeTrialComponentResponse' {} a -> s {creationTime = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the trial component.
describeTrialComponentResponse_trialComponentArn :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.Text)
describeTrialComponentResponse_trialComponentArn = Lens.lens (\DescribeTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@DescribeTrialComponentResponse' {} a -> s {trialComponentArn = a} :: DescribeTrialComponentResponse)

-- | When the component started.
describeTrialComponentResponse_startTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.UTCTime)
describeTrialComponentResponse_startTime = Lens.lens (\DescribeTrialComponentResponse' {startTime} -> startTime) (\s@DescribeTrialComponentResponse' {} a -> s {startTime = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the source and, optionally, the job
-- type.
describeTrialComponentResponse_source :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe TrialComponentSource)
describeTrialComponentResponse_source = Lens.lens (\DescribeTrialComponentResponse' {source} -> source) (\s@DescribeTrialComponentResponse' {} a -> s {source = a} :: DescribeTrialComponentResponse)

-- | When the component ended.
describeTrialComponentResponse_endTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.UTCTime)
describeTrialComponentResponse_endTime = Lens.lens (\DescribeTrialComponentResponse' {endTime} -> endTime) (\s@DescribeTrialComponentResponse' {} a -> s {endTime = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Core._Time

-- | The metrics for the component.
describeTrialComponentResponse_metrics :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe [TrialComponentMetricSummary])
describeTrialComponentResponse_metrics = Lens.lens (\DescribeTrialComponentResponse' {metrics} -> metrics) (\s@DescribeTrialComponentResponse' {} a -> s {metrics = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Lens._Coerce

-- | When the component was last modified.
describeTrialComponentResponse_lastModifiedTime :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.UTCTime)
describeTrialComponentResponse_lastModifiedTime = Lens.lens (\DescribeTrialComponentResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeTrialComponentResponse' {} a -> s {lastModifiedTime = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Core._Time

-- | The input artifacts of the component.
describeTrialComponentResponse_inputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
describeTrialComponentResponse_inputArtifacts = Lens.lens (\DescribeTrialComponentResponse' {inputArtifacts} -> inputArtifacts) (\s@DescribeTrialComponentResponse' {} a -> s {inputArtifacts = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Lens._Coerce

-- | Who created the component.
describeTrialComponentResponse_createdBy :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe UserContext)
describeTrialComponentResponse_createdBy = Lens.lens (\DescribeTrialComponentResponse' {createdBy} -> createdBy) (\s@DescribeTrialComponentResponse' {} a -> s {createdBy = a} :: DescribeTrialComponentResponse)

-- | Who last modified the component.
describeTrialComponentResponse_lastModifiedBy :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe UserContext)
describeTrialComponentResponse_lastModifiedBy = Lens.lens (\DescribeTrialComponentResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeTrialComponentResponse' {} a -> s {lastModifiedBy = a} :: DescribeTrialComponentResponse)

-- | The name of the component as displayed. If @DisplayName@ isn\'t
-- specified, @TrialComponentName@ is displayed.
describeTrialComponentResponse_displayName :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.Text)
describeTrialComponentResponse_displayName = Lens.lens (\DescribeTrialComponentResponse' {displayName} -> displayName) (\s@DescribeTrialComponentResponse' {} a -> s {displayName = a} :: DescribeTrialComponentResponse)

-- | The hyperparameters of the component.
describeTrialComponentResponse_parameters :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue))
describeTrialComponentResponse_parameters = Lens.lens (\DescribeTrialComponentResponse' {parameters} -> parameters) (\s@DescribeTrialComponentResponse' {} a -> s {parameters = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Lens._Coerce

-- | The output artifacts of the component.
describeTrialComponentResponse_outputArtifacts :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
describeTrialComponentResponse_outputArtifacts = Lens.lens (\DescribeTrialComponentResponse' {outputArtifacts} -> outputArtifacts) (\s@DescribeTrialComponentResponse' {} a -> s {outputArtifacts = a} :: DescribeTrialComponentResponse) Core.. Lens.mapping Lens._Coerce

-- | The name of the trial component.
describeTrialComponentResponse_trialComponentName :: Lens.Lens' DescribeTrialComponentResponse (Core.Maybe Core.Text)
describeTrialComponentResponse_trialComponentName = Lens.lens (\DescribeTrialComponentResponse' {trialComponentName} -> trialComponentName) (\s@DescribeTrialComponentResponse' {} a -> s {trialComponentName = a} :: DescribeTrialComponentResponse)

-- | The response's http status code.
describeTrialComponentResponse_httpStatus :: Lens.Lens' DescribeTrialComponentResponse Core.Int
describeTrialComponentResponse_httpStatus = Lens.lens (\DescribeTrialComponentResponse' {httpStatus} -> httpStatus) (\s@DescribeTrialComponentResponse' {} a -> s {httpStatus = a} :: DescribeTrialComponentResponse)

instance Core.NFData DescribeTrialComponentResponse
