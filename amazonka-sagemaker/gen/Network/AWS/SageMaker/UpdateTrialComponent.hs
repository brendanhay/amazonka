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
-- Module      : Network.AWS.SageMaker.UpdateTrialComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more properties of a trial component.
module Network.AWS.SageMaker.UpdateTrialComponent
  ( -- * Creating a Request
    UpdateTrialComponent (..),
    newUpdateTrialComponent,

    -- * Request Lenses
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_status,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_startTime,
    updateTrialComponent_endTime,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_displayName,
    updateTrialComponent_parameters,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_trialComponentName,

    -- * Destructuring the Response
    UpdateTrialComponentResponse (..),
    newUpdateTrialComponentResponse,

    -- * Response Lenses
    updateTrialComponentResponse_trialComponentArn,
    updateTrialComponentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { -- | The output artifacts to remove from the component.
    outputArtifactsToRemove :: Core.Maybe [Core.Text],
    -- | The hyperparameters to remove from the component.
    parametersToRemove :: Core.Maybe [Core.Text],
    -- | The new status of the component.
    status :: Core.Maybe TrialComponentStatus,
    -- | The input artifacts to remove from the component.
    inputArtifactsToRemove :: Core.Maybe [Core.Text],
    -- | When the component started.
    startTime :: Core.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Core.Maybe Core.POSIX,
    -- | Replaces all of the component\'s input artifacts with the specified
    -- artifacts.
    inputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Core.Maybe Core.Text,
    -- | Replaces all of the component\'s hyperparameters with the specified
    -- hyperparameters.
    parameters :: Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue),
    -- | Replaces all of the component\'s output artifacts with the specified
    -- artifacts.
    outputArtifacts :: Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact),
    -- | The name of the component to update.
    trialComponentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputArtifactsToRemove', 'updateTrialComponent_outputArtifactsToRemove' - The output artifacts to remove from the component.
--
-- 'parametersToRemove', 'updateTrialComponent_parametersToRemove' - The hyperparameters to remove from the component.
--
-- 'status', 'updateTrialComponent_status' - The new status of the component.
--
-- 'inputArtifactsToRemove', 'updateTrialComponent_inputArtifactsToRemove' - The input artifacts to remove from the component.
--
-- 'startTime', 'updateTrialComponent_startTime' - When the component started.
--
-- 'endTime', 'updateTrialComponent_endTime' - When the component ended.
--
-- 'inputArtifacts', 'updateTrialComponent_inputArtifacts' - Replaces all of the component\'s input artifacts with the specified
-- artifacts.
--
-- 'displayName', 'updateTrialComponent_displayName' - The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
--
-- 'parameters', 'updateTrialComponent_parameters' - Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters.
--
-- 'outputArtifacts', 'updateTrialComponent_outputArtifacts' - Replaces all of the component\'s output artifacts with the specified
-- artifacts.
--
-- 'trialComponentName', 'updateTrialComponent_trialComponentName' - The name of the component to update.
newUpdateTrialComponent ::
  -- | 'trialComponentName'
  Core.Text ->
  UpdateTrialComponent
newUpdateTrialComponent pTrialComponentName_ =
  UpdateTrialComponent'
    { outputArtifactsToRemove =
        Core.Nothing,
      parametersToRemove = Core.Nothing,
      status = Core.Nothing,
      inputArtifactsToRemove = Core.Nothing,
      startTime = Core.Nothing,
      endTime = Core.Nothing,
      inputArtifacts = Core.Nothing,
      displayName = Core.Nothing,
      parameters = Core.Nothing,
      outputArtifacts = Core.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The output artifacts to remove from the component.
updateTrialComponent_outputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Core.Maybe [Core.Text])
updateTrialComponent_outputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {outputArtifactsToRemove} -> outputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {outputArtifactsToRemove = a} :: UpdateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The hyperparameters to remove from the component.
updateTrialComponent_parametersToRemove :: Lens.Lens' UpdateTrialComponent (Core.Maybe [Core.Text])
updateTrialComponent_parametersToRemove = Lens.lens (\UpdateTrialComponent' {parametersToRemove} -> parametersToRemove) (\s@UpdateTrialComponent' {} a -> s {parametersToRemove = a} :: UpdateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The new status of the component.
updateTrialComponent_status :: Lens.Lens' UpdateTrialComponent (Core.Maybe TrialComponentStatus)
updateTrialComponent_status = Lens.lens (\UpdateTrialComponent' {status} -> status) (\s@UpdateTrialComponent' {} a -> s {status = a} :: UpdateTrialComponent)

-- | The input artifacts to remove from the component.
updateTrialComponent_inputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Core.Maybe [Core.Text])
updateTrialComponent_inputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {inputArtifactsToRemove} -> inputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {inputArtifactsToRemove = a} :: UpdateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | When the component started.
updateTrialComponent_startTime :: Lens.Lens' UpdateTrialComponent (Core.Maybe Core.UTCTime)
updateTrialComponent_startTime = Lens.lens (\UpdateTrialComponent' {startTime} -> startTime) (\s@UpdateTrialComponent' {} a -> s {startTime = a} :: UpdateTrialComponent) Core.. Lens.mapping Core._Time

-- | When the component ended.
updateTrialComponent_endTime :: Lens.Lens' UpdateTrialComponent (Core.Maybe Core.UTCTime)
updateTrialComponent_endTime = Lens.lens (\UpdateTrialComponent' {endTime} -> endTime) (\s@UpdateTrialComponent' {} a -> s {endTime = a} :: UpdateTrialComponent) Core.. Lens.mapping Core._Time

-- | Replaces all of the component\'s input artifacts with the specified
-- artifacts.
updateTrialComponent_inputArtifacts :: Lens.Lens' UpdateTrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
updateTrialComponent_inputArtifacts = Lens.lens (\UpdateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@UpdateTrialComponent' {} a -> s {inputArtifacts = a} :: UpdateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
updateTrialComponent_displayName :: Lens.Lens' UpdateTrialComponent (Core.Maybe Core.Text)
updateTrialComponent_displayName = Lens.lens (\UpdateTrialComponent' {displayName} -> displayName) (\s@UpdateTrialComponent' {} a -> s {displayName = a} :: UpdateTrialComponent)

-- | Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters.
updateTrialComponent_parameters :: Lens.Lens' UpdateTrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentParameterValue))
updateTrialComponent_parameters = Lens.lens (\UpdateTrialComponent' {parameters} -> parameters) (\s@UpdateTrialComponent' {} a -> s {parameters = a} :: UpdateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | Replaces all of the component\'s output artifacts with the specified
-- artifacts.
updateTrialComponent_outputArtifacts :: Lens.Lens' UpdateTrialComponent (Core.Maybe (Core.HashMap Core.Text TrialComponentArtifact))
updateTrialComponent_outputArtifacts = Lens.lens (\UpdateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@UpdateTrialComponent' {} a -> s {outputArtifacts = a} :: UpdateTrialComponent) Core.. Lens.mapping Lens._Coerce

-- | The name of the component to update.
updateTrialComponent_trialComponentName :: Lens.Lens' UpdateTrialComponent Core.Text
updateTrialComponent_trialComponentName = Lens.lens (\UpdateTrialComponent' {trialComponentName} -> trialComponentName) (\s@UpdateTrialComponent' {} a -> s {trialComponentName = a} :: UpdateTrialComponent)

instance Core.AWSRequest UpdateTrialComponent where
  type
    AWSResponse UpdateTrialComponent =
      UpdateTrialComponentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrialComponentResponse'
            Core.<$> (x Core..?> "TrialComponentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateTrialComponent

instance Core.NFData UpdateTrialComponent

instance Core.ToHeaders UpdateTrialComponent where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateTrialComponent" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateTrialComponent where
  toJSON UpdateTrialComponent' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OutputArtifactsToRemove" Core..=)
              Core.<$> outputArtifactsToRemove,
            ("ParametersToRemove" Core..=)
              Core.<$> parametersToRemove,
            ("Status" Core..=) Core.<$> status,
            ("InputArtifactsToRemove" Core..=)
              Core.<$> inputArtifactsToRemove,
            ("StartTime" Core..=) Core.<$> startTime,
            ("EndTime" Core..=) Core.<$> endTime,
            ("InputArtifacts" Core..=) Core.<$> inputArtifacts,
            ("DisplayName" Core..=) Core.<$> displayName,
            ("Parameters" Core..=) Core.<$> parameters,
            ("OutputArtifacts" Core..=) Core.<$> outputArtifacts,
            Core.Just
              ("TrialComponentName" Core..= trialComponentName)
          ]
      )

instance Core.ToPath UpdateTrialComponent where
  toPath = Core.const "/"

instance Core.ToQuery UpdateTrialComponent where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateTrialComponentResponse' smart constructor.
data UpdateTrialComponentResponse = UpdateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateTrialComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialComponentArn', 'updateTrialComponentResponse_trialComponentArn' - The Amazon Resource Name (ARN) of the trial component.
--
-- 'httpStatus', 'updateTrialComponentResponse_httpStatus' - The response's http status code.
newUpdateTrialComponentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateTrialComponentResponse
newUpdateTrialComponentResponse pHttpStatus_ =
  UpdateTrialComponentResponse'
    { trialComponentArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
updateTrialComponentResponse_trialComponentArn :: Lens.Lens' UpdateTrialComponentResponse (Core.Maybe Core.Text)
updateTrialComponentResponse_trialComponentArn = Lens.lens (\UpdateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@UpdateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: UpdateTrialComponentResponse)

-- | The response's http status code.
updateTrialComponentResponse_httpStatus :: Lens.Lens' UpdateTrialComponentResponse Core.Int
updateTrialComponentResponse_httpStatus = Lens.lens (\UpdateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@UpdateTrialComponentResponse' {} a -> s {httpStatus = a} :: UpdateTrialComponentResponse)

instance Core.NFData UpdateTrialComponentResponse
