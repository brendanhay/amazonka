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
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_status,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_startTime,
    updateTrialComponent_endTime,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_displayName,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_parameters,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { -- | The hyperparameters to remove from the component.
    parametersToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The output artifacts to remove from the component.
    outputArtifactsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The new status of the component.
    status :: Prelude.Maybe TrialComponentStatus,
    -- | The input artifacts to remove from the component.
    inputArtifactsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Replaces all of the component\'s input artifacts with the specified
    -- artifacts.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Replaces all of the component\'s output artifacts with the specified
    -- artifacts.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | Replaces all of the component\'s hyperparameters with the specified
    -- hyperparameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | The name of the component to update.
    trialComponentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTrialComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parametersToRemove', 'updateTrialComponent_parametersToRemove' - The hyperparameters to remove from the component.
--
-- 'outputArtifactsToRemove', 'updateTrialComponent_outputArtifactsToRemove' - The output artifacts to remove from the component.
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
-- 'outputArtifacts', 'updateTrialComponent_outputArtifacts' - Replaces all of the component\'s output artifacts with the specified
-- artifacts.
--
-- 'parameters', 'updateTrialComponent_parameters' - Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters.
--
-- 'trialComponentName', 'updateTrialComponent_trialComponentName' - The name of the component to update.
newUpdateTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  UpdateTrialComponent
newUpdateTrialComponent pTrialComponentName_ =
  UpdateTrialComponent'
    { parametersToRemove =
        Prelude.Nothing,
      outputArtifactsToRemove = Prelude.Nothing,
      status = Prelude.Nothing,
      inputArtifactsToRemove = Prelude.Nothing,
      startTime = Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      displayName = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      parameters = Prelude.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The hyperparameters to remove from the component.
updateTrialComponent_parametersToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_parametersToRemove = Lens.lens (\UpdateTrialComponent' {parametersToRemove} -> parametersToRemove) (\s@UpdateTrialComponent' {} a -> s {parametersToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens._Coerce

-- | The output artifacts to remove from the component.
updateTrialComponent_outputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_outputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {outputArtifactsToRemove} -> outputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {outputArtifactsToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens._Coerce

-- | The new status of the component.
updateTrialComponent_status :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe TrialComponentStatus)
updateTrialComponent_status = Lens.lens (\UpdateTrialComponent' {status} -> status) (\s@UpdateTrialComponent' {} a -> s {status = a} :: UpdateTrialComponent)

-- | The input artifacts to remove from the component.
updateTrialComponent_inputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_inputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {inputArtifactsToRemove} -> inputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {inputArtifactsToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens._Coerce

-- | When the component started.
updateTrialComponent_startTime :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.UTCTime)
updateTrialComponent_startTime = Lens.lens (\UpdateTrialComponent' {startTime} -> startTime) (\s@UpdateTrialComponent' {} a -> s {startTime = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Core._Time

-- | When the component ended.
updateTrialComponent_endTime :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.UTCTime)
updateTrialComponent_endTime = Lens.lens (\UpdateTrialComponent' {endTime} -> endTime) (\s@UpdateTrialComponent' {} a -> s {endTime = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Core._Time

-- | Replaces all of the component\'s input artifacts with the specified
-- artifacts.
updateTrialComponent_inputArtifacts :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
updateTrialComponent_inputArtifacts = Lens.lens (\UpdateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@UpdateTrialComponent' {} a -> s {inputArtifacts = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
updateTrialComponent_displayName :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.Text)
updateTrialComponent_displayName = Lens.lens (\UpdateTrialComponent' {displayName} -> displayName) (\s@UpdateTrialComponent' {} a -> s {displayName = a} :: UpdateTrialComponent)

-- | Replaces all of the component\'s output artifacts with the specified
-- artifacts.
updateTrialComponent_outputArtifacts :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
updateTrialComponent_outputArtifacts = Lens.lens (\UpdateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@UpdateTrialComponent' {} a -> s {outputArtifacts = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens._Coerce

-- | Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters.
updateTrialComponent_parameters :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
updateTrialComponent_parameters = Lens.lens (\UpdateTrialComponent' {parameters} -> parameters) (\s@UpdateTrialComponent' {} a -> s {parameters = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the component to update.
updateTrialComponent_trialComponentName :: Lens.Lens' UpdateTrialComponent Prelude.Text
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
            Prelude.<$> (x Core..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrialComponent

instance Prelude.NFData UpdateTrialComponent

instance Core.ToHeaders UpdateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTrialComponent where
  toJSON UpdateTrialComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ParametersToRemove" Core..=)
              Prelude.<$> parametersToRemove,
            ("OutputArtifactsToRemove" Core..=)
              Prelude.<$> outputArtifactsToRemove,
            ("Status" Core..=) Prelude.<$> status,
            ("InputArtifactsToRemove" Core..=)
              Prelude.<$> inputArtifactsToRemove,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("InputArtifacts" Core..=)
              Prelude.<$> inputArtifacts,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("OutputArtifacts" Core..=)
              Prelude.<$> outputArtifacts,
            ("Parameters" Core..=) Prelude.<$> parameters,
            Prelude.Just
              ("TrialComponentName" Core..= trialComponentName)
          ]
      )

instance Core.ToPath UpdateTrialComponent where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateTrialComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTrialComponentResponse' smart constructor.
data UpdateTrialComponentResponse = UpdateTrialComponentResponse'
  { -- | The Amazon Resource Name (ARN) of the trial component.
    trialComponentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateTrialComponentResponse
newUpdateTrialComponentResponse pHttpStatus_ =
  UpdateTrialComponentResponse'
    { trialComponentArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the trial component.
updateTrialComponentResponse_trialComponentArn :: Lens.Lens' UpdateTrialComponentResponse (Prelude.Maybe Prelude.Text)
updateTrialComponentResponse_trialComponentArn = Lens.lens (\UpdateTrialComponentResponse' {trialComponentArn} -> trialComponentArn) (\s@UpdateTrialComponentResponse' {} a -> s {trialComponentArn = a} :: UpdateTrialComponentResponse)

-- | The response's http status code.
updateTrialComponentResponse_httpStatus :: Lens.Lens' UpdateTrialComponentResponse Prelude.Int
updateTrialComponentResponse_httpStatus = Lens.lens (\UpdateTrialComponentResponse' {httpStatus} -> httpStatus) (\s@UpdateTrialComponentResponse' {} a -> s {httpStatus = a} :: UpdateTrialComponentResponse)

instance Prelude.NFData UpdateTrialComponentResponse
