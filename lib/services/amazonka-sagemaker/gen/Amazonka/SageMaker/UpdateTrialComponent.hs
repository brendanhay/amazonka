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
-- Module      : Amazonka.SageMaker.UpdateTrialComponent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more properties of a trial component.
module Amazonka.SageMaker.UpdateTrialComponent
  ( -- * Creating a Request
    UpdateTrialComponent (..),
    newUpdateTrialComponent,

    -- * Request Lenses
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_displayName,
    updateTrialComponent_status,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_endTime,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_startTime,
    updateTrialComponent_inputArtifactsToRemove,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { -- | The output artifacts to remove from the component.
    outputArtifactsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The new status of the component.
    status :: Prelude.Maybe TrialComponentStatus,
    -- | Replaces all of the component\'s output artifacts with the specified
    -- artifacts.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | When the component ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The hyperparameters to remove from the component.
    parametersToRemove :: Prelude.Maybe [Prelude.Text],
    -- | Replaces all of the component\'s input artifacts with the specified
    -- artifacts.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | When the component started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The input artifacts to remove from the component.
    inputArtifactsToRemove :: Prelude.Maybe [Prelude.Text],
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
-- 'outputArtifactsToRemove', 'updateTrialComponent_outputArtifactsToRemove' - The output artifacts to remove from the component.
--
-- 'displayName', 'updateTrialComponent_displayName' - The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
--
-- 'status', 'updateTrialComponent_status' - The new status of the component.
--
-- 'outputArtifacts', 'updateTrialComponent_outputArtifacts' - Replaces all of the component\'s output artifacts with the specified
-- artifacts.
--
-- 'endTime', 'updateTrialComponent_endTime' - When the component ended.
--
-- 'parametersToRemove', 'updateTrialComponent_parametersToRemove' - The hyperparameters to remove from the component.
--
-- 'inputArtifacts', 'updateTrialComponent_inputArtifacts' - Replaces all of the component\'s input artifacts with the specified
-- artifacts.
--
-- 'startTime', 'updateTrialComponent_startTime' - When the component started.
--
-- 'inputArtifactsToRemove', 'updateTrialComponent_inputArtifactsToRemove' - The input artifacts to remove from the component.
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
    { outputArtifactsToRemove =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      status = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      endTime = Prelude.Nothing,
      parametersToRemove = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      startTime = Prelude.Nothing,
      inputArtifactsToRemove = Prelude.Nothing,
      parameters = Prelude.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The output artifacts to remove from the component.
updateTrialComponent_outputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_outputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {outputArtifactsToRemove} -> outputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {outputArtifactsToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
updateTrialComponent_displayName :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.Text)
updateTrialComponent_displayName = Lens.lens (\UpdateTrialComponent' {displayName} -> displayName) (\s@UpdateTrialComponent' {} a -> s {displayName = a} :: UpdateTrialComponent)

-- | The new status of the component.
updateTrialComponent_status :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe TrialComponentStatus)
updateTrialComponent_status = Lens.lens (\UpdateTrialComponent' {status} -> status) (\s@UpdateTrialComponent' {} a -> s {status = a} :: UpdateTrialComponent)

-- | Replaces all of the component\'s output artifacts with the specified
-- artifacts.
updateTrialComponent_outputArtifacts :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
updateTrialComponent_outputArtifacts = Lens.lens (\UpdateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@UpdateTrialComponent' {} a -> s {outputArtifacts = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component ended.
updateTrialComponent_endTime :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.UTCTime)
updateTrialComponent_endTime = Lens.lens (\UpdateTrialComponent' {endTime} -> endTime) (\s@UpdateTrialComponent' {} a -> s {endTime = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Core._Time

-- | The hyperparameters to remove from the component.
updateTrialComponent_parametersToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_parametersToRemove = Lens.lens (\UpdateTrialComponent' {parametersToRemove} -> parametersToRemove) (\s@UpdateTrialComponent' {} a -> s {parametersToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | Replaces all of the component\'s input artifacts with the specified
-- artifacts.
updateTrialComponent_inputArtifacts :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
updateTrialComponent_inputArtifacts = Lens.lens (\UpdateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@UpdateTrialComponent' {} a -> s {inputArtifacts = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component started.
updateTrialComponent_startTime :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.UTCTime)
updateTrialComponent_startTime = Lens.lens (\UpdateTrialComponent' {startTime} -> startTime) (\s@UpdateTrialComponent' {} a -> s {startTime = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Core._Time

-- | The input artifacts to remove from the component.
updateTrialComponent_inputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_inputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {inputArtifactsToRemove} -> inputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {inputArtifactsToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters.
updateTrialComponent_parameters :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
updateTrialComponent_parameters = Lens.lens (\UpdateTrialComponent' {parameters} -> parameters) (\s@UpdateTrialComponent' {} a -> s {parameters = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The name of the component to update.
updateTrialComponent_trialComponentName :: Lens.Lens' UpdateTrialComponent Prelude.Text
updateTrialComponent_trialComponentName = Lens.lens (\UpdateTrialComponent' {trialComponentName} -> trialComponentName) (\s@UpdateTrialComponent' {} a -> s {trialComponentName = a} :: UpdateTrialComponent)

instance Core.AWSRequest UpdateTrialComponent where
  type
    AWSResponse UpdateTrialComponent =
      UpdateTrialComponentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTrialComponentResponse'
            Prelude.<$> (x Core..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrialComponent where
  hashWithSalt _salt UpdateTrialComponent' {..} =
    _salt
      `Prelude.hashWithSalt` outputArtifactsToRemove
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` parametersToRemove
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` inputArtifactsToRemove
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` trialComponentName

instance Prelude.NFData UpdateTrialComponent where
  rnf UpdateTrialComponent' {..} =
    Prelude.rnf outputArtifactsToRemove
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf parametersToRemove
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf inputArtifactsToRemove
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf trialComponentName

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
          [ ("OutputArtifactsToRemove" Core..=)
              Prelude.<$> outputArtifactsToRemove,
            ("DisplayName" Core..=) Prelude.<$> displayName,
            ("Status" Core..=) Prelude.<$> status,
            ("OutputArtifacts" Core..=)
              Prelude.<$> outputArtifacts,
            ("EndTime" Core..=) Prelude.<$> endTime,
            ("ParametersToRemove" Core..=)
              Prelude.<$> parametersToRemove,
            ("InputArtifacts" Core..=)
              Prelude.<$> inputArtifacts,
            ("StartTime" Core..=) Prelude.<$> startTime,
            ("InputArtifactsToRemove" Core..=)
              Prelude.<$> inputArtifactsToRemove,
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

instance Prelude.NFData UpdateTrialComponentResponse where
  rnf UpdateTrialComponentResponse' {..} =
    Prelude.rnf trialComponentArn
      `Prelude.seq` Prelude.rnf httpStatus
