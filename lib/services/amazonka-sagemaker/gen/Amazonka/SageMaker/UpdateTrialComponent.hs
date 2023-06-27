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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    updateTrialComponent_displayName,
    updateTrialComponent_endTime,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_parameters,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_startTime,
    updateTrialComponent_status,
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateTrialComponent' smart constructor.
data UpdateTrialComponent = UpdateTrialComponent'
  { -- | The name of the component as displayed. The name doesn\'t need to be
    -- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
    -- displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | When the component ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Replaces all of the component\'s input artifacts with the specified
    -- artifacts or adds new input artifacts. Existing input artifacts are
    -- replaced if the trial component is updated with an identical input
    -- artifact key.
    inputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The input artifacts to remove from the component.
    inputArtifactsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | Replaces all of the component\'s output artifacts with the specified
    -- artifacts or adds new output artifacts. Existing output artifacts are
    -- replaced if the trial component is updated with an identical output
    -- artifact key.
    outputArtifacts :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact),
    -- | The output artifacts to remove from the component.
    outputArtifactsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | Replaces all of the component\'s hyperparameters with the specified
    -- hyperparameters or add new hyperparameters. Existing hyperparameters are
    -- replaced if the trial component is updated with an identical
    -- hyperparameter key.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue),
    -- | The hyperparameters to remove from the component.
    parametersToRemove :: Prelude.Maybe [Prelude.Text],
    -- | When the component started.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The new status of the component.
    status :: Prelude.Maybe TrialComponentStatus,
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
-- 'displayName', 'updateTrialComponent_displayName' - The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
--
-- 'endTime', 'updateTrialComponent_endTime' - When the component ended.
--
-- 'inputArtifacts', 'updateTrialComponent_inputArtifacts' - Replaces all of the component\'s input artifacts with the specified
-- artifacts or adds new input artifacts. Existing input artifacts are
-- replaced if the trial component is updated with an identical input
-- artifact key.
--
-- 'inputArtifactsToRemove', 'updateTrialComponent_inputArtifactsToRemove' - The input artifacts to remove from the component.
--
-- 'outputArtifacts', 'updateTrialComponent_outputArtifacts' - Replaces all of the component\'s output artifacts with the specified
-- artifacts or adds new output artifacts. Existing output artifacts are
-- replaced if the trial component is updated with an identical output
-- artifact key.
--
-- 'outputArtifactsToRemove', 'updateTrialComponent_outputArtifactsToRemove' - The output artifacts to remove from the component.
--
-- 'parameters', 'updateTrialComponent_parameters' - Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters or add new hyperparameters. Existing hyperparameters are
-- replaced if the trial component is updated with an identical
-- hyperparameter key.
--
-- 'parametersToRemove', 'updateTrialComponent_parametersToRemove' - The hyperparameters to remove from the component.
--
-- 'startTime', 'updateTrialComponent_startTime' - When the component started.
--
-- 'status', 'updateTrialComponent_status' - The new status of the component.
--
-- 'trialComponentName', 'updateTrialComponent_trialComponentName' - The name of the component to update.
newUpdateTrialComponent ::
  -- | 'trialComponentName'
  Prelude.Text ->
  UpdateTrialComponent
newUpdateTrialComponent pTrialComponentName_ =
  UpdateTrialComponent'
    { displayName =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputArtifacts = Prelude.Nothing,
      inputArtifactsToRemove = Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      outputArtifactsToRemove = Prelude.Nothing,
      parameters = Prelude.Nothing,
      parametersToRemove = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      trialComponentName = pTrialComponentName_
    }

-- | The name of the component as displayed. The name doesn\'t need to be
-- unique. If @DisplayName@ isn\'t specified, @TrialComponentName@ is
-- displayed.
updateTrialComponent_displayName :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.Text)
updateTrialComponent_displayName = Lens.lens (\UpdateTrialComponent' {displayName} -> displayName) (\s@UpdateTrialComponent' {} a -> s {displayName = a} :: UpdateTrialComponent)

-- | When the component ended.
updateTrialComponent_endTime :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.UTCTime)
updateTrialComponent_endTime = Lens.lens (\UpdateTrialComponent' {endTime} -> endTime) (\s@UpdateTrialComponent' {} a -> s {endTime = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Data._Time

-- | Replaces all of the component\'s input artifacts with the specified
-- artifacts or adds new input artifacts. Existing input artifacts are
-- replaced if the trial component is updated with an identical input
-- artifact key.
updateTrialComponent_inputArtifacts :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
updateTrialComponent_inputArtifacts = Lens.lens (\UpdateTrialComponent' {inputArtifacts} -> inputArtifacts) (\s@UpdateTrialComponent' {} a -> s {inputArtifacts = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The input artifacts to remove from the component.
updateTrialComponent_inputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_inputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {inputArtifactsToRemove} -> inputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {inputArtifactsToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | Replaces all of the component\'s output artifacts with the specified
-- artifacts or adds new output artifacts. Existing output artifacts are
-- replaced if the trial component is updated with an identical output
-- artifact key.
updateTrialComponent_outputArtifacts :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentArtifact))
updateTrialComponent_outputArtifacts = Lens.lens (\UpdateTrialComponent' {outputArtifacts} -> outputArtifacts) (\s@UpdateTrialComponent' {} a -> s {outputArtifacts = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The output artifacts to remove from the component.
updateTrialComponent_outputArtifactsToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_outputArtifactsToRemove = Lens.lens (\UpdateTrialComponent' {outputArtifactsToRemove} -> outputArtifactsToRemove) (\s@UpdateTrialComponent' {} a -> s {outputArtifactsToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | Replaces all of the component\'s hyperparameters with the specified
-- hyperparameters or add new hyperparameters. Existing hyperparameters are
-- replaced if the trial component is updated with an identical
-- hyperparameter key.
updateTrialComponent_parameters :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text TrialComponentParameterValue))
updateTrialComponent_parameters = Lens.lens (\UpdateTrialComponent' {parameters} -> parameters) (\s@UpdateTrialComponent' {} a -> s {parameters = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | The hyperparameters to remove from the component.
updateTrialComponent_parametersToRemove :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe [Prelude.Text])
updateTrialComponent_parametersToRemove = Lens.lens (\UpdateTrialComponent' {parametersToRemove} -> parametersToRemove) (\s@UpdateTrialComponent' {} a -> s {parametersToRemove = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Lens.coerced

-- | When the component started.
updateTrialComponent_startTime :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe Prelude.UTCTime)
updateTrialComponent_startTime = Lens.lens (\UpdateTrialComponent' {startTime} -> startTime) (\s@UpdateTrialComponent' {} a -> s {startTime = a} :: UpdateTrialComponent) Prelude.. Lens.mapping Data._Time

-- | The new status of the component.
updateTrialComponent_status :: Lens.Lens' UpdateTrialComponent (Prelude.Maybe TrialComponentStatus)
updateTrialComponent_status = Lens.lens (\UpdateTrialComponent' {status} -> status) (\s@UpdateTrialComponent' {} a -> s {status = a} :: UpdateTrialComponent)

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
            Prelude.<$> (x Data..?> "TrialComponentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTrialComponent where
  hashWithSalt _salt UpdateTrialComponent' {..} =
    _salt
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` inputArtifacts
      `Prelude.hashWithSalt` inputArtifactsToRemove
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` outputArtifactsToRemove
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` parametersToRemove
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` trialComponentName

instance Prelude.NFData UpdateTrialComponent where
  rnf UpdateTrialComponent' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputArtifacts
      `Prelude.seq` Prelude.rnf inputArtifactsToRemove
      `Prelude.seq` Prelude.rnf outputArtifacts
      `Prelude.seq` Prelude.rnf outputArtifactsToRemove
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf parametersToRemove
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf trialComponentName

instance Data.ToHeaders UpdateTrialComponent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateTrialComponent" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTrialComponent where
  toJSON UpdateTrialComponent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayName" Data..=) Prelude.<$> displayName,
            ("EndTime" Data..=) Prelude.<$> endTime,
            ("InputArtifacts" Data..=)
              Prelude.<$> inputArtifacts,
            ("InputArtifactsToRemove" Data..=)
              Prelude.<$> inputArtifactsToRemove,
            ("OutputArtifacts" Data..=)
              Prelude.<$> outputArtifacts,
            ("OutputArtifactsToRemove" Data..=)
              Prelude.<$> outputArtifactsToRemove,
            ("Parameters" Data..=) Prelude.<$> parameters,
            ("ParametersToRemove" Data..=)
              Prelude.<$> parametersToRemove,
            ("StartTime" Data..=) Prelude.<$> startTime,
            ("Status" Data..=) Prelude.<$> status,
            Prelude.Just
              ("TrialComponentName" Data..= trialComponentName)
          ]
      )

instance Data.ToPath UpdateTrialComponent where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTrialComponent where
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
