{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Amplify.Types.Step
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.Step where

import Amazonka.Amplify.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an execution step, for an execution job, for an Amplify app.
--
-- /See:/ 'newStep' smart constructor.
data Step = Step'
  { -- | The list of screenshot URLs for the execution step, if relevant.
    screenshots :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The reason for the current step status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The URL to the logs for the execution step.
    logUrl :: Prelude.Maybe Prelude.Text,
    -- | The context for the current step. Includes a build image if the step is
    -- build.
    context :: Prelude.Maybe Prelude.Text,
    -- | The URL to the artifact for the execution step.
    artifactsUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to the test configuration for the execution step.
    testConfigUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to the test artifact for the execution step.
    testArtifactsUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the execution step.
    stepName :: Prelude.Text,
    -- | The start date and time of the execution step.
    startTime :: Data.POSIX,
    -- | The status of the execution step.
    status :: JobStatus,
    -- | The end date and time of the execution step.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Step' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'screenshots', 'step_screenshots' - The list of screenshot URLs for the execution step, if relevant.
--
-- 'statusReason', 'step_statusReason' - The reason for the current step status.
--
-- 'logUrl', 'step_logUrl' - The URL to the logs for the execution step.
--
-- 'context', 'step_context' - The context for the current step. Includes a build image if the step is
-- build.
--
-- 'artifactsUrl', 'step_artifactsUrl' - The URL to the artifact for the execution step.
--
-- 'testConfigUrl', 'step_testConfigUrl' - The URL to the test configuration for the execution step.
--
-- 'testArtifactsUrl', 'step_testArtifactsUrl' - The URL to the test artifact for the execution step.
--
-- 'stepName', 'step_stepName' - The name of the execution step.
--
-- 'startTime', 'step_startTime' - The start date and time of the execution step.
--
-- 'status', 'step_status' - The status of the execution step.
--
-- 'endTime', 'step_endTime' - The end date and time of the execution step.
newStep ::
  -- | 'stepName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'status'
  JobStatus ->
  -- | 'endTime'
  Prelude.UTCTime ->
  Step
newStep pStepName_ pStartTime_ pStatus_ pEndTime_ =
  Step'
    { screenshots = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      logUrl = Prelude.Nothing,
      context = Prelude.Nothing,
      artifactsUrl = Prelude.Nothing,
      testConfigUrl = Prelude.Nothing,
      testArtifactsUrl = Prelude.Nothing,
      stepName = pStepName_,
      startTime = Data._Time Lens.# pStartTime_,
      status = pStatus_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | The list of screenshot URLs for the execution step, if relevant.
step_screenshots :: Lens.Lens' Step (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
step_screenshots = Lens.lens (\Step' {screenshots} -> screenshots) (\s@Step' {} a -> s {screenshots = a} :: Step) Prelude.. Lens.mapping Lens.coerced

-- | The reason for the current step status.
step_statusReason :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_statusReason = Lens.lens (\Step' {statusReason} -> statusReason) (\s@Step' {} a -> s {statusReason = a} :: Step)

-- | The URL to the logs for the execution step.
step_logUrl :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_logUrl = Lens.lens (\Step' {logUrl} -> logUrl) (\s@Step' {} a -> s {logUrl = a} :: Step)

-- | The context for the current step. Includes a build image if the step is
-- build.
step_context :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_context = Lens.lens (\Step' {context} -> context) (\s@Step' {} a -> s {context = a} :: Step)

-- | The URL to the artifact for the execution step.
step_artifactsUrl :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_artifactsUrl = Lens.lens (\Step' {artifactsUrl} -> artifactsUrl) (\s@Step' {} a -> s {artifactsUrl = a} :: Step)

-- | The URL to the test configuration for the execution step.
step_testConfigUrl :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_testConfigUrl = Lens.lens (\Step' {testConfigUrl} -> testConfigUrl) (\s@Step' {} a -> s {testConfigUrl = a} :: Step)

-- | The URL to the test artifact for the execution step.
step_testArtifactsUrl :: Lens.Lens' Step (Prelude.Maybe Prelude.Text)
step_testArtifactsUrl = Lens.lens (\Step' {testArtifactsUrl} -> testArtifactsUrl) (\s@Step' {} a -> s {testArtifactsUrl = a} :: Step)

-- | The name of the execution step.
step_stepName :: Lens.Lens' Step Prelude.Text
step_stepName = Lens.lens (\Step' {stepName} -> stepName) (\s@Step' {} a -> s {stepName = a} :: Step)

-- | The start date and time of the execution step.
step_startTime :: Lens.Lens' Step Prelude.UTCTime
step_startTime = Lens.lens (\Step' {startTime} -> startTime) (\s@Step' {} a -> s {startTime = a} :: Step) Prelude.. Data._Time

-- | The status of the execution step.
step_status :: Lens.Lens' Step JobStatus
step_status = Lens.lens (\Step' {status} -> status) (\s@Step' {} a -> s {status = a} :: Step)

-- | The end date and time of the execution step.
step_endTime :: Lens.Lens' Step Prelude.UTCTime
step_endTime = Lens.lens (\Step' {endTime} -> endTime) (\s@Step' {} a -> s {endTime = a} :: Step) Prelude.. Data._Time

instance Data.FromJSON Step where
  parseJSON =
    Data.withObject
      "Step"
      ( \x ->
          Step'
            Prelude.<$> (x Data..:? "screenshots" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "statusReason")
            Prelude.<*> (x Data..:? "logUrl")
            Prelude.<*> (x Data..:? "context")
            Prelude.<*> (x Data..:? "artifactsUrl")
            Prelude.<*> (x Data..:? "testConfigUrl")
            Prelude.<*> (x Data..:? "testArtifactsUrl")
            Prelude.<*> (x Data..: "stepName")
            Prelude.<*> (x Data..: "startTime")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "endTime")
      )

instance Prelude.Hashable Step where
  hashWithSalt _salt Step' {..} =
    _salt `Prelude.hashWithSalt` screenshots
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` logUrl
      `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` artifactsUrl
      `Prelude.hashWithSalt` testConfigUrl
      `Prelude.hashWithSalt` testArtifactsUrl
      `Prelude.hashWithSalt` stepName
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData Step where
  rnf Step' {..} =
    Prelude.rnf screenshots
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf logUrl
      `Prelude.seq` Prelude.rnf context
      `Prelude.seq` Prelude.rnf artifactsUrl
      `Prelude.seq` Prelude.rnf testConfigUrl
      `Prelude.seq` Prelude.rnf testArtifactsUrl
      `Prelude.seq` Prelude.rnf stepName
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf endTime
