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
-- Module      : Amazonka.Evidently.StopExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an experiment that is currently running. If you stop an
-- experiment, you can\'t resume it or restart it.
module Amazonka.Evidently.StopExperiment
  ( -- * Creating a Request
    StopExperiment (..),
    newStopExperiment,

    -- * Request Lenses
    stopExperiment_desiredState,
    stopExperiment_reason,
    stopExperiment_experiment,
    stopExperiment_project,

    -- * Destructuring the Response
    StopExperimentResponse (..),
    newStopExperimentResponse,

    -- * Response Lenses
    stopExperimentResponse_endedTime,
    stopExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopExperiment' smart constructor.
data StopExperiment = StopExperiment'
  { -- | Specify whether the experiment is to be considered @COMPLETED@ or
    -- @CANCELLED@ after it stops.
    desiredState :: Prelude.Maybe ExperimentStopDesiredState,
    -- | A string that describes why you are stopping the experiment.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the experiment to stop.
    experiment :: Prelude.Text,
    -- | The name or ARN of the project that contains the experiment to stop.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'stopExperiment_desiredState' - Specify whether the experiment is to be considered @COMPLETED@ or
-- @CANCELLED@ after it stops.
--
-- 'reason', 'stopExperiment_reason' - A string that describes why you are stopping the experiment.
--
-- 'experiment', 'stopExperiment_experiment' - The name of the experiment to stop.
--
-- 'project', 'stopExperiment_project' - The name or ARN of the project that contains the experiment to stop.
newStopExperiment ::
  -- | 'experiment'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  StopExperiment
newStopExperiment pExperiment_ pProject_ =
  StopExperiment'
    { desiredState = Prelude.Nothing,
      reason = Prelude.Nothing,
      experiment = pExperiment_,
      project = pProject_
    }

-- | Specify whether the experiment is to be considered @COMPLETED@ or
-- @CANCELLED@ after it stops.
stopExperiment_desiredState :: Lens.Lens' StopExperiment (Prelude.Maybe ExperimentStopDesiredState)
stopExperiment_desiredState = Lens.lens (\StopExperiment' {desiredState} -> desiredState) (\s@StopExperiment' {} a -> s {desiredState = a} :: StopExperiment)

-- | A string that describes why you are stopping the experiment.
stopExperiment_reason :: Lens.Lens' StopExperiment (Prelude.Maybe Prelude.Text)
stopExperiment_reason = Lens.lens (\StopExperiment' {reason} -> reason) (\s@StopExperiment' {} a -> s {reason = a} :: StopExperiment)

-- | The name of the experiment to stop.
stopExperiment_experiment :: Lens.Lens' StopExperiment Prelude.Text
stopExperiment_experiment = Lens.lens (\StopExperiment' {experiment} -> experiment) (\s@StopExperiment' {} a -> s {experiment = a} :: StopExperiment)

-- | The name or ARN of the project that contains the experiment to stop.
stopExperiment_project :: Lens.Lens' StopExperiment Prelude.Text
stopExperiment_project = Lens.lens (\StopExperiment' {project} -> project) (\s@StopExperiment' {} a -> s {project = a} :: StopExperiment)

instance Core.AWSRequest StopExperiment where
  type
    AWSResponse StopExperiment =
      StopExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopExperimentResponse'
            Prelude.<$> (x Data..?> "endedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopExperiment where
  hashWithSalt _salt StopExperiment' {..} =
    _salt `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` experiment
      `Prelude.hashWithSalt` project

instance Prelude.NFData StopExperiment where
  rnf StopExperiment' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf experiment
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders StopExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopExperiment where
  toJSON StopExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("desiredState" Data..=) Prelude.<$> desiredState,
            ("reason" Data..=) Prelude.<$> reason
          ]
      )

instance Data.ToPath StopExperiment where
  toPath StopExperiment' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/experiments/",
        Data.toBS experiment,
        "/cancel"
      ]

instance Data.ToQuery StopExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopExperimentResponse' smart constructor.
data StopExperimentResponse = StopExperimentResponse'
  { -- | The date and time that the experiment stopped.
    endedTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endedTime', 'stopExperimentResponse_endedTime' - The date and time that the experiment stopped.
--
-- 'httpStatus', 'stopExperimentResponse_httpStatus' - The response's http status code.
newStopExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopExperimentResponse
newStopExperimentResponse pHttpStatus_ =
  StopExperimentResponse'
    { endedTime =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the experiment stopped.
stopExperimentResponse_endedTime :: Lens.Lens' StopExperimentResponse (Prelude.Maybe Prelude.UTCTime)
stopExperimentResponse_endedTime = Lens.lens (\StopExperimentResponse' {endedTime} -> endedTime) (\s@StopExperimentResponse' {} a -> s {endedTime = a} :: StopExperimentResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
stopExperimentResponse_httpStatus :: Lens.Lens' StopExperimentResponse Prelude.Int
stopExperimentResponse_httpStatus = Lens.lens (\StopExperimentResponse' {httpStatus} -> httpStatus) (\s@StopExperimentResponse' {} a -> s {httpStatus = a} :: StopExperimentResponse)

instance Prelude.NFData StopExperimentResponse where
  rnf StopExperimentResponse' {..} =
    Prelude.rnf endedTime
      `Prelude.seq` Prelude.rnf httpStatus
