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
-- Module      : Amazonka.Evidently.StartExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing experiment. To create an experiment, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_CreateExperiment.html CreateExperiment>.
module Amazonka.Evidently.StartExperiment
  ( -- * Creating a Request
    StartExperiment (..),
    newStartExperiment,

    -- * Request Lenses
    startExperiment_analysisCompleteTime,
    startExperiment_experiment,
    startExperiment_project,

    -- * Destructuring the Response
    StartExperimentResponse (..),
    newStartExperimentResponse,

    -- * Response Lenses
    startExperimentResponse_startedTime,
    startExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartExperiment' smart constructor.
data StartExperiment = StartExperiment'
  { -- | The date and time to end the experiment. This must be no more than 30
    -- days after the experiment starts.
    analysisCompleteTime :: Data.POSIX,
    -- | The name of the experiment to start.
    experiment :: Prelude.Text,
    -- | The name or ARN of the project that contains the experiment to start.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisCompleteTime', 'startExperiment_analysisCompleteTime' - The date and time to end the experiment. This must be no more than 30
-- days after the experiment starts.
--
-- 'experiment', 'startExperiment_experiment' - The name of the experiment to start.
--
-- 'project', 'startExperiment_project' - The name or ARN of the project that contains the experiment to start.
newStartExperiment ::
  -- | 'analysisCompleteTime'
  Prelude.UTCTime ->
  -- | 'experiment'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  StartExperiment
newStartExperiment
  pAnalysisCompleteTime_
  pExperiment_
  pProject_ =
    StartExperiment'
      { analysisCompleteTime =
          Data._Time Lens.# pAnalysisCompleteTime_,
        experiment = pExperiment_,
        project = pProject_
      }

-- | The date and time to end the experiment. This must be no more than 30
-- days after the experiment starts.
startExperiment_analysisCompleteTime :: Lens.Lens' StartExperiment Prelude.UTCTime
startExperiment_analysisCompleteTime = Lens.lens (\StartExperiment' {analysisCompleteTime} -> analysisCompleteTime) (\s@StartExperiment' {} a -> s {analysisCompleteTime = a} :: StartExperiment) Prelude.. Data._Time

-- | The name of the experiment to start.
startExperiment_experiment :: Lens.Lens' StartExperiment Prelude.Text
startExperiment_experiment = Lens.lens (\StartExperiment' {experiment} -> experiment) (\s@StartExperiment' {} a -> s {experiment = a} :: StartExperiment)

-- | The name or ARN of the project that contains the experiment to start.
startExperiment_project :: Lens.Lens' StartExperiment Prelude.Text
startExperiment_project = Lens.lens (\StartExperiment' {project} -> project) (\s@StartExperiment' {} a -> s {project = a} :: StartExperiment)

instance Core.AWSRequest StartExperiment where
  type
    AWSResponse StartExperiment =
      StartExperimentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExperimentResponse'
            Prelude.<$> (x Data..?> "startedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartExperiment where
  hashWithSalt _salt StartExperiment' {..} =
    _salt
      `Prelude.hashWithSalt` analysisCompleteTime
      `Prelude.hashWithSalt` experiment
      `Prelude.hashWithSalt` project

instance Prelude.NFData StartExperiment where
  rnf StartExperiment' {..} =
    Prelude.rnf analysisCompleteTime
      `Prelude.seq` Prelude.rnf experiment
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders StartExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartExperiment where
  toJSON StartExperiment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "analysisCompleteTime"
                  Data..= analysisCompleteTime
              )
          ]
      )

instance Data.ToPath StartExperiment where
  toPath StartExperiment' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/experiments/",
        Data.toBS experiment,
        "/start"
      ]

instance Data.ToQuery StartExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartExperimentResponse' smart constructor.
data StartExperimentResponse = StartExperimentResponse'
  { -- | A timestamp that indicates when the experiment started.
    startedTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startedTime', 'startExperimentResponse_startedTime' - A timestamp that indicates when the experiment started.
--
-- 'httpStatus', 'startExperimentResponse_httpStatus' - The response's http status code.
newStartExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartExperimentResponse
newStartExperimentResponse pHttpStatus_ =
  StartExperimentResponse'
    { startedTime =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A timestamp that indicates when the experiment started.
startExperimentResponse_startedTime :: Lens.Lens' StartExperimentResponse (Prelude.Maybe Prelude.UTCTime)
startExperimentResponse_startedTime = Lens.lens (\StartExperimentResponse' {startedTime} -> startedTime) (\s@StartExperimentResponse' {} a -> s {startedTime = a} :: StartExperimentResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
startExperimentResponse_httpStatus :: Lens.Lens' StartExperimentResponse Prelude.Int
startExperimentResponse_httpStatus = Lens.lens (\StartExperimentResponse' {httpStatus} -> httpStatus) (\s@StartExperimentResponse' {} a -> s {httpStatus = a} :: StartExperimentResponse)

instance Prelude.NFData StartExperimentResponse where
  rnf StartExperimentResponse' {..} =
    Prelude.rnf startedTime
      `Prelude.seq` Prelude.rnf httpStatus
