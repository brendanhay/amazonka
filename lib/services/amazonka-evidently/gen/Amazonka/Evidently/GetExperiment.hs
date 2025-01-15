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
-- Module      : Amazonka.Evidently.GetExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details about one experiment. You must already know the
-- experiment name. To retrieve a list of experiments in your account, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_ListExperiments.html ListExperiments>.
module Amazonka.Evidently.GetExperiment
  ( -- * Creating a Request
    GetExperiment (..),
    newGetExperiment,

    -- * Request Lenses
    getExperiment_experiment,
    getExperiment_project,

    -- * Destructuring the Response
    GetExperimentResponse (..),
    newGetExperimentResponse,

    -- * Response Lenses
    getExperimentResponse_experiment,
    getExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExperiment' smart constructor.
data GetExperiment = GetExperiment'
  { -- | The name of the experiment that you want to see the details of.
    experiment :: Prelude.Text,
    -- | The name or ARN of the project that contains the experiment.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiment', 'getExperiment_experiment' - The name of the experiment that you want to see the details of.
--
-- 'project', 'getExperiment_project' - The name or ARN of the project that contains the experiment.
newGetExperiment ::
  -- | 'experiment'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  GetExperiment
newGetExperiment pExperiment_ pProject_ =
  GetExperiment'
    { experiment = pExperiment_,
      project = pProject_
    }

-- | The name of the experiment that you want to see the details of.
getExperiment_experiment :: Lens.Lens' GetExperiment Prelude.Text
getExperiment_experiment = Lens.lens (\GetExperiment' {experiment} -> experiment) (\s@GetExperiment' {} a -> s {experiment = a} :: GetExperiment)

-- | The name or ARN of the project that contains the experiment.
getExperiment_project :: Lens.Lens' GetExperiment Prelude.Text
getExperiment_project = Lens.lens (\GetExperiment' {project} -> project) (\s@GetExperiment' {} a -> s {project = a} :: GetExperiment)

instance Core.AWSRequest GetExperiment where
  type
    AWSResponse GetExperiment =
      GetExperimentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExperimentResponse'
            Prelude.<$> (x Data..?> "experiment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExperiment where
  hashWithSalt _salt GetExperiment' {..} =
    _salt
      `Prelude.hashWithSalt` experiment
      `Prelude.hashWithSalt` project

instance Prelude.NFData GetExperiment where
  rnf GetExperiment' {..} =
    Prelude.rnf experiment `Prelude.seq`
      Prelude.rnf project

instance Data.ToHeaders GetExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExperiment where
  toPath GetExperiment' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/experiments/",
        Data.toBS experiment
      ]

instance Data.ToQuery GetExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExperimentResponse' smart constructor.
data GetExperimentResponse = GetExperimentResponse'
  { -- | A structure containing the configuration details of the experiment.
    experiment :: Prelude.Maybe Experiment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiment', 'getExperimentResponse_experiment' - A structure containing the configuration details of the experiment.
--
-- 'httpStatus', 'getExperimentResponse_httpStatus' - The response's http status code.
newGetExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExperimentResponse
newGetExperimentResponse pHttpStatus_ =
  GetExperimentResponse'
    { experiment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing the configuration details of the experiment.
getExperimentResponse_experiment :: Lens.Lens' GetExperimentResponse (Prelude.Maybe Experiment)
getExperimentResponse_experiment = Lens.lens (\GetExperimentResponse' {experiment} -> experiment) (\s@GetExperimentResponse' {} a -> s {experiment = a} :: GetExperimentResponse)

-- | The response's http status code.
getExperimentResponse_httpStatus :: Lens.Lens' GetExperimentResponse Prelude.Int
getExperimentResponse_httpStatus = Lens.lens (\GetExperimentResponse' {httpStatus} -> httpStatus) (\s@GetExperimentResponse' {} a -> s {httpStatus = a} :: GetExperimentResponse)

instance Prelude.NFData GetExperimentResponse where
  rnf GetExperimentResponse' {..} =
    Prelude.rnf experiment `Prelude.seq`
      Prelude.rnf httpStatus
