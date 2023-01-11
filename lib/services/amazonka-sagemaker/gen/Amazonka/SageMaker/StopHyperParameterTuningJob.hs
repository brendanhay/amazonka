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
-- Module      : Amazonka.SageMaker.StopHyperParameterTuningJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running hyperparameter tuning job and all running training jobs
-- that the tuning job launched.
--
-- All model artifacts output from the training jobs are stored in Amazon
-- Simple Storage Service (Amazon S3). All data that the training jobs
-- write to Amazon CloudWatch Logs are still available in CloudWatch. After
-- the tuning job moves to the @Stopped@ state, it releases all reserved
-- resources for the tuning job.
module Amazonka.SageMaker.StopHyperParameterTuningJob
  ( -- * Creating a Request
    StopHyperParameterTuningJob (..),
    newStopHyperParameterTuningJob,

    -- * Request Lenses
    stopHyperParameterTuningJob_hyperParameterTuningJobName,

    -- * Destructuring the Response
    StopHyperParameterTuningJobResponse (..),
    newStopHyperParameterTuningJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopHyperParameterTuningJob' smart constructor.
data StopHyperParameterTuningJob = StopHyperParameterTuningJob'
  { -- | The name of the tuning job to stop.
    hyperParameterTuningJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopHyperParameterTuningJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperParameterTuningJobName', 'stopHyperParameterTuningJob_hyperParameterTuningJobName' - The name of the tuning job to stop.
newStopHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Prelude.Text ->
  StopHyperParameterTuningJob
newStopHyperParameterTuningJob
  pHyperParameterTuningJobName_ =
    StopHyperParameterTuningJob'
      { hyperParameterTuningJobName =
          pHyperParameterTuningJobName_
      }

-- | The name of the tuning job to stop.
stopHyperParameterTuningJob_hyperParameterTuningJobName :: Lens.Lens' StopHyperParameterTuningJob Prelude.Text
stopHyperParameterTuningJob_hyperParameterTuningJobName = Lens.lens (\StopHyperParameterTuningJob' {hyperParameterTuningJobName} -> hyperParameterTuningJobName) (\s@StopHyperParameterTuningJob' {} a -> s {hyperParameterTuningJobName = a} :: StopHyperParameterTuningJob)

instance Core.AWSRequest StopHyperParameterTuningJob where
  type
    AWSResponse StopHyperParameterTuningJob =
      StopHyperParameterTuningJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StopHyperParameterTuningJobResponse'

instance Prelude.Hashable StopHyperParameterTuningJob where
  hashWithSalt _salt StopHyperParameterTuningJob' {..} =
    _salt
      `Prelude.hashWithSalt` hyperParameterTuningJobName

instance Prelude.NFData StopHyperParameterTuningJob where
  rnf StopHyperParameterTuningJob' {..} =
    Prelude.rnf hyperParameterTuningJobName

instance Data.ToHeaders StopHyperParameterTuningJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopHyperParameterTuningJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopHyperParameterTuningJob where
  toJSON StopHyperParameterTuningJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HyperParameterTuningJobName"
                  Data..= hyperParameterTuningJobName
              )
          ]
      )

instance Data.ToPath StopHyperParameterTuningJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopHyperParameterTuningJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopHyperParameterTuningJobResponse' smart constructor.
data StopHyperParameterTuningJobResponse = StopHyperParameterTuningJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopHyperParameterTuningJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopHyperParameterTuningJobResponse ::
  StopHyperParameterTuningJobResponse
newStopHyperParameterTuningJobResponse =
  StopHyperParameterTuningJobResponse'

instance
  Prelude.NFData
    StopHyperParameterTuningJobResponse
  where
  rnf _ = ()
