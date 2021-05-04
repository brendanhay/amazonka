{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.StopHyperParameterTuningJob
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SageMaker.StopHyperParameterTuningJob
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopHyperParameterTuningJob' smart constructor.
data StopHyperParameterTuningJob = StopHyperParameterTuningJob'
  { -- | The name of the tuning job to stop.
    hyperParameterTuningJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.AWSRequest
    StopHyperParameterTuningJob
  where
  type
    Rs StopHyperParameterTuningJob =
      StopHyperParameterTuningJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      StopHyperParameterTuningJobResponse'

instance Prelude.Hashable StopHyperParameterTuningJob

instance Prelude.NFData StopHyperParameterTuningJob

instance
  Prelude.ToHeaders
    StopHyperParameterTuningJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.StopHyperParameterTuningJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopHyperParameterTuningJob where
  toJSON StopHyperParameterTuningJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HyperParameterTuningJobName"
                  Prelude..= hyperParameterTuningJobName
              )
          ]
      )

instance Prelude.ToPath StopHyperParameterTuningJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopHyperParameterTuningJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopHyperParameterTuningJobResponse' smart constructor.
data StopHyperParameterTuningJobResponse = StopHyperParameterTuningJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
