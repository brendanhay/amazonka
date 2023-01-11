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
-- Module      : Amazonka.SageMaker.StopInferenceRecommendationsJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Inference Recommender job.
module Amazonka.SageMaker.StopInferenceRecommendationsJob
  ( -- * Creating a Request
    StopInferenceRecommendationsJob (..),
    newStopInferenceRecommendationsJob,

    -- * Request Lenses
    stopInferenceRecommendationsJob_jobName,

    -- * Destructuring the Response
    StopInferenceRecommendationsJobResponse (..),
    newStopInferenceRecommendationsJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopInferenceRecommendationsJob' smart constructor.
data StopInferenceRecommendationsJob = StopInferenceRecommendationsJob'
  { -- | The name of the job you want to stop.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInferenceRecommendationsJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'stopInferenceRecommendationsJob_jobName' - The name of the job you want to stop.
newStopInferenceRecommendationsJob ::
  -- | 'jobName'
  Prelude.Text ->
  StopInferenceRecommendationsJob
newStopInferenceRecommendationsJob pJobName_ =
  StopInferenceRecommendationsJob'
    { jobName =
        pJobName_
    }

-- | The name of the job you want to stop.
stopInferenceRecommendationsJob_jobName :: Lens.Lens' StopInferenceRecommendationsJob Prelude.Text
stopInferenceRecommendationsJob_jobName = Lens.lens (\StopInferenceRecommendationsJob' {jobName} -> jobName) (\s@StopInferenceRecommendationsJob' {} a -> s {jobName = a} :: StopInferenceRecommendationsJob)

instance
  Core.AWSRequest
    StopInferenceRecommendationsJob
  where
  type
    AWSResponse StopInferenceRecommendationsJob =
      StopInferenceRecommendationsJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StopInferenceRecommendationsJobResponse'

instance
  Prelude.Hashable
    StopInferenceRecommendationsJob
  where
  hashWithSalt
    _salt
    StopInferenceRecommendationsJob' {..} =
      _salt `Prelude.hashWithSalt` jobName

instance
  Prelude.NFData
    StopInferenceRecommendationsJob
  where
  rnf StopInferenceRecommendationsJob' {..} =
    Prelude.rnf jobName

instance
  Data.ToHeaders
    StopInferenceRecommendationsJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopInferenceRecommendationsJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopInferenceRecommendationsJob where
  toJSON StopInferenceRecommendationsJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobName" Data..= jobName)]
      )

instance Data.ToPath StopInferenceRecommendationsJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopInferenceRecommendationsJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopInferenceRecommendationsJobResponse' smart constructor.
data StopInferenceRecommendationsJobResponse = StopInferenceRecommendationsJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopInferenceRecommendationsJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopInferenceRecommendationsJobResponse ::
  StopInferenceRecommendationsJobResponse
newStopInferenceRecommendationsJobResponse =
  StopInferenceRecommendationsJobResponse'

instance
  Prelude.NFData
    StopInferenceRecommendationsJobResponse
  where
  rnf _ = ()
