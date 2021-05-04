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
-- Module      : Network.AWS.SageMaker.StopTrainingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a training job. To stop a job, Amazon SageMaker sends the
-- algorithm the @SIGTERM@ signal, which delays job termination for 120
-- seconds. Algorithms might use this 120-second window to save the model
-- artifacts, so the results of the training is not lost.
--
-- When it receives a @StopTrainingJob@ request, Amazon SageMaker changes
-- the status of the job to @Stopping@. After Amazon SageMaker stops the
-- job, it sets the status to @Stopped@.
module Network.AWS.SageMaker.StopTrainingJob
  ( -- * Creating a Request
    StopTrainingJob (..),
    newStopTrainingJob,

    -- * Request Lenses
    stopTrainingJob_trainingJobName,

    -- * Destructuring the Response
    StopTrainingJobResponse (..),
    newStopTrainingJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopTrainingJob' smart constructor.
data StopTrainingJob = StopTrainingJob'
  { -- | The name of the training job to stop.
    trainingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTrainingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingJobName', 'stopTrainingJob_trainingJobName' - The name of the training job to stop.
newStopTrainingJob ::
  -- | 'trainingJobName'
  Prelude.Text ->
  StopTrainingJob
newStopTrainingJob pTrainingJobName_ =
  StopTrainingJob'
    { trainingJobName =
        pTrainingJobName_
    }

-- | The name of the training job to stop.
stopTrainingJob_trainingJobName :: Lens.Lens' StopTrainingJob Prelude.Text
stopTrainingJob_trainingJobName = Lens.lens (\StopTrainingJob' {trainingJobName} -> trainingJobName) (\s@StopTrainingJob' {} a -> s {trainingJobName = a} :: StopTrainingJob)

instance Prelude.AWSRequest StopTrainingJob where
  type Rs StopTrainingJob = StopTrainingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopTrainingJobResponse'

instance Prelude.Hashable StopTrainingJob

instance Prelude.NFData StopTrainingJob

instance Prelude.ToHeaders StopTrainingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.StopTrainingJob" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopTrainingJob where
  toJSON StopTrainingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TrainingJobName" Prelude..= trainingJobName)
          ]
      )

instance Prelude.ToPath StopTrainingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopTrainingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTrainingJobResponse' smart constructor.
data StopTrainingJobResponse = StopTrainingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTrainingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopTrainingJobResponse ::
  StopTrainingJobResponse
newStopTrainingJobResponse = StopTrainingJobResponse'

instance Prelude.NFData StopTrainingJobResponse
