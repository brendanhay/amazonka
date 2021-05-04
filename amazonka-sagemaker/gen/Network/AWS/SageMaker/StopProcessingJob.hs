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
-- Module      : Network.AWS.SageMaker.StopProcessingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a processing job.
module Network.AWS.SageMaker.StopProcessingJob
  ( -- * Creating a Request
    StopProcessingJob (..),
    newStopProcessingJob,

    -- * Request Lenses
    stopProcessingJob_processingJobName,

    -- * Destructuring the Response
    StopProcessingJobResponse (..),
    newStopProcessingJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopProcessingJob' smart constructor.
data StopProcessingJob = StopProcessingJob'
  { -- | The name of the processing job to stop.
    processingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopProcessingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingJobName', 'stopProcessingJob_processingJobName' - The name of the processing job to stop.
newStopProcessingJob ::
  -- | 'processingJobName'
  Prelude.Text ->
  StopProcessingJob
newStopProcessingJob pProcessingJobName_ =
  StopProcessingJob'
    { processingJobName =
        pProcessingJobName_
    }

-- | The name of the processing job to stop.
stopProcessingJob_processingJobName :: Lens.Lens' StopProcessingJob Prelude.Text
stopProcessingJob_processingJobName = Lens.lens (\StopProcessingJob' {processingJobName} -> processingJobName) (\s@StopProcessingJob' {} a -> s {processingJobName = a} :: StopProcessingJob)

instance Prelude.AWSRequest StopProcessingJob where
  type Rs StopProcessingJob = StopProcessingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopProcessingJobResponse'

instance Prelude.Hashable StopProcessingJob

instance Prelude.NFData StopProcessingJob

instance Prelude.ToHeaders StopProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.StopProcessingJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopProcessingJob where
  toJSON StopProcessingJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProcessingJobName" Prelude..= processingJobName)
          ]
      )

instance Prelude.ToPath StopProcessingJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopProcessingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopProcessingJobResponse' smart constructor.
data StopProcessingJobResponse = StopProcessingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopProcessingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopProcessingJobResponse ::
  StopProcessingJobResponse
newStopProcessingJobResponse =
  StopProcessingJobResponse'

instance Prelude.NFData StopProcessingJobResponse
