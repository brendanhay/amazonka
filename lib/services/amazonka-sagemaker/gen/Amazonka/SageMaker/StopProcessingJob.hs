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
-- Module      : Amazonka.SageMaker.StopProcessingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a processing job.
module Amazonka.SageMaker.StopProcessingJob
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopProcessingJob' smart constructor.
data StopProcessingJob = StopProcessingJob'
  { -- | The name of the processing job to stop.
    processingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest StopProcessingJob where
  type
    AWSResponse StopProcessingJob =
      StopProcessingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopProcessingJobResponse'

instance Prelude.Hashable StopProcessingJob where
  hashWithSalt _salt StopProcessingJob' {..} =
    _salt `Prelude.hashWithSalt` processingJobName

instance Prelude.NFData StopProcessingJob where
  rnf StopProcessingJob' {..} =
    Prelude.rnf processingJobName

instance Data.ToHeaders StopProcessingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopProcessingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopProcessingJob where
  toJSON StopProcessingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProcessingJobName" Data..= processingJobName)
          ]
      )

instance Data.ToPath StopProcessingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopProcessingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopProcessingJobResponse' smart constructor.
data StopProcessingJobResponse = StopProcessingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopProcessingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopProcessingJobResponse ::
  StopProcessingJobResponse
newStopProcessingJobResponse =
  StopProcessingJobResponse'

instance Prelude.NFData StopProcessingJobResponse where
  rnf _ = ()
