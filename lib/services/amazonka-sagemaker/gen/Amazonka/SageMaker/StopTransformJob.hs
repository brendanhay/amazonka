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
-- Module      : Amazonka.SageMaker.StopTransformJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a batch transform job.
--
-- When Amazon SageMaker receives a @StopTransformJob@ request, the status
-- of the job changes to @Stopping@. After Amazon SageMaker stops the job,
-- the status is set to @Stopped@. When you stop a batch transform job
-- before it is completed, Amazon SageMaker doesn\'t store the job\'s
-- output in Amazon S3.
module Amazonka.SageMaker.StopTransformJob
  ( -- * Creating a Request
    StopTransformJob (..),
    newStopTransformJob,

    -- * Request Lenses
    stopTransformJob_transformJobName,

    -- * Destructuring the Response
    StopTransformJobResponse (..),
    newStopTransformJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopTransformJob' smart constructor.
data StopTransformJob = StopTransformJob'
  { -- | The name of the batch transform job to stop.
    transformJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTransformJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformJobName', 'stopTransformJob_transformJobName' - The name of the batch transform job to stop.
newStopTransformJob ::
  -- | 'transformJobName'
  Prelude.Text ->
  StopTransformJob
newStopTransformJob pTransformJobName_ =
  StopTransformJob'
    { transformJobName =
        pTransformJobName_
    }

-- | The name of the batch transform job to stop.
stopTransformJob_transformJobName :: Lens.Lens' StopTransformJob Prelude.Text
stopTransformJob_transformJobName = Lens.lens (\StopTransformJob' {transformJobName} -> transformJobName) (\s@StopTransformJob' {} a -> s {transformJobName = a} :: StopTransformJob)

instance Core.AWSRequest StopTransformJob where
  type
    AWSResponse StopTransformJob =
      StopTransformJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopTransformJobResponse'

instance Prelude.Hashable StopTransformJob where
  hashWithSalt _salt StopTransformJob' {..} =
    _salt `Prelude.hashWithSalt` transformJobName

instance Prelude.NFData StopTransformJob where
  rnf StopTransformJob' {..} =
    Prelude.rnf transformJobName

instance Data.ToHeaders StopTransformJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.StopTransformJob" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopTransformJob where
  toJSON StopTransformJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransformJobName" Data..= transformJobName)
          ]
      )

instance Data.ToPath StopTransformJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopTransformJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTransformJobResponse' smart constructor.
data StopTransformJobResponse = StopTransformJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopTransformJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopTransformJobResponse ::
  StopTransformJobResponse
newStopTransformJobResponse =
  StopTransformJobResponse'

instance Prelude.NFData StopTransformJobResponse where
  rnf _ = ()
