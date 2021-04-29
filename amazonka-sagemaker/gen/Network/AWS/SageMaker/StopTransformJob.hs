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
-- Module      : Network.AWS.SageMaker.StopTransformJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a transform job.
--
-- When Amazon SageMaker receives a @StopTransformJob@ request, the status
-- of the job changes to @Stopping@. After Amazon SageMaker stops the job,
-- the status is set to @Stopped@. When you stop a transform job before it
-- is completed, Amazon SageMaker doesn\'t store the job\'s output in
-- Amazon S3.
module Network.AWS.SageMaker.StopTransformJob
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopTransformJob' smart constructor.
data StopTransformJob = StopTransformJob'
  { -- | The name of the transform job to stop.
    transformJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTransformJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transformJobName', 'stopTransformJob_transformJobName' - The name of the transform job to stop.
newStopTransformJob ::
  -- | 'transformJobName'
  Prelude.Text ->
  StopTransformJob
newStopTransformJob pTransformJobName_ =
  StopTransformJob'
    { transformJobName =
        pTransformJobName_
    }

-- | The name of the transform job to stop.
stopTransformJob_transformJobName :: Lens.Lens' StopTransformJob Prelude.Text
stopTransformJob_transformJobName = Lens.lens (\StopTransformJob' {transformJobName} -> transformJobName) (\s@StopTransformJob' {} a -> s {transformJobName = a} :: StopTransformJob)

instance Prelude.AWSRequest StopTransformJob where
  type Rs StopTransformJob = StopTransformJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopTransformJobResponse'

instance Prelude.Hashable StopTransformJob

instance Prelude.NFData StopTransformJob

instance Prelude.ToHeaders StopTransformJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.StopTransformJob" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopTransformJob where
  toJSON StopTransformJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TransformJobName" Prelude..= transformJobName)
          ]
      )

instance Prelude.ToPath StopTransformJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopTransformJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopTransformJobResponse' smart constructor.
data StopTransformJobResponse = StopTransformJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopTransformJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopTransformJobResponse ::
  StopTransformJobResponse
newStopTransformJobResponse =
  StopTransformJobResponse'

instance Prelude.NFData StopTransformJobResponse
