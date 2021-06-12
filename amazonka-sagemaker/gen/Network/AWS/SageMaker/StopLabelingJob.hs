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
-- Module      : Network.AWS.SageMaker.StopLabelingJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running labeling job. A job that is stopped cannot be restarted.
-- Any results obtained before the job is stopped are placed in the Amazon
-- S3 output bucket.
module Network.AWS.SageMaker.StopLabelingJob
  ( -- * Creating a Request
    StopLabelingJob (..),
    newStopLabelingJob,

    -- * Request Lenses
    stopLabelingJob_labelingJobName,

    -- * Destructuring the Response
    StopLabelingJobResponse (..),
    newStopLabelingJobResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newStopLabelingJob' smart constructor.
data StopLabelingJob = StopLabelingJob'
  { -- | The name of the labeling job to stop.
    labelingJobName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopLabelingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelingJobName', 'stopLabelingJob_labelingJobName' - The name of the labeling job to stop.
newStopLabelingJob ::
  -- | 'labelingJobName'
  Core.Text ->
  StopLabelingJob
newStopLabelingJob pLabelingJobName_ =
  StopLabelingJob'
    { labelingJobName =
        pLabelingJobName_
    }

-- | The name of the labeling job to stop.
stopLabelingJob_labelingJobName :: Lens.Lens' StopLabelingJob Core.Text
stopLabelingJob_labelingJobName = Lens.lens (\StopLabelingJob' {labelingJobName} -> labelingJobName) (\s@StopLabelingJob' {} a -> s {labelingJobName = a} :: StopLabelingJob)

instance Core.AWSRequest StopLabelingJob where
  type
    AWSResponse StopLabelingJob =
      StopLabelingJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopLabelingJobResponse'

instance Core.Hashable StopLabelingJob

instance Core.NFData StopLabelingJob

instance Core.ToHeaders StopLabelingJob where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.StopLabelingJob" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopLabelingJob where
  toJSON StopLabelingJob' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("LabelingJobName" Core..= labelingJobName)
          ]
      )

instance Core.ToPath StopLabelingJob where
  toPath = Core.const "/"

instance Core.ToQuery StopLabelingJob where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopLabelingJobResponse' smart constructor.
data StopLabelingJobResponse = StopLabelingJobResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopLabelingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopLabelingJobResponse ::
  StopLabelingJobResponse
newStopLabelingJobResponse = StopLabelingJobResponse'

instance Core.NFData StopLabelingJobResponse
