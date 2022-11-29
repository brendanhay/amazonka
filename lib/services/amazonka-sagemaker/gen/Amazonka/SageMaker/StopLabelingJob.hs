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
-- Module      : Amazonka.SageMaker.StopLabelingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running labeling job. A job that is stopped cannot be restarted.
-- Any results obtained before the job is stopped are placed in the Amazon
-- S3 output bucket.
module Amazonka.SageMaker.StopLabelingJob
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopLabelingJob' smart constructor.
data StopLabelingJob = StopLabelingJob'
  { -- | The name of the labeling job to stop.
    labelingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  StopLabelingJob
newStopLabelingJob pLabelingJobName_ =
  StopLabelingJob'
    { labelingJobName =
        pLabelingJobName_
    }

-- | The name of the labeling job to stop.
stopLabelingJob_labelingJobName :: Lens.Lens' StopLabelingJob Prelude.Text
stopLabelingJob_labelingJobName = Lens.lens (\StopLabelingJob' {labelingJobName} -> labelingJobName) (\s@StopLabelingJob' {} a -> s {labelingJobName = a} :: StopLabelingJob)

instance Core.AWSRequest StopLabelingJob where
  type
    AWSResponse StopLabelingJob =
      StopLabelingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopLabelingJobResponse'

instance Prelude.Hashable StopLabelingJob where
  hashWithSalt _salt StopLabelingJob' {..} =
    _salt `Prelude.hashWithSalt` labelingJobName

instance Prelude.NFData StopLabelingJob where
  rnf StopLabelingJob' {..} =
    Prelude.rnf labelingJobName

instance Core.ToHeaders StopLabelingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.StopLabelingJob" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopLabelingJob where
  toJSON StopLabelingJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LabelingJobName" Core..= labelingJobName)
          ]
      )

instance Core.ToPath StopLabelingJob where
  toPath = Prelude.const "/"

instance Core.ToQuery StopLabelingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopLabelingJobResponse' smart constructor.
data StopLabelingJobResponse = StopLabelingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopLabelingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopLabelingJobResponse ::
  StopLabelingJobResponse
newStopLabelingJobResponse = StopLabelingJobResponse'

instance Prelude.NFData StopLabelingJobResponse where
  rnf _ = ()
