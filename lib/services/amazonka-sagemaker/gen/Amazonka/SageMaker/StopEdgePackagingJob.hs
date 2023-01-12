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
-- Module      : Amazonka.SageMaker.StopEdgePackagingJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request to stop an edge packaging job.
module Amazonka.SageMaker.StopEdgePackagingJob
  ( -- * Creating a Request
    StopEdgePackagingJob (..),
    newStopEdgePackagingJob,

    -- * Request Lenses
    stopEdgePackagingJob_edgePackagingJobName,

    -- * Destructuring the Response
    StopEdgePackagingJobResponse (..),
    newStopEdgePackagingJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopEdgePackagingJob' smart constructor.
data StopEdgePackagingJob = StopEdgePackagingJob'
  { -- | The name of the edge packaging job.
    edgePackagingJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEdgePackagingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgePackagingJobName', 'stopEdgePackagingJob_edgePackagingJobName' - The name of the edge packaging job.
newStopEdgePackagingJob ::
  -- | 'edgePackagingJobName'
  Prelude.Text ->
  StopEdgePackagingJob
newStopEdgePackagingJob pEdgePackagingJobName_ =
  StopEdgePackagingJob'
    { edgePackagingJobName =
        pEdgePackagingJobName_
    }

-- | The name of the edge packaging job.
stopEdgePackagingJob_edgePackagingJobName :: Lens.Lens' StopEdgePackagingJob Prelude.Text
stopEdgePackagingJob_edgePackagingJobName = Lens.lens (\StopEdgePackagingJob' {edgePackagingJobName} -> edgePackagingJobName) (\s@StopEdgePackagingJob' {} a -> s {edgePackagingJobName = a} :: StopEdgePackagingJob)

instance Core.AWSRequest StopEdgePackagingJob where
  type
    AWSResponse StopEdgePackagingJob =
      StopEdgePackagingJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopEdgePackagingJobResponse'

instance Prelude.Hashable StopEdgePackagingJob where
  hashWithSalt _salt StopEdgePackagingJob' {..} =
    _salt `Prelude.hashWithSalt` edgePackagingJobName

instance Prelude.NFData StopEdgePackagingJob where
  rnf StopEdgePackagingJob' {..} =
    Prelude.rnf edgePackagingJobName

instance Data.ToHeaders StopEdgePackagingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopEdgePackagingJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopEdgePackagingJob where
  toJSON StopEdgePackagingJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgePackagingJobName"
                  Data..= edgePackagingJobName
              )
          ]
      )

instance Data.ToPath StopEdgePackagingJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopEdgePackagingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEdgePackagingJobResponse' smart constructor.
data StopEdgePackagingJobResponse = StopEdgePackagingJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEdgePackagingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopEdgePackagingJobResponse ::
  StopEdgePackagingJobResponse
newStopEdgePackagingJobResponse =
  StopEdgePackagingJobResponse'

instance Prelude.NFData StopEdgePackagingJobResponse where
  rnf _ = ()
