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
-- Module      : Amazonka.SageMaker.StopCompilationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a model compilation job.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal.
-- This gracefully shuts the job down. If the job hasn\'t stopped, it sends
-- the SIGKILL signal.
--
-- When it receives a @StopCompilationJob@ request, Amazon SageMaker
-- changes the @CompilationJobStatus@ of the job to @Stopping@. After
-- Amazon SageMaker stops the job, it sets the @CompilationJobStatus@ to
-- @Stopped@.
module Amazonka.SageMaker.StopCompilationJob
  ( -- * Creating a Request
    StopCompilationJob (..),
    newStopCompilationJob,

    -- * Request Lenses
    stopCompilationJob_compilationJobName,

    -- * Destructuring the Response
    StopCompilationJobResponse (..),
    newStopCompilationJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopCompilationJob' smart constructor.
data StopCompilationJob = StopCompilationJob'
  { -- | The name of the model compilation job to stop.
    compilationJobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCompilationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationJobName', 'stopCompilationJob_compilationJobName' - The name of the model compilation job to stop.
newStopCompilationJob ::
  -- | 'compilationJobName'
  Prelude.Text ->
  StopCompilationJob
newStopCompilationJob pCompilationJobName_ =
  StopCompilationJob'
    { compilationJobName =
        pCompilationJobName_
    }

-- | The name of the model compilation job to stop.
stopCompilationJob_compilationJobName :: Lens.Lens' StopCompilationJob Prelude.Text
stopCompilationJob_compilationJobName = Lens.lens (\StopCompilationJob' {compilationJobName} -> compilationJobName) (\s@StopCompilationJob' {} a -> s {compilationJobName = a} :: StopCompilationJob)

instance Core.AWSRequest StopCompilationJob where
  type
    AWSResponse StopCompilationJob =
      StopCompilationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopCompilationJobResponse'

instance Prelude.Hashable StopCompilationJob where
  hashWithSalt _salt StopCompilationJob' {..} =
    _salt `Prelude.hashWithSalt` compilationJobName

instance Prelude.NFData StopCompilationJob where
  rnf StopCompilationJob' {..} =
    Prelude.rnf compilationJobName

instance Data.ToHeaders StopCompilationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StopCompilationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopCompilationJob where
  toJSON StopCompilationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CompilationJobName" Data..= compilationJobName)
          ]
      )

instance Data.ToPath StopCompilationJob where
  toPath = Prelude.const "/"

instance Data.ToQuery StopCompilationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopCompilationJobResponse' smart constructor.
data StopCompilationJobResponse = StopCompilationJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopCompilationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopCompilationJobResponse ::
  StopCompilationJobResponse
newStopCompilationJobResponse =
  StopCompilationJobResponse'

instance Prelude.NFData StopCompilationJobResponse where
  rnf _ = ()
