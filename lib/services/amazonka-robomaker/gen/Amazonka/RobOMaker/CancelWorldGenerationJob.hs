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
-- Module      : Amazonka.RobOMaker.CancelWorldGenerationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified world generator job.
module Amazonka.RobOMaker.CancelWorldGenerationJob
  ( -- * Creating a Request
    CancelWorldGenerationJob (..),
    newCancelWorldGenerationJob,

    -- * Request Lenses
    cancelWorldGenerationJob_job,

    -- * Destructuring the Response
    CancelWorldGenerationJobResponse (..),
    newCancelWorldGenerationJobResponse,

    -- * Response Lenses
    cancelWorldGenerationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCancelWorldGenerationJob' smart constructor.
data CancelWorldGenerationJob = CancelWorldGenerationJob'
  { -- | The Amazon Resource Name (arn) of the world generator job to cancel.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelWorldGenerationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'cancelWorldGenerationJob_job' - The Amazon Resource Name (arn) of the world generator job to cancel.
newCancelWorldGenerationJob ::
  -- | 'job'
  Prelude.Text ->
  CancelWorldGenerationJob
newCancelWorldGenerationJob pJob_ =
  CancelWorldGenerationJob' {job = pJob_}

-- | The Amazon Resource Name (arn) of the world generator job to cancel.
cancelWorldGenerationJob_job :: Lens.Lens' CancelWorldGenerationJob Prelude.Text
cancelWorldGenerationJob_job = Lens.lens (\CancelWorldGenerationJob' {job} -> job) (\s@CancelWorldGenerationJob' {} a -> s {job = a} :: CancelWorldGenerationJob)

instance Core.AWSRequest CancelWorldGenerationJob where
  type
    AWSResponse CancelWorldGenerationJob =
      CancelWorldGenerationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelWorldGenerationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelWorldGenerationJob where
  hashWithSalt _salt CancelWorldGenerationJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData CancelWorldGenerationJob where
  rnf CancelWorldGenerationJob' {..} = Prelude.rnf job

instance Data.ToHeaders CancelWorldGenerationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelWorldGenerationJob where
  toJSON CancelWorldGenerationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Data..= job)]
      )

instance Data.ToPath CancelWorldGenerationJob where
  toPath = Prelude.const "/cancelWorldGenerationJob"

instance Data.ToQuery CancelWorldGenerationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelWorldGenerationJobResponse' smart constructor.
data CancelWorldGenerationJobResponse = CancelWorldGenerationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelWorldGenerationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelWorldGenerationJobResponse_httpStatus' - The response's http status code.
newCancelWorldGenerationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelWorldGenerationJobResponse
newCancelWorldGenerationJobResponse pHttpStatus_ =
  CancelWorldGenerationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelWorldGenerationJobResponse_httpStatus :: Lens.Lens' CancelWorldGenerationJobResponse Prelude.Int
cancelWorldGenerationJobResponse_httpStatus = Lens.lens (\CancelWorldGenerationJobResponse' {httpStatus} -> httpStatus) (\s@CancelWorldGenerationJobResponse' {} a -> s {httpStatus = a} :: CancelWorldGenerationJobResponse)

instance
  Prelude.NFData
    CancelWorldGenerationJobResponse
  where
  rnf CancelWorldGenerationJobResponse' {..} =
    Prelude.rnf httpStatus
