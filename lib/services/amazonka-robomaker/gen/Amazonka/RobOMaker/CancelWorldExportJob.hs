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
-- Module      : Amazonka.RobOMaker.CancelWorldExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export job.
module Amazonka.RobOMaker.CancelWorldExportJob
  ( -- * Creating a Request
    CancelWorldExportJob (..),
    newCancelWorldExportJob,

    -- * Request Lenses
    cancelWorldExportJob_job,

    -- * Destructuring the Response
    CancelWorldExportJobResponse (..),
    newCancelWorldExportJobResponse,

    -- * Response Lenses
    cancelWorldExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCancelWorldExportJob' smart constructor.
data CancelWorldExportJob = CancelWorldExportJob'
  { -- | The Amazon Resource Name (arn) of the world export job to cancel.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelWorldExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'cancelWorldExportJob_job' - The Amazon Resource Name (arn) of the world export job to cancel.
newCancelWorldExportJob ::
  -- | 'job'
  Prelude.Text ->
  CancelWorldExportJob
newCancelWorldExportJob pJob_ =
  CancelWorldExportJob' {job = pJob_}

-- | The Amazon Resource Name (arn) of the world export job to cancel.
cancelWorldExportJob_job :: Lens.Lens' CancelWorldExportJob Prelude.Text
cancelWorldExportJob_job = Lens.lens (\CancelWorldExportJob' {job} -> job) (\s@CancelWorldExportJob' {} a -> s {job = a} :: CancelWorldExportJob)

instance Core.AWSRequest CancelWorldExportJob where
  type
    AWSResponse CancelWorldExportJob =
      CancelWorldExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelWorldExportJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelWorldExportJob where
  hashWithSalt _salt CancelWorldExportJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData CancelWorldExportJob where
  rnf CancelWorldExportJob' {..} = Prelude.rnf job

instance Data.ToHeaders CancelWorldExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelWorldExportJob where
  toJSON CancelWorldExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Data..= job)]
      )

instance Data.ToPath CancelWorldExportJob where
  toPath = Prelude.const "/cancelWorldExportJob"

instance Data.ToQuery CancelWorldExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelWorldExportJobResponse' smart constructor.
data CancelWorldExportJobResponse = CancelWorldExportJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelWorldExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelWorldExportJobResponse_httpStatus' - The response's http status code.
newCancelWorldExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelWorldExportJobResponse
newCancelWorldExportJobResponse pHttpStatus_ =
  CancelWorldExportJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelWorldExportJobResponse_httpStatus :: Lens.Lens' CancelWorldExportJobResponse Prelude.Int
cancelWorldExportJobResponse_httpStatus = Lens.lens (\CancelWorldExportJobResponse' {httpStatus} -> httpStatus) (\s@CancelWorldExportJobResponse' {} a -> s {httpStatus = a} :: CancelWorldExportJobResponse)

instance Prelude.NFData CancelWorldExportJobResponse where
  rnf CancelWorldExportJobResponse' {..} =
    Prelude.rnf httpStatus
