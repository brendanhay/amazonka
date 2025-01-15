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
-- Module      : Amazonka.Backup.DescribeCopyJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata associated with creating a copy of a resource.
module Amazonka.Backup.DescribeCopyJob
  ( -- * Creating a Request
    DescribeCopyJob (..),
    newDescribeCopyJob,

    -- * Request Lenses
    describeCopyJob_copyJobId,

    -- * Destructuring the Response
    DescribeCopyJobResponse (..),
    newDescribeCopyJobResponse,

    -- * Response Lenses
    describeCopyJobResponse_copyJob,
    describeCopyJobResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeCopyJob' smart constructor.
data DescribeCopyJob = DescribeCopyJob'
  { -- | Uniquely identifies a copy job.
    copyJobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCopyJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyJobId', 'describeCopyJob_copyJobId' - Uniquely identifies a copy job.
newDescribeCopyJob ::
  -- | 'copyJobId'
  Prelude.Text ->
  DescribeCopyJob
newDescribeCopyJob pCopyJobId_ =
  DescribeCopyJob' {copyJobId = pCopyJobId_}

-- | Uniquely identifies a copy job.
describeCopyJob_copyJobId :: Lens.Lens' DescribeCopyJob Prelude.Text
describeCopyJob_copyJobId = Lens.lens (\DescribeCopyJob' {copyJobId} -> copyJobId) (\s@DescribeCopyJob' {} a -> s {copyJobId = a} :: DescribeCopyJob)

instance Core.AWSRequest DescribeCopyJob where
  type
    AWSResponse DescribeCopyJob =
      DescribeCopyJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCopyJobResponse'
            Prelude.<$> (x Data..?> "CopyJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCopyJob where
  hashWithSalt _salt DescribeCopyJob' {..} =
    _salt `Prelude.hashWithSalt` copyJobId

instance Prelude.NFData DescribeCopyJob where
  rnf DescribeCopyJob' {..} = Prelude.rnf copyJobId

instance Data.ToHeaders DescribeCopyJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeCopyJob where
  toPath DescribeCopyJob' {..} =
    Prelude.mconcat
      ["/copy-jobs/", Data.toBS copyJobId]

instance Data.ToQuery DescribeCopyJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCopyJobResponse' smart constructor.
data DescribeCopyJobResponse = DescribeCopyJobResponse'
  { -- | Contains detailed information about a copy job.
    copyJob :: Prelude.Maybe CopyJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCopyJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyJob', 'describeCopyJobResponse_copyJob' - Contains detailed information about a copy job.
--
-- 'httpStatus', 'describeCopyJobResponse_httpStatus' - The response's http status code.
newDescribeCopyJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCopyJobResponse
newDescribeCopyJobResponse pHttpStatus_ =
  DescribeCopyJobResponse'
    { copyJob = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains detailed information about a copy job.
describeCopyJobResponse_copyJob :: Lens.Lens' DescribeCopyJobResponse (Prelude.Maybe CopyJob)
describeCopyJobResponse_copyJob = Lens.lens (\DescribeCopyJobResponse' {copyJob} -> copyJob) (\s@DescribeCopyJobResponse' {} a -> s {copyJob = a} :: DescribeCopyJobResponse)

-- | The response's http status code.
describeCopyJobResponse_httpStatus :: Lens.Lens' DescribeCopyJobResponse Prelude.Int
describeCopyJobResponse_httpStatus = Lens.lens (\DescribeCopyJobResponse' {httpStatus} -> httpStatus) (\s@DescribeCopyJobResponse' {} a -> s {httpStatus = a} :: DescribeCopyJobResponse)

instance Prelude.NFData DescribeCopyJobResponse where
  rnf DescribeCopyJobResponse' {..} =
    Prelude.rnf copyJob `Prelude.seq`
      Prelude.rnf httpStatus
