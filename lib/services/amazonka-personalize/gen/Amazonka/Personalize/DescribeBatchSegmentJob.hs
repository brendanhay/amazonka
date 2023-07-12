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
-- Module      : Amazonka.Personalize.DescribeBatchSegmentJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties of a batch segment job including name, Amazon
-- Resource Name (ARN), status, input and output configurations, and the
-- ARN of the solution version used to generate segments.
module Amazonka.Personalize.DescribeBatchSegmentJob
  ( -- * Creating a Request
    DescribeBatchSegmentJob (..),
    newDescribeBatchSegmentJob,

    -- * Request Lenses
    describeBatchSegmentJob_batchSegmentJobArn,

    -- * Destructuring the Response
    DescribeBatchSegmentJobResponse (..),
    newDescribeBatchSegmentJobResponse,

    -- * Response Lenses
    describeBatchSegmentJobResponse_batchSegmentJob,
    describeBatchSegmentJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBatchSegmentJob' smart constructor.
data DescribeBatchSegmentJob = DescribeBatchSegmentJob'
  { -- | The ARN of the batch segment job to describe.
    batchSegmentJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchSegmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSegmentJobArn', 'describeBatchSegmentJob_batchSegmentJobArn' - The ARN of the batch segment job to describe.
newDescribeBatchSegmentJob ::
  -- | 'batchSegmentJobArn'
  Prelude.Text ->
  DescribeBatchSegmentJob
newDescribeBatchSegmentJob pBatchSegmentJobArn_ =
  DescribeBatchSegmentJob'
    { batchSegmentJobArn =
        pBatchSegmentJobArn_
    }

-- | The ARN of the batch segment job to describe.
describeBatchSegmentJob_batchSegmentJobArn :: Lens.Lens' DescribeBatchSegmentJob Prelude.Text
describeBatchSegmentJob_batchSegmentJobArn = Lens.lens (\DescribeBatchSegmentJob' {batchSegmentJobArn} -> batchSegmentJobArn) (\s@DescribeBatchSegmentJob' {} a -> s {batchSegmentJobArn = a} :: DescribeBatchSegmentJob)

instance Core.AWSRequest DescribeBatchSegmentJob where
  type
    AWSResponse DescribeBatchSegmentJob =
      DescribeBatchSegmentJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBatchSegmentJobResponse'
            Prelude.<$> (x Data..?> "batchSegmentJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBatchSegmentJob where
  hashWithSalt _salt DescribeBatchSegmentJob' {..} =
    _salt `Prelude.hashWithSalt` batchSegmentJobArn

instance Prelude.NFData DescribeBatchSegmentJob where
  rnf DescribeBatchSegmentJob' {..} =
    Prelude.rnf batchSegmentJobArn

instance Data.ToHeaders DescribeBatchSegmentJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeBatchSegmentJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBatchSegmentJob where
  toJSON DescribeBatchSegmentJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("batchSegmentJobArn" Data..= batchSegmentJobArn)
          ]
      )

instance Data.ToPath DescribeBatchSegmentJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBatchSegmentJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBatchSegmentJobResponse' smart constructor.
data DescribeBatchSegmentJobResponse = DescribeBatchSegmentJobResponse'
  { -- | Information on the specified batch segment job.
    batchSegmentJob :: Prelude.Maybe BatchSegmentJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchSegmentJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSegmentJob', 'describeBatchSegmentJobResponse_batchSegmentJob' - Information on the specified batch segment job.
--
-- 'httpStatus', 'describeBatchSegmentJobResponse_httpStatus' - The response's http status code.
newDescribeBatchSegmentJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBatchSegmentJobResponse
newDescribeBatchSegmentJobResponse pHttpStatus_ =
  DescribeBatchSegmentJobResponse'
    { batchSegmentJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the specified batch segment job.
describeBatchSegmentJobResponse_batchSegmentJob :: Lens.Lens' DescribeBatchSegmentJobResponse (Prelude.Maybe BatchSegmentJob)
describeBatchSegmentJobResponse_batchSegmentJob = Lens.lens (\DescribeBatchSegmentJobResponse' {batchSegmentJob} -> batchSegmentJob) (\s@DescribeBatchSegmentJobResponse' {} a -> s {batchSegmentJob = a} :: DescribeBatchSegmentJobResponse)

-- | The response's http status code.
describeBatchSegmentJobResponse_httpStatus :: Lens.Lens' DescribeBatchSegmentJobResponse Prelude.Int
describeBatchSegmentJobResponse_httpStatus = Lens.lens (\DescribeBatchSegmentJobResponse' {httpStatus} -> httpStatus) (\s@DescribeBatchSegmentJobResponse' {} a -> s {httpStatus = a} :: DescribeBatchSegmentJobResponse)

instance
  Prelude.NFData
    DescribeBatchSegmentJobResponse
  where
  rnf DescribeBatchSegmentJobResponse' {..} =
    Prelude.rnf batchSegmentJob
      `Prelude.seq` Prelude.rnf httpStatus
