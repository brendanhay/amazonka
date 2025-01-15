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
-- Module      : Amazonka.Personalize.DescribeBatchInferenceJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties of a batch inference job including name, Amazon
-- Resource Name (ARN), status, input and output configurations, and the
-- ARN of the solution version used to generate the recommendations.
module Amazonka.Personalize.DescribeBatchInferenceJob
  ( -- * Creating a Request
    DescribeBatchInferenceJob (..),
    newDescribeBatchInferenceJob,

    -- * Request Lenses
    describeBatchInferenceJob_batchInferenceJobArn,

    -- * Destructuring the Response
    DescribeBatchInferenceJobResponse (..),
    newDescribeBatchInferenceJobResponse,

    -- * Response Lenses
    describeBatchInferenceJobResponse_batchInferenceJob,
    describeBatchInferenceJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBatchInferenceJob' smart constructor.
data DescribeBatchInferenceJob = DescribeBatchInferenceJob'
  { -- | The ARN of the batch inference job to describe.
    batchInferenceJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchInferenceJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchInferenceJobArn', 'describeBatchInferenceJob_batchInferenceJobArn' - The ARN of the batch inference job to describe.
newDescribeBatchInferenceJob ::
  -- | 'batchInferenceJobArn'
  Prelude.Text ->
  DescribeBatchInferenceJob
newDescribeBatchInferenceJob pBatchInferenceJobArn_ =
  DescribeBatchInferenceJob'
    { batchInferenceJobArn =
        pBatchInferenceJobArn_
    }

-- | The ARN of the batch inference job to describe.
describeBatchInferenceJob_batchInferenceJobArn :: Lens.Lens' DescribeBatchInferenceJob Prelude.Text
describeBatchInferenceJob_batchInferenceJobArn = Lens.lens (\DescribeBatchInferenceJob' {batchInferenceJobArn} -> batchInferenceJobArn) (\s@DescribeBatchInferenceJob' {} a -> s {batchInferenceJobArn = a} :: DescribeBatchInferenceJob)

instance Core.AWSRequest DescribeBatchInferenceJob where
  type
    AWSResponse DescribeBatchInferenceJob =
      DescribeBatchInferenceJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBatchInferenceJobResponse'
            Prelude.<$> (x Data..?> "batchInferenceJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBatchInferenceJob where
  hashWithSalt _salt DescribeBatchInferenceJob' {..} =
    _salt `Prelude.hashWithSalt` batchInferenceJobArn

instance Prelude.NFData DescribeBatchInferenceJob where
  rnf DescribeBatchInferenceJob' {..} =
    Prelude.rnf batchInferenceJobArn

instance Data.ToHeaders DescribeBatchInferenceJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeBatchInferenceJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBatchInferenceJob where
  toJSON DescribeBatchInferenceJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "batchInferenceJobArn"
                  Data..= batchInferenceJobArn
              )
          ]
      )

instance Data.ToPath DescribeBatchInferenceJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBatchInferenceJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBatchInferenceJobResponse' smart constructor.
data DescribeBatchInferenceJobResponse = DescribeBatchInferenceJobResponse'
  { -- | Information on the specified batch inference job.
    batchInferenceJob :: Prelude.Maybe BatchInferenceJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBatchInferenceJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchInferenceJob', 'describeBatchInferenceJobResponse_batchInferenceJob' - Information on the specified batch inference job.
--
-- 'httpStatus', 'describeBatchInferenceJobResponse_httpStatus' - The response's http status code.
newDescribeBatchInferenceJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBatchInferenceJobResponse
newDescribeBatchInferenceJobResponse pHttpStatus_ =
  DescribeBatchInferenceJobResponse'
    { batchInferenceJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information on the specified batch inference job.
describeBatchInferenceJobResponse_batchInferenceJob :: Lens.Lens' DescribeBatchInferenceJobResponse (Prelude.Maybe BatchInferenceJob)
describeBatchInferenceJobResponse_batchInferenceJob = Lens.lens (\DescribeBatchInferenceJobResponse' {batchInferenceJob} -> batchInferenceJob) (\s@DescribeBatchInferenceJobResponse' {} a -> s {batchInferenceJob = a} :: DescribeBatchInferenceJobResponse)

-- | The response's http status code.
describeBatchInferenceJobResponse_httpStatus :: Lens.Lens' DescribeBatchInferenceJobResponse Prelude.Int
describeBatchInferenceJobResponse_httpStatus = Lens.lens (\DescribeBatchInferenceJobResponse' {httpStatus} -> httpStatus) (\s@DescribeBatchInferenceJobResponse' {} a -> s {httpStatus = a} :: DescribeBatchInferenceJobResponse)

instance
  Prelude.NFData
    DescribeBatchInferenceJobResponse
  where
  rnf DescribeBatchInferenceJobResponse' {..} =
    Prelude.rnf batchInferenceJob `Prelude.seq`
      Prelude.rnf httpStatus
