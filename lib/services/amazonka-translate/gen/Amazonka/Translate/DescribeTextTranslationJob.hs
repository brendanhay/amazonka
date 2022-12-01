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
-- Module      : Amazonka.Translate.DescribeTextTranslationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an asynchronous batch translation
-- job including name, ID, status, source and target languages,
-- input\/output S3 buckets, and so on.
module Amazonka.Translate.DescribeTextTranslationJob
  ( -- * Creating a Request
    DescribeTextTranslationJob (..),
    newDescribeTextTranslationJob,

    -- * Request Lenses
    describeTextTranslationJob_jobId,

    -- * Destructuring the Response
    DescribeTextTranslationJobResponse (..),
    newDescribeTextTranslationJobResponse,

    -- * Response Lenses
    describeTextTranslationJobResponse_textTranslationJobProperties,
    describeTextTranslationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newDescribeTextTranslationJob' smart constructor.
data DescribeTextTranslationJob = DescribeTextTranslationJob'
  { -- | The identifier that Amazon Translate generated for the job. The
    -- StartTextTranslationJob operation returns this identifier in its
    -- response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTextTranslationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeTextTranslationJob_jobId' - The identifier that Amazon Translate generated for the job. The
-- StartTextTranslationJob operation returns this identifier in its
-- response.
newDescribeTextTranslationJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeTextTranslationJob
newDescribeTextTranslationJob pJobId_ =
  DescribeTextTranslationJob' {jobId = pJobId_}

-- | The identifier that Amazon Translate generated for the job. The
-- StartTextTranslationJob operation returns this identifier in its
-- response.
describeTextTranslationJob_jobId :: Lens.Lens' DescribeTextTranslationJob Prelude.Text
describeTextTranslationJob_jobId = Lens.lens (\DescribeTextTranslationJob' {jobId} -> jobId) (\s@DescribeTextTranslationJob' {} a -> s {jobId = a} :: DescribeTextTranslationJob)

instance Core.AWSRequest DescribeTextTranslationJob where
  type
    AWSResponse DescribeTextTranslationJob =
      DescribeTextTranslationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTextTranslationJobResponse'
            Prelude.<$> (x Core..?> "TextTranslationJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTextTranslationJob where
  hashWithSalt _salt DescribeTextTranslationJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeTextTranslationJob where
  rnf DescribeTextTranslationJob' {..} =
    Prelude.rnf jobId

instance Core.ToHeaders DescribeTextTranslationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.DescribeTextTranslationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTextTranslationJob where
  toJSON DescribeTextTranslationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeTextTranslationJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTextTranslationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTextTranslationJobResponse' smart constructor.
data DescribeTextTranslationJobResponse = DescribeTextTranslationJobResponse'
  { -- | An object that contains the properties associated with an asynchronous
    -- batch translation job.
    textTranslationJobProperties :: Prelude.Maybe TextTranslationJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTextTranslationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textTranslationJobProperties', 'describeTextTranslationJobResponse_textTranslationJobProperties' - An object that contains the properties associated with an asynchronous
-- batch translation job.
--
-- 'httpStatus', 'describeTextTranslationJobResponse_httpStatus' - The response's http status code.
newDescribeTextTranslationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTextTranslationJobResponse
newDescribeTextTranslationJobResponse pHttpStatus_ =
  DescribeTextTranslationJobResponse'
    { textTranslationJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with an asynchronous
-- batch translation job.
describeTextTranslationJobResponse_textTranslationJobProperties :: Lens.Lens' DescribeTextTranslationJobResponse (Prelude.Maybe TextTranslationJobProperties)
describeTextTranslationJobResponse_textTranslationJobProperties = Lens.lens (\DescribeTextTranslationJobResponse' {textTranslationJobProperties} -> textTranslationJobProperties) (\s@DescribeTextTranslationJobResponse' {} a -> s {textTranslationJobProperties = a} :: DescribeTextTranslationJobResponse)

-- | The response's http status code.
describeTextTranslationJobResponse_httpStatus :: Lens.Lens' DescribeTextTranslationJobResponse Prelude.Int
describeTextTranslationJobResponse_httpStatus = Lens.lens (\DescribeTextTranslationJobResponse' {httpStatus} -> httpStatus) (\s@DescribeTextTranslationJobResponse' {} a -> s {httpStatus = a} :: DescribeTextTranslationJobResponse)

instance
  Prelude.NFData
    DescribeTextTranslationJobResponse
  where
  rnf DescribeTextTranslationJobResponse' {..} =
    Prelude.rnf textTranslationJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
