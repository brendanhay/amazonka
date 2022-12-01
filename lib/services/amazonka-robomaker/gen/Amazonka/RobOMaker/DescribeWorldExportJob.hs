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
-- Module      : Amazonka.RobOMaker.DescribeWorldExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a world export job.
module Amazonka.RobOMaker.DescribeWorldExportJob
  ( -- * Creating a Request
    DescribeWorldExportJob (..),
    newDescribeWorldExportJob,

    -- * Request Lenses
    describeWorldExportJob_job,

    -- * Destructuring the Response
    DescribeWorldExportJobResponse (..),
    newDescribeWorldExportJobResponse,

    -- * Response Lenses
    describeWorldExportJobResponse_tags,
    describeWorldExportJobResponse_iamRole,
    describeWorldExportJobResponse_failureCode,
    describeWorldExportJobResponse_clientRequestToken,
    describeWorldExportJobResponse_arn,
    describeWorldExportJobResponse_outputLocation,
    describeWorldExportJobResponse_status,
    describeWorldExportJobResponse_worlds,
    describeWorldExportJobResponse_createdAt,
    describeWorldExportJobResponse_failureReason,
    describeWorldExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeWorldExportJob' smart constructor.
data DescribeWorldExportJob = DescribeWorldExportJob'
  { -- | The Amazon Resource Name (arn) of the world export job to describe.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorldExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'describeWorldExportJob_job' - The Amazon Resource Name (arn) of the world export job to describe.
newDescribeWorldExportJob ::
  -- | 'job'
  Prelude.Text ->
  DescribeWorldExportJob
newDescribeWorldExportJob pJob_ =
  DescribeWorldExportJob' {job = pJob_}

-- | The Amazon Resource Name (arn) of the world export job to describe.
describeWorldExportJob_job :: Lens.Lens' DescribeWorldExportJob Prelude.Text
describeWorldExportJob_job = Lens.lens (\DescribeWorldExportJob' {job} -> job) (\s@DescribeWorldExportJob' {} a -> s {job = a} :: DescribeWorldExportJob)

instance Core.AWSRequest DescribeWorldExportJob where
  type
    AWSResponse DescribeWorldExportJob =
      DescribeWorldExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorldExportJobResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "iamRole")
            Prelude.<*> (x Core..?> "failureCode")
            Prelude.<*> (x Core..?> "clientRequestToken")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "outputLocation")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "worlds")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "failureReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorldExportJob where
  hashWithSalt _salt DescribeWorldExportJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData DescribeWorldExportJob where
  rnf DescribeWorldExportJob' {..} = Prelude.rnf job

instance Core.ToHeaders DescribeWorldExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorldExportJob where
  toJSON DescribeWorldExportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Core..= job)]
      )

instance Core.ToPath DescribeWorldExportJob where
  toPath = Prelude.const "/describeWorldExportJob"

instance Core.ToQuery DescribeWorldExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorldExportJobResponse' smart constructor.
data DescribeWorldExportJobResponse = DescribeWorldExportJobResponse'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- world export job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The IAM role that the world export process uses to access the Amazon S3
    -- bucket and put the export.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | The failure code of the world export job if it failed:
    --
    -- [InternalServiceError]
    --     Internal service error.
    --
    -- [LimitExceeded]
    --     The requested resource exceeds the maximum number allowed, or the
    --     number of concurrent stream requests exceeds the maximum number
    --     allowed.
    --
    -- [ResourceNotFound]
    --     The specified resource could not be found.
    --
    -- [RequestThrottled]
    --     The request was throttled.
    --
    -- [InvalidInput]
    --     An input parameter in the request is not valid.
    failureCode :: Prelude.Maybe WorldExportJobErrorCode,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the world export job.
    arn :: Prelude.Maybe Prelude.Text,
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The status of the world export job.
    --
    -- [Pending]
    --     The world export job request is pending.
    --
    -- [Running]
    --     The world export job is running.
    --
    -- [Completed]
    --     The world export job completed.
    --
    -- [Failed]
    --     The world export job failed. See @failureCode@ and @failureReason@
    --     for more information.
    --
    -- [Canceled]
    --     The world export job was cancelled.
    --
    -- [Canceling]
    --     The world export job is being cancelled.
    status :: Prelude.Maybe WorldExportJobStatus,
    -- | A list of Amazon Resource Names (arns) that correspond to worlds to be
    -- exported.
    worlds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The time, in milliseconds since the epoch, when the world export job was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The reason why the world export job failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorldExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeWorldExportJobResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- world export job.
--
-- 'iamRole', 'describeWorldExportJobResponse_iamRole' - The IAM role that the world export process uses to access the Amazon S3
-- bucket and put the export.
--
-- 'failureCode', 'describeWorldExportJobResponse_failureCode' - The failure code of the world export job if it failed:
--
-- [InternalServiceError]
--     Internal service error.
--
-- [LimitExceeded]
--     The requested resource exceeds the maximum number allowed, or the
--     number of concurrent stream requests exceeds the maximum number
--     allowed.
--
-- [ResourceNotFound]
--     The specified resource could not be found.
--
-- [RequestThrottled]
--     The request was throttled.
--
-- [InvalidInput]
--     An input parameter in the request is not valid.
--
-- 'clientRequestToken', 'describeWorldExportJobResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'arn', 'describeWorldExportJobResponse_arn' - The Amazon Resource Name (ARN) of the world export job.
--
-- 'outputLocation', 'describeWorldExportJobResponse_outputLocation' - Undocumented member.
--
-- 'status', 'describeWorldExportJobResponse_status' - The status of the world export job.
--
-- [Pending]
--     The world export job request is pending.
--
-- [Running]
--     The world export job is running.
--
-- [Completed]
--     The world export job completed.
--
-- [Failed]
--     The world export job failed. See @failureCode@ and @failureReason@
--     for more information.
--
-- [Canceled]
--     The world export job was cancelled.
--
-- [Canceling]
--     The world export job is being cancelled.
--
-- 'worlds', 'describeWorldExportJobResponse_worlds' - A list of Amazon Resource Names (arns) that correspond to worlds to be
-- exported.
--
-- 'createdAt', 'describeWorldExportJobResponse_createdAt' - The time, in milliseconds since the epoch, when the world export job was
-- created.
--
-- 'failureReason', 'describeWorldExportJobResponse_failureReason' - The reason why the world export job failed.
--
-- 'httpStatus', 'describeWorldExportJobResponse_httpStatus' - The response's http status code.
newDescribeWorldExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorldExportJobResponse
newDescribeWorldExportJobResponse pHttpStatus_ =
  DescribeWorldExportJobResponse'
    { tags =
        Prelude.Nothing,
      iamRole = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      status = Prelude.Nothing,
      worlds = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map that contains tag keys and tag values that are attached to the
-- world export job.
describeWorldExportJobResponse_tags :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeWorldExportJobResponse_tags = Lens.lens (\DescribeWorldExportJobResponse' {tags} -> tags) (\s@DescribeWorldExportJobResponse' {} a -> s {tags = a} :: DescribeWorldExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role that the world export process uses to access the Amazon S3
-- bucket and put the export.
describeWorldExportJobResponse_iamRole :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe Prelude.Text)
describeWorldExportJobResponse_iamRole = Lens.lens (\DescribeWorldExportJobResponse' {iamRole} -> iamRole) (\s@DescribeWorldExportJobResponse' {} a -> s {iamRole = a} :: DescribeWorldExportJobResponse)

-- | The failure code of the world export job if it failed:
--
-- [InternalServiceError]
--     Internal service error.
--
-- [LimitExceeded]
--     The requested resource exceeds the maximum number allowed, or the
--     number of concurrent stream requests exceeds the maximum number
--     allowed.
--
-- [ResourceNotFound]
--     The specified resource could not be found.
--
-- [RequestThrottled]
--     The request was throttled.
--
-- [InvalidInput]
--     An input parameter in the request is not valid.
describeWorldExportJobResponse_failureCode :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe WorldExportJobErrorCode)
describeWorldExportJobResponse_failureCode = Lens.lens (\DescribeWorldExportJobResponse' {failureCode} -> failureCode) (\s@DescribeWorldExportJobResponse' {} a -> s {failureCode = a} :: DescribeWorldExportJobResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
describeWorldExportJobResponse_clientRequestToken :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe Prelude.Text)
describeWorldExportJobResponse_clientRequestToken = Lens.lens (\DescribeWorldExportJobResponse' {clientRequestToken} -> clientRequestToken) (\s@DescribeWorldExportJobResponse' {} a -> s {clientRequestToken = a} :: DescribeWorldExportJobResponse)

-- | The Amazon Resource Name (ARN) of the world export job.
describeWorldExportJobResponse_arn :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe Prelude.Text)
describeWorldExportJobResponse_arn = Lens.lens (\DescribeWorldExportJobResponse' {arn} -> arn) (\s@DescribeWorldExportJobResponse' {} a -> s {arn = a} :: DescribeWorldExportJobResponse)

-- | Undocumented member.
describeWorldExportJobResponse_outputLocation :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe OutputLocation)
describeWorldExportJobResponse_outputLocation = Lens.lens (\DescribeWorldExportJobResponse' {outputLocation} -> outputLocation) (\s@DescribeWorldExportJobResponse' {} a -> s {outputLocation = a} :: DescribeWorldExportJobResponse)

-- | The status of the world export job.
--
-- [Pending]
--     The world export job request is pending.
--
-- [Running]
--     The world export job is running.
--
-- [Completed]
--     The world export job completed.
--
-- [Failed]
--     The world export job failed. See @failureCode@ and @failureReason@
--     for more information.
--
-- [Canceled]
--     The world export job was cancelled.
--
-- [Canceling]
--     The world export job is being cancelled.
describeWorldExportJobResponse_status :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe WorldExportJobStatus)
describeWorldExportJobResponse_status = Lens.lens (\DescribeWorldExportJobResponse' {status} -> status) (\s@DescribeWorldExportJobResponse' {} a -> s {status = a} :: DescribeWorldExportJobResponse)

-- | A list of Amazon Resource Names (arns) that correspond to worlds to be
-- exported.
describeWorldExportJobResponse_worlds :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorldExportJobResponse_worlds = Lens.lens (\DescribeWorldExportJobResponse' {worlds} -> worlds) (\s@DescribeWorldExportJobResponse' {} a -> s {worlds = a} :: DescribeWorldExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time, in milliseconds since the epoch, when the world export job was
-- created.
describeWorldExportJobResponse_createdAt :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe Prelude.UTCTime)
describeWorldExportJobResponse_createdAt = Lens.lens (\DescribeWorldExportJobResponse' {createdAt} -> createdAt) (\s@DescribeWorldExportJobResponse' {} a -> s {createdAt = a} :: DescribeWorldExportJobResponse) Prelude.. Lens.mapping Core._Time

-- | The reason why the world export job failed.
describeWorldExportJobResponse_failureReason :: Lens.Lens' DescribeWorldExportJobResponse (Prelude.Maybe Prelude.Text)
describeWorldExportJobResponse_failureReason = Lens.lens (\DescribeWorldExportJobResponse' {failureReason} -> failureReason) (\s@DescribeWorldExportJobResponse' {} a -> s {failureReason = a} :: DescribeWorldExportJobResponse)

-- | The response's http status code.
describeWorldExportJobResponse_httpStatus :: Lens.Lens' DescribeWorldExportJobResponse Prelude.Int
describeWorldExportJobResponse_httpStatus = Lens.lens (\DescribeWorldExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeWorldExportJobResponse' {} a -> s {httpStatus = a} :: DescribeWorldExportJobResponse)

instance
  Prelude.NFData
    DescribeWorldExportJobResponse
  where
  rnf DescribeWorldExportJobResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf worlds
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf httpStatus
