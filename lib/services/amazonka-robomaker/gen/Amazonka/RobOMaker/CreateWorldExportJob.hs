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
-- Module      : Amazonka.RobOMaker.CreateWorldExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a world export job.
module Amazonka.RobOMaker.CreateWorldExportJob
  ( -- * Creating a Request
    CreateWorldExportJob (..),
    newCreateWorldExportJob,

    -- * Request Lenses
    createWorldExportJob_clientRequestToken,
    createWorldExportJob_tags,
    createWorldExportJob_worlds,
    createWorldExportJob_outputLocation,
    createWorldExportJob_iamRole,

    -- * Destructuring the Response
    CreateWorldExportJobResponse (..),
    newCreateWorldExportJobResponse,

    -- * Response Lenses
    createWorldExportJobResponse_arn,
    createWorldExportJobResponse_clientRequestToken,
    createWorldExportJobResponse_createdAt,
    createWorldExportJobResponse_failureCode,
    createWorldExportJobResponse_iamRole,
    createWorldExportJobResponse_outputLocation,
    createWorldExportJobResponse_status,
    createWorldExportJobResponse_tags,
    createWorldExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateWorldExportJob' smart constructor.
data CreateWorldExportJob = CreateWorldExportJob'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A map that contains tag keys and tag values that are attached to the
    -- world export job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of Amazon Resource Names (arns) that correspond to worlds to
    -- export.
    worlds :: Prelude.NonEmpty Prelude.Text,
    outputLocation :: OutputLocation,
    -- | The IAM role that the world export process uses to access the Amazon S3
    -- bucket and put the export.
    iamRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorldExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createWorldExportJob_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'tags', 'createWorldExportJob_tags' - A map that contains tag keys and tag values that are attached to the
-- world export job.
--
-- 'worlds', 'createWorldExportJob_worlds' - A list of Amazon Resource Names (arns) that correspond to worlds to
-- export.
--
-- 'outputLocation', 'createWorldExportJob_outputLocation' - Undocumented member.
--
-- 'iamRole', 'createWorldExportJob_iamRole' - The IAM role that the world export process uses to access the Amazon S3
-- bucket and put the export.
newCreateWorldExportJob ::
  -- | 'worlds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'outputLocation'
  OutputLocation ->
  -- | 'iamRole'
  Prelude.Text ->
  CreateWorldExportJob
newCreateWorldExportJob
  pWorlds_
  pOutputLocation_
  pIamRole_ =
    CreateWorldExportJob'
      { clientRequestToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        worlds = Lens.coerced Lens.# pWorlds_,
        outputLocation = pOutputLocation_,
        iamRole = pIamRole_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldExportJob_clientRequestToken :: Lens.Lens' CreateWorldExportJob (Prelude.Maybe Prelude.Text)
createWorldExportJob_clientRequestToken = Lens.lens (\CreateWorldExportJob' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldExportJob' {} a -> s {clientRequestToken = a} :: CreateWorldExportJob)

-- | A map that contains tag keys and tag values that are attached to the
-- world export job.
createWorldExportJob_tags :: Lens.Lens' CreateWorldExportJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldExportJob_tags = Lens.lens (\CreateWorldExportJob' {tags} -> tags) (\s@CreateWorldExportJob' {} a -> s {tags = a} :: CreateWorldExportJob) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Resource Names (arns) that correspond to worlds to
-- export.
createWorldExportJob_worlds :: Lens.Lens' CreateWorldExportJob (Prelude.NonEmpty Prelude.Text)
createWorldExportJob_worlds = Lens.lens (\CreateWorldExportJob' {worlds} -> worlds) (\s@CreateWorldExportJob' {} a -> s {worlds = a} :: CreateWorldExportJob) Prelude.. Lens.coerced

-- | Undocumented member.
createWorldExportJob_outputLocation :: Lens.Lens' CreateWorldExportJob OutputLocation
createWorldExportJob_outputLocation = Lens.lens (\CreateWorldExportJob' {outputLocation} -> outputLocation) (\s@CreateWorldExportJob' {} a -> s {outputLocation = a} :: CreateWorldExportJob)

-- | The IAM role that the world export process uses to access the Amazon S3
-- bucket and put the export.
createWorldExportJob_iamRole :: Lens.Lens' CreateWorldExportJob Prelude.Text
createWorldExportJob_iamRole = Lens.lens (\CreateWorldExportJob' {iamRole} -> iamRole) (\s@CreateWorldExportJob' {} a -> s {iamRole = a} :: CreateWorldExportJob)

instance Core.AWSRequest CreateWorldExportJob where
  type
    AWSResponse CreateWorldExportJob =
      CreateWorldExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorldExportJobResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "clientRequestToken")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "iamRole")
            Prelude.<*> (x Data..?> "outputLocation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorldExportJob where
  hashWithSalt _salt CreateWorldExportJob' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` worlds
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` iamRole

instance Prelude.NFData CreateWorldExportJob where
  rnf CreateWorldExportJob' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf worlds
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf iamRole

instance Data.ToHeaders CreateWorldExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorldExportJob where
  toJSON CreateWorldExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("worlds" Data..= worlds),
            Prelude.Just
              ("outputLocation" Data..= outputLocation),
            Prelude.Just ("iamRole" Data..= iamRole)
          ]
      )

instance Data.ToPath CreateWorldExportJob where
  toPath = Prelude.const "/createWorldExportJob"

instance Data.ToQuery CreateWorldExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorldExportJobResponse' smart constructor.
data CreateWorldExportJobResponse = CreateWorldExportJobResponse'
  { -- | The Amazon Resource Name (ARN) of the world export job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world export job was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
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
    --
    -- [AllWorldGenerationFailed]
    --     All of the worlds in the world generation job failed. This can
    --     happen if your @worldCount@ is greater than 50 or less than 1.
    --
    -- For more information about troubleshooting WorldForge, see
    -- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting-worldforge.html Troubleshooting Simulation WorldForge>.
    failureCode :: Prelude.Maybe WorldExportJobErrorCode,
    -- | The IAM role that the world export process uses to access the Amazon S3
    -- bucket and put the export.
    iamRole :: Prelude.Maybe Prelude.Text,
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
    --     The world export job failed. See @failureCode@ for more information.
    --
    -- [Canceled]
    --     The world export job was cancelled.
    --
    -- [Canceling]
    --     The world export job is being cancelled.
    status :: Prelude.Maybe WorldExportJobStatus,
    -- | A map that contains tag keys and tag values that are attached to the
    -- world export job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorldExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createWorldExportJobResponse_arn' - The Amazon Resource Name (ARN) of the world export job.
--
-- 'clientRequestToken', 'createWorldExportJobResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'createdAt', 'createWorldExportJobResponse_createdAt' - The time, in milliseconds since the epoch, when the world export job was
-- created.
--
-- 'failureCode', 'createWorldExportJobResponse_failureCode' - The failure code of the world export job if it failed:
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
-- [AllWorldGenerationFailed]
--     All of the worlds in the world generation job failed. This can
--     happen if your @worldCount@ is greater than 50 or less than 1.
--
-- For more information about troubleshooting WorldForge, see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting-worldforge.html Troubleshooting Simulation WorldForge>.
--
-- 'iamRole', 'createWorldExportJobResponse_iamRole' - The IAM role that the world export process uses to access the Amazon S3
-- bucket and put the export.
--
-- 'outputLocation', 'createWorldExportJobResponse_outputLocation' - Undocumented member.
--
-- 'status', 'createWorldExportJobResponse_status' - The status of the world export job.
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
--     The world export job failed. See @failureCode@ for more information.
--
-- [Canceled]
--     The world export job was cancelled.
--
-- [Canceling]
--     The world export job is being cancelled.
--
-- 'tags', 'createWorldExportJobResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- world export job.
--
-- 'httpStatus', 'createWorldExportJobResponse_httpStatus' - The response's http status code.
newCreateWorldExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorldExportJobResponse
newCreateWorldExportJobResponse pHttpStatus_ =
  CreateWorldExportJobResponse'
    { arn =
        Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the world export job.
createWorldExportJobResponse_arn :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe Prelude.Text)
createWorldExportJobResponse_arn = Lens.lens (\CreateWorldExportJobResponse' {arn} -> arn) (\s@CreateWorldExportJobResponse' {} a -> s {arn = a} :: CreateWorldExportJobResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldExportJobResponse_clientRequestToken :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe Prelude.Text)
createWorldExportJobResponse_clientRequestToken = Lens.lens (\CreateWorldExportJobResponse' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldExportJobResponse' {} a -> s {clientRequestToken = a} :: CreateWorldExportJobResponse)

-- | The time, in milliseconds since the epoch, when the world export job was
-- created.
createWorldExportJobResponse_createdAt :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe Prelude.UTCTime)
createWorldExportJobResponse_createdAt = Lens.lens (\CreateWorldExportJobResponse' {createdAt} -> createdAt) (\s@CreateWorldExportJobResponse' {} a -> s {createdAt = a} :: CreateWorldExportJobResponse) Prelude.. Lens.mapping Data._Time

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
--
-- [AllWorldGenerationFailed]
--     All of the worlds in the world generation job failed. This can
--     happen if your @worldCount@ is greater than 50 or less than 1.
--
-- For more information about troubleshooting WorldForge, see
-- <https://docs.aws.amazon.com/robomaker/latest/dg/troubleshooting-worldforge.html Troubleshooting Simulation WorldForge>.
createWorldExportJobResponse_failureCode :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe WorldExportJobErrorCode)
createWorldExportJobResponse_failureCode = Lens.lens (\CreateWorldExportJobResponse' {failureCode} -> failureCode) (\s@CreateWorldExportJobResponse' {} a -> s {failureCode = a} :: CreateWorldExportJobResponse)

-- | The IAM role that the world export process uses to access the Amazon S3
-- bucket and put the export.
createWorldExportJobResponse_iamRole :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe Prelude.Text)
createWorldExportJobResponse_iamRole = Lens.lens (\CreateWorldExportJobResponse' {iamRole} -> iamRole) (\s@CreateWorldExportJobResponse' {} a -> s {iamRole = a} :: CreateWorldExportJobResponse)

-- | Undocumented member.
createWorldExportJobResponse_outputLocation :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe OutputLocation)
createWorldExportJobResponse_outputLocation = Lens.lens (\CreateWorldExportJobResponse' {outputLocation} -> outputLocation) (\s@CreateWorldExportJobResponse' {} a -> s {outputLocation = a} :: CreateWorldExportJobResponse)

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
--     The world export job failed. See @failureCode@ for more information.
--
-- [Canceled]
--     The world export job was cancelled.
--
-- [Canceling]
--     The world export job is being cancelled.
createWorldExportJobResponse_status :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe WorldExportJobStatus)
createWorldExportJobResponse_status = Lens.lens (\CreateWorldExportJobResponse' {status} -> status) (\s@CreateWorldExportJobResponse' {} a -> s {status = a} :: CreateWorldExportJobResponse)

-- | A map that contains tag keys and tag values that are attached to the
-- world export job.
createWorldExportJobResponse_tags :: Lens.Lens' CreateWorldExportJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldExportJobResponse_tags = Lens.lens (\CreateWorldExportJobResponse' {tags} -> tags) (\s@CreateWorldExportJobResponse' {} a -> s {tags = a} :: CreateWorldExportJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createWorldExportJobResponse_httpStatus :: Lens.Lens' CreateWorldExportJobResponse Prelude.Int
createWorldExportJobResponse_httpStatus = Lens.lens (\CreateWorldExportJobResponse' {httpStatus} -> httpStatus) (\s@CreateWorldExportJobResponse' {} a -> s {httpStatus = a} :: CreateWorldExportJobResponse)

instance Prelude.NFData CreateWorldExportJobResponse where
  rnf CreateWorldExportJobResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
