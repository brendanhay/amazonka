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
-- Module      : Amazonka.RobOMaker.CreateWorldGenerationJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates worlds using the specified template.
module Amazonka.RobOMaker.CreateWorldGenerationJob
  ( -- * Creating a Request
    CreateWorldGenerationJob (..),
    newCreateWorldGenerationJob,

    -- * Request Lenses
    createWorldGenerationJob_tags,
    createWorldGenerationJob_worldTags,
    createWorldGenerationJob_clientRequestToken,
    createWorldGenerationJob_template,
    createWorldGenerationJob_worldCount,

    -- * Destructuring the Response
    CreateWorldGenerationJobResponse (..),
    newCreateWorldGenerationJobResponse,

    -- * Response Lenses
    createWorldGenerationJobResponse_tags,
    createWorldGenerationJobResponse_worldCount,
    createWorldGenerationJobResponse_worldTags,
    createWorldGenerationJobResponse_failureCode,
    createWorldGenerationJobResponse_clientRequestToken,
    createWorldGenerationJobResponse_arn,
    createWorldGenerationJobResponse_status,
    createWorldGenerationJobResponse_createdAt,
    createWorldGenerationJobResponse_template,
    createWorldGenerationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateWorldGenerationJob' smart constructor.
data CreateWorldGenerationJob = CreateWorldGenerationJob'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- world generator job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map that contains tag keys and tag values that are attached to the
    -- generated worlds.
    worldTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (arn) of the world template describing the
    -- worlds you want to create.
    template :: Prelude.Text,
    -- | Information about the world count.
    worldCount :: WorldCount
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorldGenerationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorldGenerationJob_tags' - A map that contains tag keys and tag values that are attached to the
-- world generator job.
--
-- 'worldTags', 'createWorldGenerationJob_worldTags' - A map that contains tag keys and tag values that are attached to the
-- generated worlds.
--
-- 'clientRequestToken', 'createWorldGenerationJob_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'template', 'createWorldGenerationJob_template' - The Amazon Resource Name (arn) of the world template describing the
-- worlds you want to create.
--
-- 'worldCount', 'createWorldGenerationJob_worldCount' - Information about the world count.
newCreateWorldGenerationJob ::
  -- | 'template'
  Prelude.Text ->
  -- | 'worldCount'
  WorldCount ->
  CreateWorldGenerationJob
newCreateWorldGenerationJob pTemplate_ pWorldCount_ =
  CreateWorldGenerationJob'
    { tags = Prelude.Nothing,
      worldTags = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      template = pTemplate_,
      worldCount = pWorldCount_
    }

-- | A map that contains tag keys and tag values that are attached to the
-- world generator job.
createWorldGenerationJob_tags :: Lens.Lens' CreateWorldGenerationJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldGenerationJob_tags = Lens.lens (\CreateWorldGenerationJob' {tags} -> tags) (\s@CreateWorldGenerationJob' {} a -> s {tags = a} :: CreateWorldGenerationJob) Prelude.. Lens.mapping Lens.coerced

-- | A map that contains tag keys and tag values that are attached to the
-- generated worlds.
createWorldGenerationJob_worldTags :: Lens.Lens' CreateWorldGenerationJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldGenerationJob_worldTags = Lens.lens (\CreateWorldGenerationJob' {worldTags} -> worldTags) (\s@CreateWorldGenerationJob' {} a -> s {worldTags = a} :: CreateWorldGenerationJob) Prelude.. Lens.mapping Lens.coerced

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldGenerationJob_clientRequestToken :: Lens.Lens' CreateWorldGenerationJob (Prelude.Maybe Prelude.Text)
createWorldGenerationJob_clientRequestToken = Lens.lens (\CreateWorldGenerationJob' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldGenerationJob' {} a -> s {clientRequestToken = a} :: CreateWorldGenerationJob)

-- | The Amazon Resource Name (arn) of the world template describing the
-- worlds you want to create.
createWorldGenerationJob_template :: Lens.Lens' CreateWorldGenerationJob Prelude.Text
createWorldGenerationJob_template = Lens.lens (\CreateWorldGenerationJob' {template} -> template) (\s@CreateWorldGenerationJob' {} a -> s {template = a} :: CreateWorldGenerationJob)

-- | Information about the world count.
createWorldGenerationJob_worldCount :: Lens.Lens' CreateWorldGenerationJob WorldCount
createWorldGenerationJob_worldCount = Lens.lens (\CreateWorldGenerationJob' {worldCount} -> worldCount) (\s@CreateWorldGenerationJob' {} a -> s {worldCount = a} :: CreateWorldGenerationJob)

instance Core.AWSRequest CreateWorldGenerationJob where
  type
    AWSResponse CreateWorldGenerationJob =
      CreateWorldGenerationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorldGenerationJobResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "worldCount")
            Prelude.<*> (x Data..?> "worldTags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "clientRequestToken")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "template")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorldGenerationJob where
  hashWithSalt _salt CreateWorldGenerationJob' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` worldTags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` template
      `Prelude.hashWithSalt` worldCount

instance Prelude.NFData CreateWorldGenerationJob where
  rnf CreateWorldGenerationJob' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf worldTags
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf template
      `Prelude.seq` Prelude.rnf worldCount

instance Data.ToHeaders CreateWorldGenerationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorldGenerationJob where
  toJSON CreateWorldGenerationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("worldTags" Data..=) Prelude.<$> worldTags,
            ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("template" Data..= template),
            Prelude.Just ("worldCount" Data..= worldCount)
          ]
      )

instance Data.ToPath CreateWorldGenerationJob where
  toPath = Prelude.const "/createWorldGenerationJob"

instance Data.ToQuery CreateWorldGenerationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorldGenerationJobResponse' smart constructor.
data CreateWorldGenerationJobResponse = CreateWorldGenerationJobResponse'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- world generator job.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information about the world count.
    worldCount :: Prelude.Maybe WorldCount,
    -- | A map that contains tag keys and tag values that are attached to the
    -- generated worlds.
    worldTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The failure code of the world generator job if it failed:
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
    failureCode :: Prelude.Maybe WorldGenerationJobErrorCode,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the world generator job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the world generator job.
    --
    -- [Pending]
    --     The world generator job request is pending.
    --
    -- [Running]
    --     The world generator job is running.
    --
    -- [Completed]
    --     The world generator job completed.
    --
    -- [Failed]
    --     The world generator job failed. See @failureCode@ for more
    --     information.
    --
    -- [PartialFailed]
    --     Some worlds did not generate.
    --
    -- [Canceled]
    --     The world generator job was cancelled.
    --
    -- [Canceling]
    --     The world generator job is being cancelled.
    status :: Prelude.Maybe WorldGenerationJobStatus,
    -- | The time, in milliseconds since the epoch, when the world generator job
    -- was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (arn) of the world template.
    template :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorldGenerationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorldGenerationJobResponse_tags' - A map that contains tag keys and tag values that are attached to the
-- world generator job.
--
-- 'worldCount', 'createWorldGenerationJobResponse_worldCount' - Information about the world count.
--
-- 'worldTags', 'createWorldGenerationJobResponse_worldTags' - A map that contains tag keys and tag values that are attached to the
-- generated worlds.
--
-- 'failureCode', 'createWorldGenerationJobResponse_failureCode' - The failure code of the world generator job if it failed:
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
-- 'clientRequestToken', 'createWorldGenerationJobResponse_clientRequestToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'arn', 'createWorldGenerationJobResponse_arn' - The Amazon Resource Name (ARN) of the world generator job.
--
-- 'status', 'createWorldGenerationJobResponse_status' - The status of the world generator job.
--
-- [Pending]
--     The world generator job request is pending.
--
-- [Running]
--     The world generator job is running.
--
-- [Completed]
--     The world generator job completed.
--
-- [Failed]
--     The world generator job failed. See @failureCode@ for more
--     information.
--
-- [PartialFailed]
--     Some worlds did not generate.
--
-- [Canceled]
--     The world generator job was cancelled.
--
-- [Canceling]
--     The world generator job is being cancelled.
--
-- 'createdAt', 'createWorldGenerationJobResponse_createdAt' - The time, in milliseconds since the epoch, when the world generator job
-- was created.
--
-- 'template', 'createWorldGenerationJobResponse_template' - The Amazon Resource Name (arn) of the world template.
--
-- 'httpStatus', 'createWorldGenerationJobResponse_httpStatus' - The response's http status code.
newCreateWorldGenerationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorldGenerationJobResponse
newCreateWorldGenerationJobResponse pHttpStatus_ =
  CreateWorldGenerationJobResponse'
    { tags =
        Prelude.Nothing,
      worldCount = Prelude.Nothing,
      worldTags = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      clientRequestToken = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      template = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A map that contains tag keys and tag values that are attached to the
-- world generator job.
createWorldGenerationJobResponse_tags :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldGenerationJobResponse_tags = Lens.lens (\CreateWorldGenerationJobResponse' {tags} -> tags) (\s@CreateWorldGenerationJobResponse' {} a -> s {tags = a} :: CreateWorldGenerationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the world count.
createWorldGenerationJobResponse_worldCount :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe WorldCount)
createWorldGenerationJobResponse_worldCount = Lens.lens (\CreateWorldGenerationJobResponse' {worldCount} -> worldCount) (\s@CreateWorldGenerationJobResponse' {} a -> s {worldCount = a} :: CreateWorldGenerationJobResponse)

-- | A map that contains tag keys and tag values that are attached to the
-- generated worlds.
createWorldGenerationJobResponse_worldTags :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorldGenerationJobResponse_worldTags = Lens.lens (\CreateWorldGenerationJobResponse' {worldTags} -> worldTags) (\s@CreateWorldGenerationJobResponse' {} a -> s {worldTags = a} :: CreateWorldGenerationJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The failure code of the world generator job if it failed:
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
createWorldGenerationJobResponse_failureCode :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe WorldGenerationJobErrorCode)
createWorldGenerationJobResponse_failureCode = Lens.lens (\CreateWorldGenerationJobResponse' {failureCode} -> failureCode) (\s@CreateWorldGenerationJobResponse' {} a -> s {failureCode = a} :: CreateWorldGenerationJobResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createWorldGenerationJobResponse_clientRequestToken :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe Prelude.Text)
createWorldGenerationJobResponse_clientRequestToken = Lens.lens (\CreateWorldGenerationJobResponse' {clientRequestToken} -> clientRequestToken) (\s@CreateWorldGenerationJobResponse' {} a -> s {clientRequestToken = a} :: CreateWorldGenerationJobResponse)

-- | The Amazon Resource Name (ARN) of the world generator job.
createWorldGenerationJobResponse_arn :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe Prelude.Text)
createWorldGenerationJobResponse_arn = Lens.lens (\CreateWorldGenerationJobResponse' {arn} -> arn) (\s@CreateWorldGenerationJobResponse' {} a -> s {arn = a} :: CreateWorldGenerationJobResponse)

-- | The status of the world generator job.
--
-- [Pending]
--     The world generator job request is pending.
--
-- [Running]
--     The world generator job is running.
--
-- [Completed]
--     The world generator job completed.
--
-- [Failed]
--     The world generator job failed. See @failureCode@ for more
--     information.
--
-- [PartialFailed]
--     Some worlds did not generate.
--
-- [Canceled]
--     The world generator job was cancelled.
--
-- [Canceling]
--     The world generator job is being cancelled.
createWorldGenerationJobResponse_status :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe WorldGenerationJobStatus)
createWorldGenerationJobResponse_status = Lens.lens (\CreateWorldGenerationJobResponse' {status} -> status) (\s@CreateWorldGenerationJobResponse' {} a -> s {status = a} :: CreateWorldGenerationJobResponse)

-- | The time, in milliseconds since the epoch, when the world generator job
-- was created.
createWorldGenerationJobResponse_createdAt :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe Prelude.UTCTime)
createWorldGenerationJobResponse_createdAt = Lens.lens (\CreateWorldGenerationJobResponse' {createdAt} -> createdAt) (\s@CreateWorldGenerationJobResponse' {} a -> s {createdAt = a} :: CreateWorldGenerationJobResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (arn) of the world template.
createWorldGenerationJobResponse_template :: Lens.Lens' CreateWorldGenerationJobResponse (Prelude.Maybe Prelude.Text)
createWorldGenerationJobResponse_template = Lens.lens (\CreateWorldGenerationJobResponse' {template} -> template) (\s@CreateWorldGenerationJobResponse' {} a -> s {template = a} :: CreateWorldGenerationJobResponse)

-- | The response's http status code.
createWorldGenerationJobResponse_httpStatus :: Lens.Lens' CreateWorldGenerationJobResponse Prelude.Int
createWorldGenerationJobResponse_httpStatus = Lens.lens (\CreateWorldGenerationJobResponse' {httpStatus} -> httpStatus) (\s@CreateWorldGenerationJobResponse' {} a -> s {httpStatus = a} :: CreateWorldGenerationJobResponse)

instance
  Prelude.NFData
    CreateWorldGenerationJobResponse
  where
  rnf CreateWorldGenerationJobResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf worldCount
      `Prelude.seq` Prelude.rnf worldTags
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf template
      `Prelude.seq` Prelude.rnf httpStatus
