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
-- Module      : Network.AWS.MigrationHub.CreateProgressUpdateStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a progress update stream which is an AWS resource used for
-- access control as well as a namespace for migration task names that is
-- implicitly linked to your AWS account. It must uniquely identify the
-- migration tool as it is used for all updates made by the tool; however,
-- it does not need to be unique for each AWS account because it is scoped
-- to the AWS account.
module Network.AWS.MigrationHub.CreateProgressUpdateStream
  ( -- * Creating a Request
    CreateProgressUpdateStream (..),
    newCreateProgressUpdateStream,

    -- * Request Lenses
    createProgressUpdateStream_dryRun,
    createProgressUpdateStream_progressUpdateStreamName,

    -- * Destructuring the Response
    CreateProgressUpdateStreamResponse (..),
    newCreateProgressUpdateStreamResponse,

    -- * Response Lenses
    createProgressUpdateStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProgressUpdateStream' smart constructor.
data CreateProgressUpdateStream = CreateProgressUpdateStream'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the ProgressUpdateStream. /Do not store personal data in
    -- this field./
    progressUpdateStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProgressUpdateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'createProgressUpdateStream_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStreamName', 'createProgressUpdateStream_progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
newCreateProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Core.Text ->
  CreateProgressUpdateStream
newCreateProgressUpdateStream
  pProgressUpdateStreamName_ =
    CreateProgressUpdateStream'
      { dryRun = Core.Nothing,
        progressUpdateStreamName =
          pProgressUpdateStreamName_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
createProgressUpdateStream_dryRun :: Lens.Lens' CreateProgressUpdateStream (Core.Maybe Core.Bool)
createProgressUpdateStream_dryRun = Lens.lens (\CreateProgressUpdateStream' {dryRun} -> dryRun) (\s@CreateProgressUpdateStream' {} a -> s {dryRun = a} :: CreateProgressUpdateStream)

-- | The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
createProgressUpdateStream_progressUpdateStreamName :: Lens.Lens' CreateProgressUpdateStream Core.Text
createProgressUpdateStream_progressUpdateStreamName = Lens.lens (\CreateProgressUpdateStream' {progressUpdateStreamName} -> progressUpdateStreamName) (\s@CreateProgressUpdateStream' {} a -> s {progressUpdateStreamName = a} :: CreateProgressUpdateStream)

instance Core.AWSRequest CreateProgressUpdateStream where
  type
    AWSResponse CreateProgressUpdateStream =
      CreateProgressUpdateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateProgressUpdateStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProgressUpdateStream

instance Core.NFData CreateProgressUpdateStream

instance Core.ToHeaders CreateProgressUpdateStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.CreateProgressUpdateStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateProgressUpdateStream where
  toJSON CreateProgressUpdateStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            Core.Just
              ( "ProgressUpdateStreamName"
                  Core..= progressUpdateStreamName
              )
          ]
      )

instance Core.ToPath CreateProgressUpdateStream where
  toPath = Core.const "/"

instance Core.ToQuery CreateProgressUpdateStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateProgressUpdateStreamResponse' smart constructor.
data CreateProgressUpdateStreamResponse = CreateProgressUpdateStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProgressUpdateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProgressUpdateStreamResponse_httpStatus' - The response's http status code.
newCreateProgressUpdateStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateProgressUpdateStreamResponse
newCreateProgressUpdateStreamResponse pHttpStatus_ =
  CreateProgressUpdateStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createProgressUpdateStreamResponse_httpStatus :: Lens.Lens' CreateProgressUpdateStreamResponse Core.Int
createProgressUpdateStreamResponse_httpStatus = Lens.lens (\CreateProgressUpdateStreamResponse' {httpStatus} -> httpStatus) (\s@CreateProgressUpdateStreamResponse' {} a -> s {httpStatus = a} :: CreateProgressUpdateStreamResponse)

instance
  Core.NFData
    CreateProgressUpdateStreamResponse
