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
-- Module      : Network.AWS.MigrationHub.DeleteProgressUpdateStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a progress update stream, including all of its tasks, which was
-- previously created as an AWS resource used for access control. This API
-- has the following traits:
--
-- -   The only parameter needed for @DeleteProgressUpdateStream@ is the
--     stream name (same as a @CreateProgressUpdateStream@ call).
--
-- -   The call will return, and a background process will asynchronously
--     delete the stream and all of its resources (tasks, associated
--     resources, resource attributes, created artifacts).
--
-- -   If the stream takes time to be deleted, it might still show up on a
--     @ListProgressUpdateStreams@ call.
--
-- -   @CreateProgressUpdateStream@, @ImportMigrationTask@,
--     @NotifyMigrationTaskState@, and all Associate[*] APIs related to the
--     tasks belonging to the stream will throw \"InvalidInputException\"
--     if the stream of the same name is in the process of being deleted.
--
-- -   Once the stream and all of its resources are deleted,
--     @CreateProgressUpdateStream@ for a stream of the same name will
--     succeed, and that stream will be an entirely new logical resource
--     (without any resources associated with the old stream).
module Network.AWS.MigrationHub.DeleteProgressUpdateStream
  ( -- * Creating a Request
    DeleteProgressUpdateStream (..),
    newDeleteProgressUpdateStream,

    -- * Request Lenses
    deleteProgressUpdateStream_dryRun,
    deleteProgressUpdateStream_progressUpdateStreamName,

    -- * Destructuring the Response
    DeleteProgressUpdateStreamResponse (..),
    newDeleteProgressUpdateStreamResponse,

    -- * Response Lenses
    deleteProgressUpdateStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProgressUpdateStream' smart constructor.
data DeleteProgressUpdateStream = DeleteProgressUpdateStream'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Core.Maybe Core.Bool,
    -- | The name of the ProgressUpdateStream. /Do not store personal data in
    -- this field./
    progressUpdateStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProgressUpdateStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteProgressUpdateStream_dryRun' - Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
--
-- 'progressUpdateStreamName', 'deleteProgressUpdateStream_progressUpdateStreamName' - The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
newDeleteProgressUpdateStream ::
  -- | 'progressUpdateStreamName'
  Core.Text ->
  DeleteProgressUpdateStream
newDeleteProgressUpdateStream
  pProgressUpdateStreamName_ =
    DeleteProgressUpdateStream'
      { dryRun = Core.Nothing,
        progressUpdateStreamName =
          pProgressUpdateStreamName_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
deleteProgressUpdateStream_dryRun :: Lens.Lens' DeleteProgressUpdateStream (Core.Maybe Core.Bool)
deleteProgressUpdateStream_dryRun = Lens.lens (\DeleteProgressUpdateStream' {dryRun} -> dryRun) (\s@DeleteProgressUpdateStream' {} a -> s {dryRun = a} :: DeleteProgressUpdateStream)

-- | The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
deleteProgressUpdateStream_progressUpdateStreamName :: Lens.Lens' DeleteProgressUpdateStream Core.Text
deleteProgressUpdateStream_progressUpdateStreamName = Lens.lens (\DeleteProgressUpdateStream' {progressUpdateStreamName} -> progressUpdateStreamName) (\s@DeleteProgressUpdateStream' {} a -> s {progressUpdateStreamName = a} :: DeleteProgressUpdateStream)

instance Core.AWSRequest DeleteProgressUpdateStream where
  type
    AWSResponse DeleteProgressUpdateStream =
      DeleteProgressUpdateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProgressUpdateStreamResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProgressUpdateStream

instance Core.NFData DeleteProgressUpdateStream

instance Core.ToHeaders DeleteProgressUpdateStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSMigrationHub.DeleteProgressUpdateStream" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProgressUpdateStream where
  toJSON DeleteProgressUpdateStream' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DryRun" Core..=) Core.<$> dryRun,
            Core.Just
              ( "ProgressUpdateStreamName"
                  Core..= progressUpdateStreamName
              )
          ]
      )

instance Core.ToPath DeleteProgressUpdateStream where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProgressUpdateStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProgressUpdateStreamResponse' smart constructor.
data DeleteProgressUpdateStreamResponse = DeleteProgressUpdateStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProgressUpdateStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProgressUpdateStreamResponse_httpStatus' - The response's http status code.
newDeleteProgressUpdateStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProgressUpdateStreamResponse
newDeleteProgressUpdateStreamResponse pHttpStatus_ =
  DeleteProgressUpdateStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProgressUpdateStreamResponse_httpStatus :: Lens.Lens' DeleteProgressUpdateStreamResponse Core.Int
deleteProgressUpdateStreamResponse_httpStatus = Lens.lens (\DeleteProgressUpdateStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteProgressUpdateStreamResponse' {} a -> s {httpStatus = a} :: DeleteProgressUpdateStreamResponse)

instance
  Core.NFData
    DeleteProgressUpdateStreamResponse
