{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProgressUpdateStream' smart constructor.
data DeleteProgressUpdateStream = DeleteProgressUpdateStream'
  { -- | Optional boolean flag to indicate whether any effect should take place.
    -- Used to test if the caller has permission to make the call.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name of the ProgressUpdateStream. /Do not store personal data in
    -- this field./
    progressUpdateStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteProgressUpdateStream
newDeleteProgressUpdateStream
  pProgressUpdateStreamName_ =
    DeleteProgressUpdateStream'
      { dryRun =
          Prelude.Nothing,
        progressUpdateStreamName =
          pProgressUpdateStreamName_
      }

-- | Optional boolean flag to indicate whether any effect should take place.
-- Used to test if the caller has permission to make the call.
deleteProgressUpdateStream_dryRun :: Lens.Lens' DeleteProgressUpdateStream (Prelude.Maybe Prelude.Bool)
deleteProgressUpdateStream_dryRun = Lens.lens (\DeleteProgressUpdateStream' {dryRun} -> dryRun) (\s@DeleteProgressUpdateStream' {} a -> s {dryRun = a} :: DeleteProgressUpdateStream)

-- | The name of the ProgressUpdateStream. /Do not store personal data in
-- this field./
deleteProgressUpdateStream_progressUpdateStreamName :: Lens.Lens' DeleteProgressUpdateStream Prelude.Text
deleteProgressUpdateStream_progressUpdateStreamName = Lens.lens (\DeleteProgressUpdateStream' {progressUpdateStreamName} -> progressUpdateStreamName) (\s@DeleteProgressUpdateStream' {} a -> s {progressUpdateStreamName = a} :: DeleteProgressUpdateStream)

instance
  Prelude.AWSRequest
    DeleteProgressUpdateStream
  where
  type
    Rs DeleteProgressUpdateStream =
      DeleteProgressUpdateStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProgressUpdateStreamResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProgressUpdateStream

instance Prelude.NFData DeleteProgressUpdateStream

instance Prelude.ToHeaders DeleteProgressUpdateStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSMigrationHub.DeleteProgressUpdateStream" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProgressUpdateStream where
  toJSON DeleteProgressUpdateStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DryRun" Prelude..=) Prelude.<$> dryRun,
            Prelude.Just
              ( "ProgressUpdateStreamName"
                  Prelude..= progressUpdateStreamName
              )
          ]
      )

instance Prelude.ToPath DeleteProgressUpdateStream where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProgressUpdateStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProgressUpdateStreamResponse' smart constructor.
data DeleteProgressUpdateStreamResponse = DeleteProgressUpdateStreamResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteProgressUpdateStreamResponse
newDeleteProgressUpdateStreamResponse pHttpStatus_ =
  DeleteProgressUpdateStreamResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteProgressUpdateStreamResponse_httpStatus :: Lens.Lens' DeleteProgressUpdateStreamResponse Prelude.Int
deleteProgressUpdateStreamResponse_httpStatus = Lens.lens (\DeleteProgressUpdateStreamResponse' {httpStatus} -> httpStatus) (\s@DeleteProgressUpdateStreamResponse' {} a -> s {httpStatus = a} :: DeleteProgressUpdateStreamResponse)

instance
  Prelude.NFData
    DeleteProgressUpdateStreamResponse
