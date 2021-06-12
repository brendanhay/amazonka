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
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log stream and permanently deletes all the
-- archived log events associated with the log stream.
module Network.AWS.CloudWatchLogs.DeleteLogStream
  ( -- * Creating a Request
    DeleteLogStream (..),
    newDeleteLogStream,

    -- * Request Lenses
    deleteLogStream_logGroupName,
    deleteLogStream_logStreamName,

    -- * Destructuring the Response
    DeleteLogStreamResponse (..),
    newDeleteLogStreamResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLogStream' smart constructor.
data DeleteLogStream = DeleteLogStream'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | The name of the log stream.
    logStreamName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLogStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'deleteLogStream_logGroupName' - The name of the log group.
--
-- 'logStreamName', 'deleteLogStream_logStreamName' - The name of the log stream.
newDeleteLogStream ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'logStreamName'
  Core.Text ->
  DeleteLogStream
newDeleteLogStream pLogGroupName_ pLogStreamName_ =
  DeleteLogStream'
    { logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The name of the log group.
deleteLogStream_logGroupName :: Lens.Lens' DeleteLogStream Core.Text
deleteLogStream_logGroupName = Lens.lens (\DeleteLogStream' {logGroupName} -> logGroupName) (\s@DeleteLogStream' {} a -> s {logGroupName = a} :: DeleteLogStream)

-- | The name of the log stream.
deleteLogStream_logStreamName :: Lens.Lens' DeleteLogStream Core.Text
deleteLogStream_logStreamName = Lens.lens (\DeleteLogStream' {logStreamName} -> logStreamName) (\s@DeleteLogStream' {} a -> s {logStreamName = a} :: DeleteLogStream)

instance Core.AWSRequest DeleteLogStream where
  type
    AWSResponse DeleteLogStream =
      DeleteLogStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteLogStreamResponse'

instance Core.Hashable DeleteLogStream

instance Core.NFData DeleteLogStream

instance Core.ToHeaders DeleteLogStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.DeleteLogStream" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLogStream where
  toJSON DeleteLogStream' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("logStreamName" Core..= logStreamName)
          ]
      )

instance Core.ToPath DeleteLogStream where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLogStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLogStreamResponse' smart constructor.
data DeleteLogStreamResponse = DeleteLogStreamResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLogStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLogStreamResponse ::
  DeleteLogStreamResponse
newDeleteLogStreamResponse = DeleteLogStreamResponse'

instance Core.NFData DeleteLogStreamResponse
