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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLogStream' smart constructor.
data DeleteLogStream = DeleteLogStream'
  { -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | The name of the log stream.
    logStreamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'logStreamName'
  Prelude.Text ->
  DeleteLogStream
newDeleteLogStream pLogGroupName_ pLogStreamName_ =
  DeleteLogStream'
    { logGroupName = pLogGroupName_,
      logStreamName = pLogStreamName_
    }

-- | The name of the log group.
deleteLogStream_logGroupName :: Lens.Lens' DeleteLogStream Prelude.Text
deleteLogStream_logGroupName = Lens.lens (\DeleteLogStream' {logGroupName} -> logGroupName) (\s@DeleteLogStream' {} a -> s {logGroupName = a} :: DeleteLogStream)

-- | The name of the log stream.
deleteLogStream_logStreamName :: Lens.Lens' DeleteLogStream Prelude.Text
deleteLogStream_logStreamName = Lens.lens (\DeleteLogStream' {logStreamName} -> logStreamName) (\s@DeleteLogStream' {} a -> s {logStreamName = a} :: DeleteLogStream)

instance Prelude.AWSRequest DeleteLogStream where
  type Rs DeleteLogStream = DeleteLogStreamResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteLogStreamResponse'

instance Prelude.Hashable DeleteLogStream

instance Prelude.NFData DeleteLogStream

instance Prelude.ToHeaders DeleteLogStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.DeleteLogStream" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteLogStream where
  toJSON DeleteLogStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("logGroupName" Prelude..= logGroupName),
            Prelude.Just
              ("logStreamName" Prelude..= logStreamName)
          ]
      )

instance Prelude.ToPath DeleteLogStream where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLogStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLogStreamResponse' smart constructor.
data DeleteLogStreamResponse = DeleteLogStreamResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLogStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLogStreamResponse ::
  DeleteLogStreamResponse
newDeleteLogStreamResponse = DeleteLogStreamResponse'

instance Prelude.NFData DeleteLogStreamResponse
