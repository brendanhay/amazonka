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
-- Module      : Network.AWS.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis data stream and all its shards and data. You must shut
-- down any applications that are operating on the stream before you delete
-- the stream. If an application attempts to operate on a deleted stream,
-- it receives the exception @ResourceNotFoundException@.
--
-- If the stream is in the @ACTIVE@ state, you can delete it. After a
-- @DeleteStream@ request, the specified stream is in the @DELETING@ state
-- until Kinesis Data Streams completes the deletion.
--
-- __Note:__ Kinesis Data Streams might continue to accept data read and
-- write operations, such as PutRecord, PutRecords, and GetRecords, on a
-- stream in the @DELETING@ state until the stream deletion is complete.
--
-- When you delete a stream, any shards in that stream are also deleted,
-- and any tags are dissociated from the stream.
--
-- You can use the DescribeStream operation to check the state of the
-- stream, which is returned in @StreamStatus@.
--
-- DeleteStream has a limit of five transactions per second per account.
module Network.AWS.Kinesis.DeleteStream
  ( -- * Creating a Request
    DeleteStream (..),
    newDeleteStream,

    -- * Request Lenses
    deleteStream_enforceConsumerDeletion,
    deleteStream_streamName,

    -- * Destructuring the Response
    DeleteStreamResponse (..),
    newDeleteStreamResponse,
  )
where

import Network.AWS.Kinesis.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for DeleteStream.
--
-- /See:/ 'newDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | If this parameter is unset (@null@) or if you set it to @false@, and the
    -- stream has registered consumers, the call to @DeleteStream@ fails with a
    -- @ResourceInUseException@.
    enforceConsumerDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The name of the stream to delete.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enforceConsumerDeletion', 'deleteStream_enforceConsumerDeletion' - If this parameter is unset (@null@) or if you set it to @false@, and the
-- stream has registered consumers, the call to @DeleteStream@ fails with a
-- @ResourceInUseException@.
--
-- 'streamName', 'deleteStream_streamName' - The name of the stream to delete.
newDeleteStream ::
  -- | 'streamName'
  Prelude.Text ->
  DeleteStream
newDeleteStream pStreamName_ =
  DeleteStream'
    { enforceConsumerDeletion =
        Prelude.Nothing,
      streamName = pStreamName_
    }

-- | If this parameter is unset (@null@) or if you set it to @false@, and the
-- stream has registered consumers, the call to @DeleteStream@ fails with a
-- @ResourceInUseException@.
deleteStream_enforceConsumerDeletion :: Lens.Lens' DeleteStream (Prelude.Maybe Prelude.Bool)
deleteStream_enforceConsumerDeletion = Lens.lens (\DeleteStream' {enforceConsumerDeletion} -> enforceConsumerDeletion) (\s@DeleteStream' {} a -> s {enforceConsumerDeletion = a} :: DeleteStream)

-- | The name of the stream to delete.
deleteStream_streamName :: Lens.Lens' DeleteStream Prelude.Text
deleteStream_streamName = Lens.lens (\DeleteStream' {streamName} -> streamName) (\s@DeleteStream' {} a -> s {streamName = a} :: DeleteStream)

instance Prelude.AWSRequest DeleteStream where
  type Rs DeleteStream = DeleteStreamResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteStreamResponse'

instance Prelude.Hashable DeleteStream

instance Prelude.NFData DeleteStream

instance Prelude.ToHeaders DeleteStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Kinesis_20131202.DeleteStream" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteStream where
  toJSON DeleteStream' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EnforceConsumerDeletion" Prelude..=)
              Prelude.<$> enforceConsumerDeletion,
            Prelude.Just ("StreamName" Prelude..= streamName)
          ]
      )

instance Prelude.ToPath DeleteStream where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStreamResponse ::
  DeleteStreamResponse
newDeleteStreamResponse = DeleteStreamResponse'

instance Prelude.NFData DeleteStreamResponse
