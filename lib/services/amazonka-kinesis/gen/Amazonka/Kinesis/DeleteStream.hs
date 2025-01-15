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
-- Module      : Amazonka.Kinesis.DeleteStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Kinesis data stream and all its shards and data. You must shut
-- down any applications that are operating on the stream before you delete
-- the stream. If an application attempts to operate on a deleted stream,
-- it receives the exception @ResourceNotFoundException@.
--
-- When invoking this API, it is recommended you use the @StreamARN@ input
-- parameter rather than the @StreamName@ input parameter.
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
-- You can use the DescribeStreamSummary operation to check the state of
-- the stream, which is returned in @StreamStatus@.
--
-- DeleteStream has a limit of five transactions per second per account.
module Amazonka.Kinesis.DeleteStream
  ( -- * Creating a Request
    DeleteStream (..),
    newDeleteStream,

    -- * Request Lenses
    deleteStream_enforceConsumerDeletion,
    deleteStream_streamARN,
    deleteStream_streamName,

    -- * Destructuring the Response
    DeleteStreamResponse (..),
    newDeleteStreamResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for DeleteStream.
--
-- /See:/ 'newDeleteStream' smart constructor.
data DeleteStream = DeleteStream'
  { -- | If this parameter is unset (@null@) or if you set it to @false@, and the
    -- stream has registered consumers, the call to @DeleteStream@ fails with a
    -- @ResourceInUseException@.
    enforceConsumerDeletion :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the stream.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream to delete.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'streamARN', 'deleteStream_streamARN' - The ARN of the stream.
--
-- 'streamName', 'deleteStream_streamName' - The name of the stream to delete.
newDeleteStream ::
  DeleteStream
newDeleteStream =
  DeleteStream'
    { enforceConsumerDeletion =
        Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | If this parameter is unset (@null@) or if you set it to @false@, and the
-- stream has registered consumers, the call to @DeleteStream@ fails with a
-- @ResourceInUseException@.
deleteStream_enforceConsumerDeletion :: Lens.Lens' DeleteStream (Prelude.Maybe Prelude.Bool)
deleteStream_enforceConsumerDeletion = Lens.lens (\DeleteStream' {enforceConsumerDeletion} -> enforceConsumerDeletion) (\s@DeleteStream' {} a -> s {enforceConsumerDeletion = a} :: DeleteStream)

-- | The ARN of the stream.
deleteStream_streamARN :: Lens.Lens' DeleteStream (Prelude.Maybe Prelude.Text)
deleteStream_streamARN = Lens.lens (\DeleteStream' {streamARN} -> streamARN) (\s@DeleteStream' {} a -> s {streamARN = a} :: DeleteStream)

-- | The name of the stream to delete.
deleteStream_streamName :: Lens.Lens' DeleteStream (Prelude.Maybe Prelude.Text)
deleteStream_streamName = Lens.lens (\DeleteStream' {streamName} -> streamName) (\s@DeleteStream' {} a -> s {streamName = a} :: DeleteStream)

instance Core.AWSRequest DeleteStream where
  type AWSResponse DeleteStream = DeleteStreamResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteStreamResponse'

instance Prelude.Hashable DeleteStream where
  hashWithSalt _salt DeleteStream' {..} =
    _salt
      `Prelude.hashWithSalt` enforceConsumerDeletion
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData DeleteStream where
  rnf DeleteStream' {..} =
    Prelude.rnf enforceConsumerDeletion `Prelude.seq`
      Prelude.rnf streamARN `Prelude.seq`
        Prelude.rnf streamName

instance Data.ToHeaders DeleteStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Kinesis_20131202.DeleteStream" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteStream where
  toJSON DeleteStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnforceConsumerDeletion" Data..=)
              Prelude.<$> enforceConsumerDeletion,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath DeleteStream where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStreamResponse' smart constructor.
data DeleteStreamResponse = DeleteStreamResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteStreamResponse ::
  DeleteStreamResponse
newDeleteStreamResponse = DeleteStreamResponse'

instance Prelude.NFData DeleteStreamResponse where
  rnf _ = ()
