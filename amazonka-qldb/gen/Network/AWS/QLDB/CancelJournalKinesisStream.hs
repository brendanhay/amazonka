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
-- Module      : Network.AWS.QLDB.CancelJournalKinesisStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Ends a given Amazon QLDB journal stream. Before a stream can be
-- canceled, its current status must be @ACTIVE@.
--
-- You can\'t restart a stream after you cancel it. Canceled QLDB stream
-- resources are subject to a 7-day retention period, so they are
-- automatically deleted after this limit expires.
module Network.AWS.QLDB.CancelJournalKinesisStream
  ( -- * Creating a Request
    CancelJournalKinesisStream (..),
    newCancelJournalKinesisStream,

    -- * Request Lenses
    cancelJournalKinesisStream_ledgerName,
    cancelJournalKinesisStream_streamId,

    -- * Destructuring the Response
    CancelJournalKinesisStreamResponse (..),
    newCancelJournalKinesisStreamResponse,

    -- * Response Lenses
    cancelJournalKinesisStreamResponse_streamId,
    cancelJournalKinesisStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCancelJournalKinesisStream' smart constructor.
data CancelJournalKinesisStream = CancelJournalKinesisStream'
  { -- | The name of the ledger.
    ledgerName :: Prelude.Text,
    -- | The unique ID that QLDB assigns to each QLDB journal stream.
    streamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJournalKinesisStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ledgerName', 'cancelJournalKinesisStream_ledgerName' - The name of the ledger.
--
-- 'streamId', 'cancelJournalKinesisStream_streamId' - The unique ID that QLDB assigns to each QLDB journal stream.
newCancelJournalKinesisStream ::
  -- | 'ledgerName'
  Prelude.Text ->
  -- | 'streamId'
  Prelude.Text ->
  CancelJournalKinesisStream
newCancelJournalKinesisStream pLedgerName_ pStreamId_ =
  CancelJournalKinesisStream'
    { ledgerName =
        pLedgerName_,
      streamId = pStreamId_
    }

-- | The name of the ledger.
cancelJournalKinesisStream_ledgerName :: Lens.Lens' CancelJournalKinesisStream Prelude.Text
cancelJournalKinesisStream_ledgerName = Lens.lens (\CancelJournalKinesisStream' {ledgerName} -> ledgerName) (\s@CancelJournalKinesisStream' {} a -> s {ledgerName = a} :: CancelJournalKinesisStream)

-- | The unique ID that QLDB assigns to each QLDB journal stream.
cancelJournalKinesisStream_streamId :: Lens.Lens' CancelJournalKinesisStream Prelude.Text
cancelJournalKinesisStream_streamId = Lens.lens (\CancelJournalKinesisStream' {streamId} -> streamId) (\s@CancelJournalKinesisStream' {} a -> s {streamId = a} :: CancelJournalKinesisStream)

instance Core.AWSRequest CancelJournalKinesisStream where
  type
    AWSResponse CancelJournalKinesisStream =
      CancelJournalKinesisStreamResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelJournalKinesisStreamResponse'
            Prelude.<$> (x Core..?> "StreamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelJournalKinesisStream

instance Prelude.NFData CancelJournalKinesisStream

instance Core.ToHeaders CancelJournalKinesisStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath CancelJournalKinesisStream where
  toPath CancelJournalKinesisStream' {..} =
    Prelude.mconcat
      [ "/ledgers/",
        Core.toBS ledgerName,
        "/journal-kinesis-streams/",
        Core.toBS streamId
      ]

instance Core.ToQuery CancelJournalKinesisStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelJournalKinesisStreamResponse' smart constructor.
data CancelJournalKinesisStreamResponse = CancelJournalKinesisStreamResponse'
  { -- | The unique ID that QLDB assigns to each QLDB journal stream.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelJournalKinesisStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'cancelJournalKinesisStreamResponse_streamId' - The unique ID that QLDB assigns to each QLDB journal stream.
--
-- 'httpStatus', 'cancelJournalKinesisStreamResponse_httpStatus' - The response's http status code.
newCancelJournalKinesisStreamResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelJournalKinesisStreamResponse
newCancelJournalKinesisStreamResponse pHttpStatus_ =
  CancelJournalKinesisStreamResponse'
    { streamId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID that QLDB assigns to each QLDB journal stream.
cancelJournalKinesisStreamResponse_streamId :: Lens.Lens' CancelJournalKinesisStreamResponse (Prelude.Maybe Prelude.Text)
cancelJournalKinesisStreamResponse_streamId = Lens.lens (\CancelJournalKinesisStreamResponse' {streamId} -> streamId) (\s@CancelJournalKinesisStreamResponse' {} a -> s {streamId = a} :: CancelJournalKinesisStreamResponse)

-- | The response's http status code.
cancelJournalKinesisStreamResponse_httpStatus :: Lens.Lens' CancelJournalKinesisStreamResponse Prelude.Int
cancelJournalKinesisStreamResponse_httpStatus = Lens.lens (\CancelJournalKinesisStreamResponse' {httpStatus} -> httpStatus) (\s@CancelJournalKinesisStreamResponse' {} a -> s {httpStatus = a} :: CancelJournalKinesisStreamResponse)

instance
  Prelude.NFData
    CancelJournalKinesisStreamResponse
