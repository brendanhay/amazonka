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
-- Module      : Network.AWS.QLDB.DescribeJournalKinesisStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a given Amazon QLDB journal stream.
-- The output includes the Amazon Resource Name (ARN), stream name, current
-- status, creation time, and the parameters of your original stream
-- creation request.
module Network.AWS.QLDB.DescribeJournalKinesisStream
  ( -- * Creating a Request
    DescribeJournalKinesisStream (..),
    newDescribeJournalKinesisStream,

    -- * Request Lenses
    describeJournalKinesisStream_ledgerName,
    describeJournalKinesisStream_streamId,

    -- * Destructuring the Response
    DescribeJournalKinesisStreamResponse (..),
    newDescribeJournalKinesisStreamResponse,

    -- * Response Lenses
    describeJournalKinesisStreamResponse_stream,
    describeJournalKinesisStreamResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeJournalKinesisStream' smart constructor.
data DescribeJournalKinesisStream = DescribeJournalKinesisStream'
  { -- | The name of the ledger.
    ledgerName :: Core.Text,
    -- | The unique ID that QLDB assigns to each QLDB journal stream.
    streamId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJournalKinesisStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ledgerName', 'describeJournalKinesisStream_ledgerName' - The name of the ledger.
--
-- 'streamId', 'describeJournalKinesisStream_streamId' - The unique ID that QLDB assigns to each QLDB journal stream.
newDescribeJournalKinesisStream ::
  -- | 'ledgerName'
  Core.Text ->
  -- | 'streamId'
  Core.Text ->
  DescribeJournalKinesisStream
newDescribeJournalKinesisStream
  pLedgerName_
  pStreamId_ =
    DescribeJournalKinesisStream'
      { ledgerName =
          pLedgerName_,
        streamId = pStreamId_
      }

-- | The name of the ledger.
describeJournalKinesisStream_ledgerName :: Lens.Lens' DescribeJournalKinesisStream Core.Text
describeJournalKinesisStream_ledgerName = Lens.lens (\DescribeJournalKinesisStream' {ledgerName} -> ledgerName) (\s@DescribeJournalKinesisStream' {} a -> s {ledgerName = a} :: DescribeJournalKinesisStream)

-- | The unique ID that QLDB assigns to each QLDB journal stream.
describeJournalKinesisStream_streamId :: Lens.Lens' DescribeJournalKinesisStream Core.Text
describeJournalKinesisStream_streamId = Lens.lens (\DescribeJournalKinesisStream' {streamId} -> streamId) (\s@DescribeJournalKinesisStream' {} a -> s {streamId = a} :: DescribeJournalKinesisStream)

instance Core.AWSRequest DescribeJournalKinesisStream where
  type
    AWSResponse DescribeJournalKinesisStream =
      DescribeJournalKinesisStreamResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJournalKinesisStreamResponse'
            Core.<$> (x Core..?> "Stream")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeJournalKinesisStream

instance Core.NFData DescribeJournalKinesisStream

instance Core.ToHeaders DescribeJournalKinesisStream where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeJournalKinesisStream where
  toPath DescribeJournalKinesisStream' {..} =
    Core.mconcat
      [ "/ledgers/",
        Core.toBS ledgerName,
        "/journal-kinesis-streams/",
        Core.toBS streamId
      ]

instance Core.ToQuery DescribeJournalKinesisStream where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeJournalKinesisStreamResponse' smart constructor.
data DescribeJournalKinesisStreamResponse = DescribeJournalKinesisStreamResponse'
  { -- | Information about the QLDB journal stream returned by a
    -- @DescribeJournalS3Export@ request.
    stream :: Core.Maybe JournalKinesisStreamDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeJournalKinesisStreamResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stream', 'describeJournalKinesisStreamResponse_stream' - Information about the QLDB journal stream returned by a
-- @DescribeJournalS3Export@ request.
--
-- 'httpStatus', 'describeJournalKinesisStreamResponse_httpStatus' - The response's http status code.
newDescribeJournalKinesisStreamResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeJournalKinesisStreamResponse
newDescribeJournalKinesisStreamResponse pHttpStatus_ =
  DescribeJournalKinesisStreamResponse'
    { stream =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the QLDB journal stream returned by a
-- @DescribeJournalS3Export@ request.
describeJournalKinesisStreamResponse_stream :: Lens.Lens' DescribeJournalKinesisStreamResponse (Core.Maybe JournalKinesisStreamDescription)
describeJournalKinesisStreamResponse_stream = Lens.lens (\DescribeJournalKinesisStreamResponse' {stream} -> stream) (\s@DescribeJournalKinesisStreamResponse' {} a -> s {stream = a} :: DescribeJournalKinesisStreamResponse)

-- | The response's http status code.
describeJournalKinesisStreamResponse_httpStatus :: Lens.Lens' DescribeJournalKinesisStreamResponse Core.Int
describeJournalKinesisStreamResponse_httpStatus = Lens.lens (\DescribeJournalKinesisStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeJournalKinesisStreamResponse' {} a -> s {httpStatus = a} :: DescribeJournalKinesisStreamResponse)

instance
  Core.NFData
    DescribeJournalKinesisStreamResponse
