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
-- Module      : Amazonka.QLDB.DescribeJournalKinesisStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about a given Amazon QLDB journal stream.
-- The output includes the Amazon Resource Name (ARN), stream name, current
-- status, creation time, and the parameters of the original stream
-- creation request.
--
-- This action does not return any expired journal streams. For more
-- information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/streams.create.html#streams.create.states.expiration Expiration for terminal streams>
-- in the /Amazon QLDB Developer Guide/.
module Amazonka.QLDB.DescribeJournalKinesisStream
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJournalKinesisStream' smart constructor.
data DescribeJournalKinesisStream = DescribeJournalKinesisStream'
  { -- | The name of the ledger.
    ledgerName :: Prelude.Text,
    -- | The UUID (represented in Base62-encoded text) of the QLDB journal stream
    -- to describe.
    streamId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'streamId', 'describeJournalKinesisStream_streamId' - The UUID (represented in Base62-encoded text) of the QLDB journal stream
-- to describe.
newDescribeJournalKinesisStream ::
  -- | 'ledgerName'
  Prelude.Text ->
  -- | 'streamId'
  Prelude.Text ->
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
describeJournalKinesisStream_ledgerName :: Lens.Lens' DescribeJournalKinesisStream Prelude.Text
describeJournalKinesisStream_ledgerName = Lens.lens (\DescribeJournalKinesisStream' {ledgerName} -> ledgerName) (\s@DescribeJournalKinesisStream' {} a -> s {ledgerName = a} :: DescribeJournalKinesisStream)

-- | The UUID (represented in Base62-encoded text) of the QLDB journal stream
-- to describe.
describeJournalKinesisStream_streamId :: Lens.Lens' DescribeJournalKinesisStream Prelude.Text
describeJournalKinesisStream_streamId = Lens.lens (\DescribeJournalKinesisStream' {streamId} -> streamId) (\s@DescribeJournalKinesisStream' {} a -> s {streamId = a} :: DescribeJournalKinesisStream)

instance Core.AWSRequest DescribeJournalKinesisStream where
  type
    AWSResponse DescribeJournalKinesisStream =
      DescribeJournalKinesisStreamResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJournalKinesisStreamResponse'
            Prelude.<$> (x Data..?> "Stream")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeJournalKinesisStream
  where
  hashWithSalt _salt DescribeJournalKinesisStream' {..} =
    _salt
      `Prelude.hashWithSalt` ledgerName
      `Prelude.hashWithSalt` streamId

instance Prelude.NFData DescribeJournalKinesisStream where
  rnf DescribeJournalKinesisStream' {..} =
    Prelude.rnf ledgerName
      `Prelude.seq` Prelude.rnf streamId

instance Data.ToHeaders DescribeJournalKinesisStream where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeJournalKinesisStream where
  toPath DescribeJournalKinesisStream' {..} =
    Prelude.mconcat
      [ "/ledgers/",
        Data.toBS ledgerName,
        "/journal-kinesis-streams/",
        Data.toBS streamId
      ]

instance Data.ToQuery DescribeJournalKinesisStream where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJournalKinesisStreamResponse' smart constructor.
data DescribeJournalKinesisStreamResponse = DescribeJournalKinesisStreamResponse'
  { -- | Information about the QLDB journal stream returned by a
    -- @DescribeJournalS3Export@ request.
    stream :: Prelude.Maybe JournalKinesisStreamDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeJournalKinesisStreamResponse
newDescribeJournalKinesisStreamResponse pHttpStatus_ =
  DescribeJournalKinesisStreamResponse'
    { stream =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the QLDB journal stream returned by a
-- @DescribeJournalS3Export@ request.
describeJournalKinesisStreamResponse_stream :: Lens.Lens' DescribeJournalKinesisStreamResponse (Prelude.Maybe JournalKinesisStreamDescription)
describeJournalKinesisStreamResponse_stream = Lens.lens (\DescribeJournalKinesisStreamResponse' {stream} -> stream) (\s@DescribeJournalKinesisStreamResponse' {} a -> s {stream = a} :: DescribeJournalKinesisStreamResponse)

-- | The response's http status code.
describeJournalKinesisStreamResponse_httpStatus :: Lens.Lens' DescribeJournalKinesisStreamResponse Prelude.Int
describeJournalKinesisStreamResponse_httpStatus = Lens.lens (\DescribeJournalKinesisStreamResponse' {httpStatus} -> httpStatus) (\s@DescribeJournalKinesisStreamResponse' {} a -> s {httpStatus = a} :: DescribeJournalKinesisStreamResponse)

instance
  Prelude.NFData
    DescribeJournalKinesisStreamResponse
  where
  rnf DescribeJournalKinesisStreamResponse' {..} =
    Prelude.rnf stream
      `Prelude.seq` Prelude.rnf httpStatus
