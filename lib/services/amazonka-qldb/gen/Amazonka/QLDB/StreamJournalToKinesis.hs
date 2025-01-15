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
-- Module      : Amazonka.QLDB.StreamJournalToKinesis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a journal stream for a given Amazon QLDB ledger. The stream
-- captures every document revision that is committed to the ledger\'s
-- journal and delivers the data to a specified Amazon Kinesis Data Streams
-- resource.
module Amazonka.QLDB.StreamJournalToKinesis
  ( -- * Creating a Request
    StreamJournalToKinesis (..),
    newStreamJournalToKinesis,

    -- * Request Lenses
    streamJournalToKinesis_exclusiveEndTime,
    streamJournalToKinesis_tags,
    streamJournalToKinesis_ledgerName,
    streamJournalToKinesis_roleArn,
    streamJournalToKinesis_inclusiveStartTime,
    streamJournalToKinesis_kinesisConfiguration,
    streamJournalToKinesis_streamName,

    -- * Destructuring the Response
    StreamJournalToKinesisResponse (..),
    newStreamJournalToKinesisResponse,

    -- * Response Lenses
    streamJournalToKinesisResponse_streamId,
    streamJournalToKinesisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStreamJournalToKinesis' smart constructor.
data StreamJournalToKinesis = StreamJournalToKinesis'
  { -- | The exclusive date and time that specifies when the stream ends. If you
    -- don\'t define this parameter, the stream runs indefinitely until you
    -- cancel it.
    --
    -- The @ExclusiveEndTime@ must be in @ISO 8601@ date and time format and in
    -- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@.
    exclusiveEndTime :: Prelude.Maybe Data.POSIX,
    -- | The key-value pairs to add as tags to the stream that you want to
    -- create. Tag keys are case sensitive. Tag values are case sensitive and
    -- can be null.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the ledger.
    ledgerName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
    -- permissions for a journal stream to write data records to a Kinesis Data
    -- Streams resource.
    --
    -- To pass a role to QLDB when requesting a journal stream, you must have
    -- permissions to perform the @iam:PassRole@ action on the IAM role
    -- resource. This is required for all journal stream requests.
    roleArn :: Prelude.Text,
    -- | The inclusive start date and time from which to start streaming journal
    -- data. This parameter must be in @ISO 8601@ date and time format and in
    -- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@.
    --
    -- The @InclusiveStartTime@ cannot be in the future and must be before
    -- @ExclusiveEndTime@.
    --
    -- If you provide an @InclusiveStartTime@ that is before the ledger\'s
    -- @CreationDateTime@, QLDB effectively defaults it to the ledger\'s
    -- @CreationDateTime@.
    inclusiveStartTime :: Data.POSIX,
    -- | The configuration settings of the Kinesis Data Streams destination for
    -- your stream request.
    kinesisConfiguration :: KinesisConfiguration,
    -- | The name that you want to assign to the QLDB journal stream.
    -- User-defined names can help identify and indicate the purpose of a
    -- stream.
    --
    -- Your stream name must be unique among other /active/ streams for a given
    -- ledger. Stream names have the same naming constraints as ledger names,
    -- as defined in
    -- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
    -- in the /Amazon QLDB Developer Guide/.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamJournalToKinesis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exclusiveEndTime', 'streamJournalToKinesis_exclusiveEndTime' - The exclusive date and time that specifies when the stream ends. If you
-- don\'t define this parameter, the stream runs indefinitely until you
-- cancel it.
--
-- The @ExclusiveEndTime@ must be in @ISO 8601@ date and time format and in
-- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@.
--
-- 'tags', 'streamJournalToKinesis_tags' - The key-value pairs to add as tags to the stream that you want to
-- create. Tag keys are case sensitive. Tag values are case sensitive and
-- can be null.
--
-- 'ledgerName', 'streamJournalToKinesis_ledgerName' - The name of the ledger.
--
-- 'roleArn', 'streamJournalToKinesis_roleArn' - The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal stream to write data records to a Kinesis Data
-- Streams resource.
--
-- To pass a role to QLDB when requesting a journal stream, you must have
-- permissions to perform the @iam:PassRole@ action on the IAM role
-- resource. This is required for all journal stream requests.
--
-- 'inclusiveStartTime', 'streamJournalToKinesis_inclusiveStartTime' - The inclusive start date and time from which to start streaming journal
-- data. This parameter must be in @ISO 8601@ date and time format and in
-- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@.
--
-- The @InclusiveStartTime@ cannot be in the future and must be before
-- @ExclusiveEndTime@.
--
-- If you provide an @InclusiveStartTime@ that is before the ledger\'s
-- @CreationDateTime@, QLDB effectively defaults it to the ledger\'s
-- @CreationDateTime@.
--
-- 'kinesisConfiguration', 'streamJournalToKinesis_kinesisConfiguration' - The configuration settings of the Kinesis Data Streams destination for
-- your stream request.
--
-- 'streamName', 'streamJournalToKinesis_streamName' - The name that you want to assign to the QLDB journal stream.
-- User-defined names can help identify and indicate the purpose of a
-- stream.
--
-- Your stream name must be unique among other /active/ streams for a given
-- ledger. Stream names have the same naming constraints as ledger names,
-- as defined in
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
-- in the /Amazon QLDB Developer Guide/.
newStreamJournalToKinesis ::
  -- | 'ledgerName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'inclusiveStartTime'
  Prelude.UTCTime ->
  -- | 'kinesisConfiguration'
  KinesisConfiguration ->
  -- | 'streamName'
  Prelude.Text ->
  StreamJournalToKinesis
newStreamJournalToKinesis
  pLedgerName_
  pRoleArn_
  pInclusiveStartTime_
  pKinesisConfiguration_
  pStreamName_ =
    StreamJournalToKinesis'
      { exclusiveEndTime =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        ledgerName = pLedgerName_,
        roleArn = pRoleArn_,
        inclusiveStartTime =
          Data._Time Lens.# pInclusiveStartTime_,
        kinesisConfiguration = pKinesisConfiguration_,
        streamName = pStreamName_
      }

-- | The exclusive date and time that specifies when the stream ends. If you
-- don\'t define this parameter, the stream runs indefinitely until you
-- cancel it.
--
-- The @ExclusiveEndTime@ must be in @ISO 8601@ date and time format and in
-- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@.
streamJournalToKinesis_exclusiveEndTime :: Lens.Lens' StreamJournalToKinesis (Prelude.Maybe Prelude.UTCTime)
streamJournalToKinesis_exclusiveEndTime = Lens.lens (\StreamJournalToKinesis' {exclusiveEndTime} -> exclusiveEndTime) (\s@StreamJournalToKinesis' {} a -> s {exclusiveEndTime = a} :: StreamJournalToKinesis) Prelude.. Lens.mapping Data._Time

-- | The key-value pairs to add as tags to the stream that you want to
-- create. Tag keys are case sensitive. Tag values are case sensitive and
-- can be null.
streamJournalToKinesis_tags :: Lens.Lens' StreamJournalToKinesis (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
streamJournalToKinesis_tags = Lens.lens (\StreamJournalToKinesis' {tags} -> tags) (\s@StreamJournalToKinesis' {} a -> s {tags = a} :: StreamJournalToKinesis) Prelude.. Lens.mapping Lens.coerced

-- | The name of the ledger.
streamJournalToKinesis_ledgerName :: Lens.Lens' StreamJournalToKinesis Prelude.Text
streamJournalToKinesis_ledgerName = Lens.lens (\StreamJournalToKinesis' {ledgerName} -> ledgerName) (\s@StreamJournalToKinesis' {} a -> s {ledgerName = a} :: StreamJournalToKinesis)

-- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal stream to write data records to a Kinesis Data
-- Streams resource.
--
-- To pass a role to QLDB when requesting a journal stream, you must have
-- permissions to perform the @iam:PassRole@ action on the IAM role
-- resource. This is required for all journal stream requests.
streamJournalToKinesis_roleArn :: Lens.Lens' StreamJournalToKinesis Prelude.Text
streamJournalToKinesis_roleArn = Lens.lens (\StreamJournalToKinesis' {roleArn} -> roleArn) (\s@StreamJournalToKinesis' {} a -> s {roleArn = a} :: StreamJournalToKinesis)

-- | The inclusive start date and time from which to start streaming journal
-- data. This parameter must be in @ISO 8601@ date and time format and in
-- Universal Coordinated Time (UTC). For example: @2019-06-13T21:36:34Z@.
--
-- The @InclusiveStartTime@ cannot be in the future and must be before
-- @ExclusiveEndTime@.
--
-- If you provide an @InclusiveStartTime@ that is before the ledger\'s
-- @CreationDateTime@, QLDB effectively defaults it to the ledger\'s
-- @CreationDateTime@.
streamJournalToKinesis_inclusiveStartTime :: Lens.Lens' StreamJournalToKinesis Prelude.UTCTime
streamJournalToKinesis_inclusiveStartTime = Lens.lens (\StreamJournalToKinesis' {inclusiveStartTime} -> inclusiveStartTime) (\s@StreamJournalToKinesis' {} a -> s {inclusiveStartTime = a} :: StreamJournalToKinesis) Prelude.. Data._Time

-- | The configuration settings of the Kinesis Data Streams destination for
-- your stream request.
streamJournalToKinesis_kinesisConfiguration :: Lens.Lens' StreamJournalToKinesis KinesisConfiguration
streamJournalToKinesis_kinesisConfiguration = Lens.lens (\StreamJournalToKinesis' {kinesisConfiguration} -> kinesisConfiguration) (\s@StreamJournalToKinesis' {} a -> s {kinesisConfiguration = a} :: StreamJournalToKinesis)

-- | The name that you want to assign to the QLDB journal stream.
-- User-defined names can help identify and indicate the purpose of a
-- stream.
--
-- Your stream name must be unique among other /active/ streams for a given
-- ledger. Stream names have the same naming constraints as ledger names,
-- as defined in
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/limits.html#limits.naming Quotas in Amazon QLDB>
-- in the /Amazon QLDB Developer Guide/.
streamJournalToKinesis_streamName :: Lens.Lens' StreamJournalToKinesis Prelude.Text
streamJournalToKinesis_streamName = Lens.lens (\StreamJournalToKinesis' {streamName} -> streamName) (\s@StreamJournalToKinesis' {} a -> s {streamName = a} :: StreamJournalToKinesis)

instance Core.AWSRequest StreamJournalToKinesis where
  type
    AWSResponse StreamJournalToKinesis =
      StreamJournalToKinesisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StreamJournalToKinesisResponse'
            Prelude.<$> (x Data..?> "StreamId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StreamJournalToKinesis where
  hashWithSalt _salt StreamJournalToKinesis' {..} =
    _salt
      `Prelude.hashWithSalt` exclusiveEndTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` ledgerName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` inclusiveStartTime
      `Prelude.hashWithSalt` kinesisConfiguration
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData StreamJournalToKinesis where
  rnf StreamJournalToKinesis' {..} =
    Prelude.rnf exclusiveEndTime `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf ledgerName `Prelude.seq`
          Prelude.rnf roleArn `Prelude.seq`
            Prelude.rnf inclusiveStartTime `Prelude.seq`
              Prelude.rnf kinesisConfiguration `Prelude.seq`
                Prelude.rnf streamName

instance Data.ToHeaders StreamJournalToKinesis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StreamJournalToKinesis where
  toJSON StreamJournalToKinesis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExclusiveEndTime" Data..=)
              Prelude.<$> exclusiveEndTime,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just
              ("InclusiveStartTime" Data..= inclusiveStartTime),
            Prelude.Just
              ( "KinesisConfiguration"
                  Data..= kinesisConfiguration
              ),
            Prelude.Just ("StreamName" Data..= streamName)
          ]
      )

instance Data.ToPath StreamJournalToKinesis where
  toPath StreamJournalToKinesis' {..} =
    Prelude.mconcat
      [ "/ledgers/",
        Data.toBS ledgerName,
        "/journal-kinesis-streams"
      ]

instance Data.ToQuery StreamJournalToKinesis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStreamJournalToKinesisResponse' smart constructor.
data StreamJournalToKinesisResponse = StreamJournalToKinesisResponse'
  { -- | The UUID (represented in Base62-encoded text) that QLDB assigns to each
    -- QLDB journal stream.
    streamId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StreamJournalToKinesisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamId', 'streamJournalToKinesisResponse_streamId' - The UUID (represented in Base62-encoded text) that QLDB assigns to each
-- QLDB journal stream.
--
-- 'httpStatus', 'streamJournalToKinesisResponse_httpStatus' - The response's http status code.
newStreamJournalToKinesisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StreamJournalToKinesisResponse
newStreamJournalToKinesisResponse pHttpStatus_ =
  StreamJournalToKinesisResponse'
    { streamId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The UUID (represented in Base62-encoded text) that QLDB assigns to each
-- QLDB journal stream.
streamJournalToKinesisResponse_streamId :: Lens.Lens' StreamJournalToKinesisResponse (Prelude.Maybe Prelude.Text)
streamJournalToKinesisResponse_streamId = Lens.lens (\StreamJournalToKinesisResponse' {streamId} -> streamId) (\s@StreamJournalToKinesisResponse' {} a -> s {streamId = a} :: StreamJournalToKinesisResponse)

-- | The response's http status code.
streamJournalToKinesisResponse_httpStatus :: Lens.Lens' StreamJournalToKinesisResponse Prelude.Int
streamJournalToKinesisResponse_httpStatus = Lens.lens (\StreamJournalToKinesisResponse' {httpStatus} -> httpStatus) (\s@StreamJournalToKinesisResponse' {} a -> s {httpStatus = a} :: StreamJournalToKinesisResponse)

instance
  Prelude.NFData
    StreamJournalToKinesisResponse
  where
  rnf StreamJournalToKinesisResponse' {..} =
    Prelude.rnf streamId `Prelude.seq`
      Prelude.rnf httpStatus
