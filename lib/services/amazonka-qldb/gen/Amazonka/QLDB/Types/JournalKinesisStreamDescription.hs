{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QLDB.Types.JournalKinesisStreamDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.JournalKinesisStreamDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types.ErrorCause
import Amazonka.QLDB.Types.KinesisConfiguration
import Amazonka.QLDB.Types.StreamStatus

-- | Information about an Amazon QLDB journal stream, including the Amazon
-- Resource Name (ARN), stream name, creation time, current status, and the
-- parameters of the original stream creation request.
--
-- /See:/ 'newJournalKinesisStreamDescription' smart constructor.
data JournalKinesisStreamDescription = JournalKinesisStreamDescription'
  { -- | The error message that describes the reason that a stream has a status
    -- of @IMPAIRED@ or @FAILED@. This is not applicable to streams that have
    -- other status values.
    errorCause :: Prelude.Maybe ErrorCause,
    -- | The inclusive start date and time from which to start streaming journal
    -- data.
    inclusiveStartTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the QLDB journal stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The exclusive date and time that specifies when the stream ends. If this
    -- parameter is undefined, the stream runs indefinitely until you cancel
    -- it.
    exclusiveEndTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time, in epoch time format, when the QLDB journal stream
    -- was created. (Epoch time format is the number of seconds elapsed since
    -- 12:00:00 AM January 1, 1970 UTC.)
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the ledger.
    ledgerName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
    -- permissions for a journal stream to write data records to a Kinesis Data
    -- Streams resource.
    roleArn :: Prelude.Text,
    -- | The UUID (represented in Base62-encoded text) of the QLDB journal
    -- stream.
    streamId :: Prelude.Text,
    -- | The current state of the QLDB journal stream.
    status :: StreamStatus,
    -- | The configuration settings of the Amazon Kinesis Data Streams
    -- destination for a QLDB journal stream.
    kinesisConfiguration :: KinesisConfiguration,
    -- | The user-defined name of the QLDB journal stream.
    streamName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JournalKinesisStreamDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCause', 'journalKinesisStreamDescription_errorCause' - The error message that describes the reason that a stream has a status
-- of @IMPAIRED@ or @FAILED@. This is not applicable to streams that have
-- other status values.
--
-- 'inclusiveStartTime', 'journalKinesisStreamDescription_inclusiveStartTime' - The inclusive start date and time from which to start streaming journal
-- data.
--
-- 'arn', 'journalKinesisStreamDescription_arn' - The Amazon Resource Name (ARN) of the QLDB journal stream.
--
-- 'exclusiveEndTime', 'journalKinesisStreamDescription_exclusiveEndTime' - The exclusive date and time that specifies when the stream ends. If this
-- parameter is undefined, the stream runs indefinitely until you cancel
-- it.
--
-- 'creationTime', 'journalKinesisStreamDescription_creationTime' - The date and time, in epoch time format, when the QLDB journal stream
-- was created. (Epoch time format is the number of seconds elapsed since
-- 12:00:00 AM January 1, 1970 UTC.)
--
-- 'ledgerName', 'journalKinesisStreamDescription_ledgerName' - The name of the ledger.
--
-- 'roleArn', 'journalKinesisStreamDescription_roleArn' - The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal stream to write data records to a Kinesis Data
-- Streams resource.
--
-- 'streamId', 'journalKinesisStreamDescription_streamId' - The UUID (represented in Base62-encoded text) of the QLDB journal
-- stream.
--
-- 'status', 'journalKinesisStreamDescription_status' - The current state of the QLDB journal stream.
--
-- 'kinesisConfiguration', 'journalKinesisStreamDescription_kinesisConfiguration' - The configuration settings of the Amazon Kinesis Data Streams
-- destination for a QLDB journal stream.
--
-- 'streamName', 'journalKinesisStreamDescription_streamName' - The user-defined name of the QLDB journal stream.
newJournalKinesisStreamDescription ::
  -- | 'ledgerName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'streamId'
  Prelude.Text ->
  -- | 'status'
  StreamStatus ->
  -- | 'kinesisConfiguration'
  KinesisConfiguration ->
  -- | 'streamName'
  Prelude.Text ->
  JournalKinesisStreamDescription
newJournalKinesisStreamDescription
  pLedgerName_
  pRoleArn_
  pStreamId_
  pStatus_
  pKinesisConfiguration_
  pStreamName_ =
    JournalKinesisStreamDescription'
      { errorCause =
          Prelude.Nothing,
        inclusiveStartTime = Prelude.Nothing,
        arn = Prelude.Nothing,
        exclusiveEndTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        ledgerName = pLedgerName_,
        roleArn = pRoleArn_,
        streamId = pStreamId_,
        status = pStatus_,
        kinesisConfiguration =
          pKinesisConfiguration_,
        streamName = pStreamName_
      }

-- | The error message that describes the reason that a stream has a status
-- of @IMPAIRED@ or @FAILED@. This is not applicable to streams that have
-- other status values.
journalKinesisStreamDescription_errorCause :: Lens.Lens' JournalKinesisStreamDescription (Prelude.Maybe ErrorCause)
journalKinesisStreamDescription_errorCause = Lens.lens (\JournalKinesisStreamDescription' {errorCause} -> errorCause) (\s@JournalKinesisStreamDescription' {} a -> s {errorCause = a} :: JournalKinesisStreamDescription)

-- | The inclusive start date and time from which to start streaming journal
-- data.
journalKinesisStreamDescription_inclusiveStartTime :: Lens.Lens' JournalKinesisStreamDescription (Prelude.Maybe Prelude.UTCTime)
journalKinesisStreamDescription_inclusiveStartTime = Lens.lens (\JournalKinesisStreamDescription' {inclusiveStartTime} -> inclusiveStartTime) (\s@JournalKinesisStreamDescription' {} a -> s {inclusiveStartTime = a} :: JournalKinesisStreamDescription) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the QLDB journal stream.
journalKinesisStreamDescription_arn :: Lens.Lens' JournalKinesisStreamDescription (Prelude.Maybe Prelude.Text)
journalKinesisStreamDescription_arn = Lens.lens (\JournalKinesisStreamDescription' {arn} -> arn) (\s@JournalKinesisStreamDescription' {} a -> s {arn = a} :: JournalKinesisStreamDescription)

-- | The exclusive date and time that specifies when the stream ends. If this
-- parameter is undefined, the stream runs indefinitely until you cancel
-- it.
journalKinesisStreamDescription_exclusiveEndTime :: Lens.Lens' JournalKinesisStreamDescription (Prelude.Maybe Prelude.UTCTime)
journalKinesisStreamDescription_exclusiveEndTime = Lens.lens (\JournalKinesisStreamDescription' {exclusiveEndTime} -> exclusiveEndTime) (\s@JournalKinesisStreamDescription' {} a -> s {exclusiveEndTime = a} :: JournalKinesisStreamDescription) Prelude.. Lens.mapping Core._Time

-- | The date and time, in epoch time format, when the QLDB journal stream
-- was created. (Epoch time format is the number of seconds elapsed since
-- 12:00:00 AM January 1, 1970 UTC.)
journalKinesisStreamDescription_creationTime :: Lens.Lens' JournalKinesisStreamDescription (Prelude.Maybe Prelude.UTCTime)
journalKinesisStreamDescription_creationTime = Lens.lens (\JournalKinesisStreamDescription' {creationTime} -> creationTime) (\s@JournalKinesisStreamDescription' {} a -> s {creationTime = a} :: JournalKinesisStreamDescription) Prelude.. Lens.mapping Core._Time

-- | The name of the ledger.
journalKinesisStreamDescription_ledgerName :: Lens.Lens' JournalKinesisStreamDescription Prelude.Text
journalKinesisStreamDescription_ledgerName = Lens.lens (\JournalKinesisStreamDescription' {ledgerName} -> ledgerName) (\s@JournalKinesisStreamDescription' {} a -> s {ledgerName = a} :: JournalKinesisStreamDescription)

-- | The Amazon Resource Name (ARN) of the IAM role that grants QLDB
-- permissions for a journal stream to write data records to a Kinesis Data
-- Streams resource.
journalKinesisStreamDescription_roleArn :: Lens.Lens' JournalKinesisStreamDescription Prelude.Text
journalKinesisStreamDescription_roleArn = Lens.lens (\JournalKinesisStreamDescription' {roleArn} -> roleArn) (\s@JournalKinesisStreamDescription' {} a -> s {roleArn = a} :: JournalKinesisStreamDescription)

-- | The UUID (represented in Base62-encoded text) of the QLDB journal
-- stream.
journalKinesisStreamDescription_streamId :: Lens.Lens' JournalKinesisStreamDescription Prelude.Text
journalKinesisStreamDescription_streamId = Lens.lens (\JournalKinesisStreamDescription' {streamId} -> streamId) (\s@JournalKinesisStreamDescription' {} a -> s {streamId = a} :: JournalKinesisStreamDescription)

-- | The current state of the QLDB journal stream.
journalKinesisStreamDescription_status :: Lens.Lens' JournalKinesisStreamDescription StreamStatus
journalKinesisStreamDescription_status = Lens.lens (\JournalKinesisStreamDescription' {status} -> status) (\s@JournalKinesisStreamDescription' {} a -> s {status = a} :: JournalKinesisStreamDescription)

-- | The configuration settings of the Amazon Kinesis Data Streams
-- destination for a QLDB journal stream.
journalKinesisStreamDescription_kinesisConfiguration :: Lens.Lens' JournalKinesisStreamDescription KinesisConfiguration
journalKinesisStreamDescription_kinesisConfiguration = Lens.lens (\JournalKinesisStreamDescription' {kinesisConfiguration} -> kinesisConfiguration) (\s@JournalKinesisStreamDescription' {} a -> s {kinesisConfiguration = a} :: JournalKinesisStreamDescription)

-- | The user-defined name of the QLDB journal stream.
journalKinesisStreamDescription_streamName :: Lens.Lens' JournalKinesisStreamDescription Prelude.Text
journalKinesisStreamDescription_streamName = Lens.lens (\JournalKinesisStreamDescription' {streamName} -> streamName) (\s@JournalKinesisStreamDescription' {} a -> s {streamName = a} :: JournalKinesisStreamDescription)

instance
  Core.FromJSON
    JournalKinesisStreamDescription
  where
  parseJSON =
    Core.withObject
      "JournalKinesisStreamDescription"
      ( \x ->
          JournalKinesisStreamDescription'
            Prelude.<$> (x Core..:? "ErrorCause")
            Prelude.<*> (x Core..:? "InclusiveStartTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "ExclusiveEndTime")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..: "LedgerName")
            Prelude.<*> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "StreamId")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "KinesisConfiguration")
            Prelude.<*> (x Core..: "StreamName")
      )

instance
  Prelude.Hashable
    JournalKinesisStreamDescription
  where
  hashWithSalt
    _salt
    JournalKinesisStreamDescription' {..} =
      _salt `Prelude.hashWithSalt` errorCause
        `Prelude.hashWithSalt` inclusiveStartTime
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` exclusiveEndTime
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` ledgerName
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` streamId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` kinesisConfiguration
        `Prelude.hashWithSalt` streamName

instance
  Prelude.NFData
    JournalKinesisStreamDescription
  where
  rnf JournalKinesisStreamDescription' {..} =
    Prelude.rnf errorCause
      `Prelude.seq` Prelude.rnf inclusiveStartTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf exclusiveEndTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf ledgerName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf streamId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf kinesisConfiguration
      `Prelude.seq` Prelude.rnf streamName
