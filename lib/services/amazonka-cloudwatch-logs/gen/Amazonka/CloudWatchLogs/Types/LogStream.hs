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
-- Module      : Amazonka.CloudWatchLogs.Types.LogStream
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.LogStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a log stream, which is a sequence of log events from a single
-- emitter of logs.
--
-- /See:/ 'newLogStream' smart constructor.
data LogStream = LogStream'
  { -- | The Amazon Resource Name (ARN) of the log stream.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the stream, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | The time of the first event, expressed as the number of milliseconds
    -- after @Jan 1, 1970 00:00:00 UTC@.
    firstEventTimestamp :: Prelude.Maybe Prelude.Natural,
    -- | The time of the most recent log event in the log stream in CloudWatch
    -- Logs. This number is expressed as the number of milliseconds after
    -- @Jan 1, 1970 00:00:00 UTC@. The @lastEventTime@ value updates on an
    -- eventual consistency basis. It typically updates in less than an hour
    -- from ingestion, but in rare situations might take longer.
    lastEventTimestamp :: Prelude.Maybe Prelude.Natural,
    -- | The ingestion time, expressed as the number of milliseconds after
    -- @Jan 1, 1970 00:00:00 UTC@ The @lastIngestionTime@ value updates on an
    -- eventual consistency basis. It typically updates in less than an hour
    -- after ingestion, but in rare situations might take longer.
    lastIngestionTime :: Prelude.Maybe Prelude.Natural,
    -- | The name of the log stream.
    logStreamName :: Prelude.Maybe Prelude.Text,
    -- | The number of bytes stored.
    --
    -- __Important:__ As of June 17, 2019, this parameter is no longer
    -- supported for log streams, and is always reported as zero. This change
    -- applies only to log streams. The @storedBytes@ parameter for log groups
    -- is not affected.
    storedBytes :: Prelude.Maybe Prelude.Natural,
    -- | The sequence token.
    --
    -- The sequence token is now ignored in @PutLogEvents@ actions.
    -- @PutLogEvents@ actions are always accepted regardless of receiving an
    -- invalid sequence token. You don\'t need to obtain @uploadSequenceToken@
    -- to use a @PutLogEvents@ action.
    uploadSequenceToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'logStream_arn' - The Amazon Resource Name (ARN) of the log stream.
--
-- 'creationTime', 'logStream_creationTime' - The creation time of the stream, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
--
-- 'firstEventTimestamp', 'logStream_firstEventTimestamp' - The time of the first event, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
--
-- 'lastEventTimestamp', 'logStream_lastEventTimestamp' - The time of the most recent log event in the log stream in CloudWatch
-- Logs. This number is expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@. The @lastEventTime@ value updates on an
-- eventual consistency basis. It typically updates in less than an hour
-- from ingestion, but in rare situations might take longer.
--
-- 'lastIngestionTime', 'logStream_lastIngestionTime' - The ingestion time, expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@ The @lastIngestionTime@ value updates on an
-- eventual consistency basis. It typically updates in less than an hour
-- after ingestion, but in rare situations might take longer.
--
-- 'logStreamName', 'logStream_logStreamName' - The name of the log stream.
--
-- 'storedBytes', 'logStream_storedBytes' - The number of bytes stored.
--
-- __Important:__ As of June 17, 2019, this parameter is no longer
-- supported for log streams, and is always reported as zero. This change
-- applies only to log streams. The @storedBytes@ parameter for log groups
-- is not affected.
--
-- 'uploadSequenceToken', 'logStream_uploadSequenceToken' - The sequence token.
--
-- The sequence token is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are always accepted regardless of receiving an
-- invalid sequence token. You don\'t need to obtain @uploadSequenceToken@
-- to use a @PutLogEvents@ action.
newLogStream ::
  LogStream
newLogStream =
  LogStream'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      firstEventTimestamp = Prelude.Nothing,
      lastEventTimestamp = Prelude.Nothing,
      lastIngestionTime = Prelude.Nothing,
      logStreamName = Prelude.Nothing,
      storedBytes = Prelude.Nothing,
      uploadSequenceToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the log stream.
logStream_arn :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Text)
logStream_arn = Lens.lens (\LogStream' {arn} -> arn) (\s@LogStream' {} a -> s {arn = a} :: LogStream)

-- | The creation time of the stream, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
logStream_creationTime :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Natural)
logStream_creationTime = Lens.lens (\LogStream' {creationTime} -> creationTime) (\s@LogStream' {} a -> s {creationTime = a} :: LogStream)

-- | The time of the first event, expressed as the number of milliseconds
-- after @Jan 1, 1970 00:00:00 UTC@.
logStream_firstEventTimestamp :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Natural)
logStream_firstEventTimestamp = Lens.lens (\LogStream' {firstEventTimestamp} -> firstEventTimestamp) (\s@LogStream' {} a -> s {firstEventTimestamp = a} :: LogStream)

-- | The time of the most recent log event in the log stream in CloudWatch
-- Logs. This number is expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@. The @lastEventTime@ value updates on an
-- eventual consistency basis. It typically updates in less than an hour
-- from ingestion, but in rare situations might take longer.
logStream_lastEventTimestamp :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Natural)
logStream_lastEventTimestamp = Lens.lens (\LogStream' {lastEventTimestamp} -> lastEventTimestamp) (\s@LogStream' {} a -> s {lastEventTimestamp = a} :: LogStream)

-- | The ingestion time, expressed as the number of milliseconds after
-- @Jan 1, 1970 00:00:00 UTC@ The @lastIngestionTime@ value updates on an
-- eventual consistency basis. It typically updates in less than an hour
-- after ingestion, but in rare situations might take longer.
logStream_lastIngestionTime :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Natural)
logStream_lastIngestionTime = Lens.lens (\LogStream' {lastIngestionTime} -> lastIngestionTime) (\s@LogStream' {} a -> s {lastIngestionTime = a} :: LogStream)

-- | The name of the log stream.
logStream_logStreamName :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Text)
logStream_logStreamName = Lens.lens (\LogStream' {logStreamName} -> logStreamName) (\s@LogStream' {} a -> s {logStreamName = a} :: LogStream)

-- | The number of bytes stored.
--
-- __Important:__ As of June 17, 2019, this parameter is no longer
-- supported for log streams, and is always reported as zero. This change
-- applies only to log streams. The @storedBytes@ parameter for log groups
-- is not affected.
logStream_storedBytes :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Natural)
logStream_storedBytes = Lens.lens (\LogStream' {storedBytes} -> storedBytes) (\s@LogStream' {} a -> s {storedBytes = a} :: LogStream)

-- | The sequence token.
--
-- The sequence token is now ignored in @PutLogEvents@ actions.
-- @PutLogEvents@ actions are always accepted regardless of receiving an
-- invalid sequence token. You don\'t need to obtain @uploadSequenceToken@
-- to use a @PutLogEvents@ action.
logStream_uploadSequenceToken :: Lens.Lens' LogStream (Prelude.Maybe Prelude.Text)
logStream_uploadSequenceToken = Lens.lens (\LogStream' {uploadSequenceToken} -> uploadSequenceToken) (\s@LogStream' {} a -> s {uploadSequenceToken = a} :: LogStream)

instance Data.FromJSON LogStream where
  parseJSON =
    Data.withObject
      "LogStream"
      ( \x ->
          LogStream'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "firstEventTimestamp")
            Prelude.<*> (x Data..:? "lastEventTimestamp")
            Prelude.<*> (x Data..:? "lastIngestionTime")
            Prelude.<*> (x Data..:? "logStreamName")
            Prelude.<*> (x Data..:? "storedBytes")
            Prelude.<*> (x Data..:? "uploadSequenceToken")
      )

instance Prelude.Hashable LogStream where
  hashWithSalt _salt LogStream' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` firstEventTimestamp
      `Prelude.hashWithSalt` lastEventTimestamp
      `Prelude.hashWithSalt` lastIngestionTime
      `Prelude.hashWithSalt` logStreamName
      `Prelude.hashWithSalt` storedBytes
      `Prelude.hashWithSalt` uploadSequenceToken

instance Prelude.NFData LogStream where
  rnf LogStream' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf firstEventTimestamp
      `Prelude.seq` Prelude.rnf lastEventTimestamp
      `Prelude.seq` Prelude.rnf lastIngestionTime
      `Prelude.seq` Prelude.rnf logStreamName
      `Prelude.seq` Prelude.rnf storedBytes
      `Prelude.seq` Prelude.rnf uploadSequenceToken
