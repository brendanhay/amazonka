{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.LogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.LogStream
  ( LogStream (..),

    -- * Smart constructor
    mkLogStream,

    -- * Lenses
    lsCreationTime,
    lsUploadSequenceToken,
    lsArn,
    lsFirstEventTimestamp,
    lsLogStreamName,
    lsStoredBytes,
    lsLastIngestionTime,
    lsLastEventTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a log stream, which is a sequence of log events from a single emitter of logs.
--
-- /See:/ 'mkLogStream' smart constructor.
data LogStream = LogStream'
  { -- | The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Lude.Maybe Lude.Natural,
    -- | The sequence token.
    uploadSequenceToken :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the log stream.
    arn :: Lude.Maybe Lude.Text,
    -- | The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    firstEventTimestamp :: Lude.Maybe Lude.Natural,
    -- | The name of the log stream.
    logStreamName :: Lude.Maybe Lude.Text,
    -- | The number of bytes stored.
    --
    -- __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
    storedBytes :: Lude.Maybe Lude.Natural,
    -- | The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    lastIngestionTime :: Lude.Maybe Lude.Natural,
    -- | The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
    lastEventTimestamp :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogStream' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'uploadSequenceToken' - The sequence token.
-- * 'arn' - The Amazon Resource Name (ARN) of the log stream.
-- * 'firstEventTimestamp' - The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'logStreamName' - The name of the log stream.
-- * 'storedBytes' - The number of bytes stored.
--
-- __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
-- * 'lastIngestionTime' - The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
-- * 'lastEventTimestamp' - The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
mkLogStream ::
  LogStream
mkLogStream =
  LogStream'
    { creationTime = Lude.Nothing,
      uploadSequenceToken = Lude.Nothing,
      arn = Lude.Nothing,
      firstEventTimestamp = Lude.Nothing,
      logStreamName = Lude.Nothing,
      storedBytes = Lude.Nothing,
      lastIngestionTime = Lude.Nothing,
      lastEventTimestamp = Lude.Nothing
    }

-- | The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsCreationTime :: Lens.Lens' LogStream (Lude.Maybe Lude.Natural)
lsCreationTime = Lens.lens (creationTime :: LogStream -> Lude.Maybe Lude.Natural) (\s a -> s {creationTime = a} :: LogStream)
{-# DEPRECATED lsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The sequence token.
--
-- /Note:/ Consider using 'uploadSequenceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsUploadSequenceToken :: Lens.Lens' LogStream (Lude.Maybe Lude.Text)
lsUploadSequenceToken = Lens.lens (uploadSequenceToken :: LogStream -> Lude.Maybe Lude.Text) (\s a -> s {uploadSequenceToken = a} :: LogStream)
{-# DEPRECATED lsUploadSequenceToken "Use generic-lens or generic-optics with 'uploadSequenceToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the log stream.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsArn :: Lens.Lens' LogStream (Lude.Maybe Lude.Text)
lsArn = Lens.lens (arn :: LogStream -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: LogStream)
{-# DEPRECATED lsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'firstEventTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsFirstEventTimestamp :: Lens.Lens' LogStream (Lude.Maybe Lude.Natural)
lsFirstEventTimestamp = Lens.lens (firstEventTimestamp :: LogStream -> Lude.Maybe Lude.Natural) (\s a -> s {firstEventTimestamp = a} :: LogStream)
{-# DEPRECATED lsFirstEventTimestamp "Use generic-lens or generic-optics with 'firstEventTimestamp' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLogStreamName :: Lens.Lens' LogStream (Lude.Maybe Lude.Text)
lsLogStreamName = Lens.lens (logStreamName :: LogStream -> Lude.Maybe Lude.Text) (\s a -> s {logStreamName = a} :: LogStream)
{-# DEPRECATED lsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The number of bytes stored.
--
-- __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
--
-- /Note:/ Consider using 'storedBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStoredBytes :: Lens.Lens' LogStream (Lude.Maybe Lude.Natural)
lsStoredBytes = Lens.lens (storedBytes :: LogStream -> Lude.Maybe Lude.Natural) (\s a -> s {storedBytes = a} :: LogStream)
{-# DEPRECATED lsStoredBytes "Use generic-lens or generic-optics with 'storedBytes' instead." #-}

-- | The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'lastIngestionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastIngestionTime :: Lens.Lens' LogStream (Lude.Maybe Lude.Natural)
lsLastIngestionTime = Lens.lens (lastIngestionTime :: LogStream -> Lude.Maybe Lude.Natural) (\s a -> s {lastIngestionTime = a} :: LogStream)
{-# DEPRECATED lsLastIngestionTime "Use generic-lens or generic-optics with 'lastIngestionTime' instead." #-}

-- | The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
--
-- /Note:/ Consider using 'lastEventTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastEventTimestamp :: Lens.Lens' LogStream (Lude.Maybe Lude.Natural)
lsLastEventTimestamp = Lens.lens (lastEventTimestamp :: LogStream -> Lude.Maybe Lude.Natural) (\s a -> s {lastEventTimestamp = a} :: LogStream)
{-# DEPRECATED lsLastEventTimestamp "Use generic-lens or generic-optics with 'lastEventTimestamp' instead." #-}

instance Lude.FromJSON LogStream where
  parseJSON =
    Lude.withObject
      "LogStream"
      ( \x ->
          LogStream'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "uploadSequenceToken")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "firstEventTimestamp")
            Lude.<*> (x Lude..:? "logStreamName")
            Lude.<*> (x Lude..:? "storedBytes")
            Lude.<*> (x Lude..:? "lastIngestionTime")
            Lude.<*> (x Lude..:? "lastEventTimestamp")
      )
