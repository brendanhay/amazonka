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
    lsArn,
    lsCreationTime,
    lsFirstEventTimestamp,
    lsLastEventTimestamp,
    lsLastIngestionTime,
    lsLogStreamName,
    lsStoredBytes,
    lsUploadSequenceToken,
  )
where

import qualified Network.AWS.CloudWatchLogs.Types.Arn as Types
import qualified Network.AWS.CloudWatchLogs.Types.LogStreamName as Types
import qualified Network.AWS.CloudWatchLogs.Types.SequenceToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a log stream, which is a sequence of log events from a single emitter of logs.
--
-- /See:/ 'mkLogStream' smart constructor.
data LogStream = LogStream'
  { -- | The Amazon Resource Name (ARN) of the log stream.
    arn :: Core.Maybe Types.Arn,
    -- | The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural,
    -- | The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    firstEventTimestamp :: Core.Maybe Core.Natural,
    -- | The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
    lastEventTimestamp :: Core.Maybe Core.Natural,
    -- | The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
    lastIngestionTime :: Core.Maybe Core.Natural,
    -- | The name of the log stream.
    logStreamName :: Core.Maybe Types.LogStreamName,
    -- | The number of bytes stored.
    --
    -- __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
    storedBytes :: Core.Maybe Core.Natural,
    -- | The sequence token.
    uploadSequenceToken :: Core.Maybe Types.SequenceToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogStream' value with any optional fields omitted.
mkLogStream ::
  LogStream
mkLogStream =
  LogStream'
    { arn = Core.Nothing,
      creationTime = Core.Nothing,
      firstEventTimestamp = Core.Nothing,
      lastEventTimestamp = Core.Nothing,
      lastIngestionTime = Core.Nothing,
      logStreamName = Core.Nothing,
      storedBytes = Core.Nothing,
      uploadSequenceToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the log stream.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsArn :: Lens.Lens' LogStream (Core.Maybe Types.Arn)
lsArn = Lens.field @"arn"
{-# DEPRECATED lsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The creation time of the stream, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsCreationTime :: Lens.Lens' LogStream (Core.Maybe Core.Natural)
lsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED lsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time of the first event, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'firstEventTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsFirstEventTimestamp :: Lens.Lens' LogStream (Core.Maybe Core.Natural)
lsFirstEventTimestamp = Lens.field @"firstEventTimestamp"
{-# DEPRECATED lsFirstEventTimestamp "Use generic-lens or generic-optics with 'firstEventTimestamp' instead." #-}

-- | The time of the most recent log event in the log stream in CloudWatch Logs. This number is expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. The @lastEventTime@ value updates on an eventual consistency basis. It typically updates in less than an hour from ingestion, but in rare situations might take longer.
--
-- /Note:/ Consider using 'lastEventTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastEventTimestamp :: Lens.Lens' LogStream (Core.Maybe Core.Natural)
lsLastEventTimestamp = Lens.field @"lastEventTimestamp"
{-# DEPRECATED lsLastEventTimestamp "Use generic-lens or generic-optics with 'lastEventTimestamp' instead." #-}

-- | The ingestion time, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- /Note:/ Consider using 'lastIngestionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLastIngestionTime :: Lens.Lens' LogStream (Core.Maybe Core.Natural)
lsLastIngestionTime = Lens.field @"lastIngestionTime"
{-# DEPRECATED lsLastIngestionTime "Use generic-lens or generic-optics with 'lastIngestionTime' instead." #-}

-- | The name of the log stream.
--
-- /Note:/ Consider using 'logStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsLogStreamName :: Lens.Lens' LogStream (Core.Maybe Types.LogStreamName)
lsLogStreamName = Lens.field @"logStreamName"
{-# DEPRECATED lsLogStreamName "Use generic-lens or generic-optics with 'logStreamName' instead." #-}

-- | The number of bytes stored.
--
-- __Important:__ On June 17, 2019, this parameter was deprecated for log streams, and is always reported as zero. This change applies only to log streams. The @storedBytes@ parameter for log groups is not affected.
--
-- /Note:/ Consider using 'storedBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsStoredBytes :: Lens.Lens' LogStream (Core.Maybe Core.Natural)
lsStoredBytes = Lens.field @"storedBytes"
{-# DEPRECATED lsStoredBytes "Use generic-lens or generic-optics with 'storedBytes' instead." #-}

-- | The sequence token.
--
-- /Note:/ Consider using 'uploadSequenceToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsUploadSequenceToken :: Lens.Lens' LogStream (Core.Maybe Types.SequenceToken)
lsUploadSequenceToken = Lens.field @"uploadSequenceToken"
{-# DEPRECATED lsUploadSequenceToken "Use generic-lens or generic-optics with 'uploadSequenceToken' instead." #-}

instance Core.FromJSON LogStream where
  parseJSON =
    Core.withObject "LogStream" Core.$
      \x ->
        LogStream'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "creationTime")
          Core.<*> (x Core..:? "firstEventTimestamp")
          Core.<*> (x Core..:? "lastEventTimestamp")
          Core.<*> (x Core..:? "lastIngestionTime")
          Core.<*> (x Core..:? "logStreamName")
          Core.<*> (x Core..:? "storedBytes")
          Core.<*> (x Core..:? "uploadSequenceToken")
