{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsLogStream
  ( CloudWatchLogsLogStream (..),

    -- * Smart constructor
    mkCloudWatchLogsLogStream,

    -- * Lenses
    cwllsBatchCount,
    cwllsBatchSize,
    cwllsBufferDuration,
    cwllsDatetimeFormat,
    cwllsEncoding,
    cwllsFile,
    cwllsFileFingerprintLines,
    cwllsInitialPosition,
    cwllsLogGroupName,
    cwllsMultiLineStartPattern,
    cwllsTimeZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding as Types
import qualified Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition as Types
import qualified Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone as Types
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon CloudWatch logs configuration for a layer. For detailed information about members of this data type, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
--
-- /See:/ 'mkCloudWatchLogsLogStream' smart constructor.
data CloudWatchLogsLogStream = CloudWatchLogsLogStream'
  { -- | Specifies the max number of log events in a batch, up to 10000. The default value is 1000.
    batchCount :: Core.Maybe Core.Int,
    -- | Specifies the maximum size of log events in a batch, in bytes, up to 1048576 bytes. The default value is 32768 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
    batchSize :: Core.Maybe Core.Int,
    -- | Specifies the time duration for the batching of log events. The minimum value is 5000ms and default value is 5000ms.
    bufferDuration :: Core.Maybe Core.Int,
    -- | Specifies how the time stamp is extracted from logs. For more information, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
    datetimeFormat :: Core.Maybe Types.String,
    -- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
    encoding :: Core.Maybe Types.CloudWatchLogsEncoding,
    -- | Specifies log files that you want to push to CloudWatch Logs.
    --
    -- @File@ can point to a specific file or multiple files (by using wild card characters such as @/var/log/system.log*@ ). Only the latest file is pushed to CloudWatch Logs, based on file modification time. We recommend that you use wild card characters to specify a series of files of the same type, such as @access_log.2014-06-01-01@ , @access_log.2014-06-01-02@ , and so on by using a pattern like @access_log.*@ . Don't use a wildcard to match multiple file types, such as @access_log_80@ and @access_log_443@ . To specify multiple, different file types, add another log stream entry to the configuration file, so that each log file type is stored in a different log group.
    -- Zipped files are not supported.
    file :: Core.Maybe Types.String,
    -- | Specifies the range of lines for identifying a file. The valid values are one number, or two dash-delimited numbers, such as '1', '2-5'. The default value is '1', meaning the first line is used to calculate the fingerprint. Fingerprint lines are not sent to CloudWatch Logs unless all specified lines are available.
    fileFingerprintLines :: Core.Maybe Types.String,
    -- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. This setting is only used if there is no state persisted for that log stream.
    initialPosition :: Core.Maybe Types.CloudWatchLogsInitialPosition,
    -- | Specifies the destination log group. A log group is created automatically if it doesn't already exist. Log group names can be between 1 and 512 characters long. Allowed characters include a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
    logGroupName :: Core.Maybe Types.String,
    -- | Specifies the pattern for identifying the start of a log message.
    multiLineStartPattern :: Core.Maybe Types.String,
    -- | Specifies the time zone of log event time stamps.
    timeZone :: Core.Maybe Types.CloudWatchLogsTimeZone
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchLogsLogStream' value with any optional fields omitted.
mkCloudWatchLogsLogStream ::
  CloudWatchLogsLogStream
mkCloudWatchLogsLogStream =
  CloudWatchLogsLogStream'
    { batchCount = Core.Nothing,
      batchSize = Core.Nothing,
      bufferDuration = Core.Nothing,
      datetimeFormat = Core.Nothing,
      encoding = Core.Nothing,
      file = Core.Nothing,
      fileFingerprintLines = Core.Nothing,
      initialPosition = Core.Nothing,
      logGroupName = Core.Nothing,
      multiLineStartPattern = Core.Nothing,
      timeZone = Core.Nothing
    }

-- | Specifies the max number of log events in a batch, up to 10000. The default value is 1000.
--
-- /Note:/ Consider using 'batchCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsBatchCount :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Core.Int)
cwllsBatchCount = Lens.field @"batchCount"
{-# DEPRECATED cwllsBatchCount "Use generic-lens or generic-optics with 'batchCount' instead." #-}

-- | Specifies the maximum size of log events in a batch, in bytes, up to 1048576 bytes. The default value is 32768 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsBatchSize :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Core.Int)
cwllsBatchSize = Lens.field @"batchSize"
{-# DEPRECATED cwllsBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | Specifies the time duration for the batching of log events. The minimum value is 5000ms and default value is 5000ms.
--
-- /Note:/ Consider using 'bufferDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsBufferDuration :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Core.Int)
cwllsBufferDuration = Lens.field @"bufferDuration"
{-# DEPRECATED cwllsBufferDuration "Use generic-lens or generic-optics with 'bufferDuration' instead." #-}

-- | Specifies how the time stamp is extracted from logs. For more information, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
--
-- /Note:/ Consider using 'datetimeFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsDatetimeFormat :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.String)
cwllsDatetimeFormat = Lens.field @"datetimeFormat"
{-# DEPRECATED cwllsDatetimeFormat "Use generic-lens or generic-optics with 'datetimeFormat' instead." #-}

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
--
-- /Note:/ Consider using 'encoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsEncoding :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.CloudWatchLogsEncoding)
cwllsEncoding = Lens.field @"encoding"
{-# DEPRECATED cwllsEncoding "Use generic-lens or generic-optics with 'encoding' instead." #-}

-- | Specifies log files that you want to push to CloudWatch Logs.
--
-- @File@ can point to a specific file or multiple files (by using wild card characters such as @/var/log/system.log*@ ). Only the latest file is pushed to CloudWatch Logs, based on file modification time. We recommend that you use wild card characters to specify a series of files of the same type, such as @access_log.2014-06-01-01@ , @access_log.2014-06-01-02@ , and so on by using a pattern like @access_log.*@ . Don't use a wildcard to match multiple file types, such as @access_log_80@ and @access_log_443@ . To specify multiple, different file types, add another log stream entry to the configuration file, so that each log file type is stored in a different log group.
-- Zipped files are not supported.
--
-- /Note:/ Consider using 'file' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsFile :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.String)
cwllsFile = Lens.field @"file"
{-# DEPRECATED cwllsFile "Use generic-lens or generic-optics with 'file' instead." #-}

-- | Specifies the range of lines for identifying a file. The valid values are one number, or two dash-delimited numbers, such as '1', '2-5'. The default value is '1', meaning the first line is used to calculate the fingerprint. Fingerprint lines are not sent to CloudWatch Logs unless all specified lines are available.
--
-- /Note:/ Consider using 'fileFingerprintLines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsFileFingerprintLines :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.String)
cwllsFileFingerprintLines = Lens.field @"fileFingerprintLines"
{-# DEPRECATED cwllsFileFingerprintLines "Use generic-lens or generic-optics with 'fileFingerprintLines' instead." #-}

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. This setting is only used if there is no state persisted for that log stream.
--
-- /Note:/ Consider using 'initialPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsInitialPosition :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.CloudWatchLogsInitialPosition)
cwllsInitialPosition = Lens.field @"initialPosition"
{-# DEPRECATED cwllsInitialPosition "Use generic-lens or generic-optics with 'initialPosition' instead." #-}

-- | Specifies the destination log group. A log group is created automatically if it doesn't already exist. Log group names can be between 1 and 512 characters long. Allowed characters include a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsLogGroupName :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.String)
cwllsLogGroupName = Lens.field @"logGroupName"
{-# DEPRECATED cwllsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Specifies the pattern for identifying the start of a log message.
--
-- /Note:/ Consider using 'multiLineStartPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsMultiLineStartPattern :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.String)
cwllsMultiLineStartPattern = Lens.field @"multiLineStartPattern"
{-# DEPRECATED cwllsMultiLineStartPattern "Use generic-lens or generic-optics with 'multiLineStartPattern' instead." #-}

-- | Specifies the time zone of log event time stamps.
--
-- /Note:/ Consider using 'timeZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsTimeZone :: Lens.Lens' CloudWatchLogsLogStream (Core.Maybe Types.CloudWatchLogsTimeZone)
cwllsTimeZone = Lens.field @"timeZone"
{-# DEPRECATED cwllsTimeZone "Use generic-lens or generic-optics with 'timeZone' instead." #-}

instance Core.FromJSON CloudWatchLogsLogStream where
  toJSON CloudWatchLogsLogStream {..} =
    Core.object
      ( Core.catMaybes
          [ ("BatchCount" Core..=) Core.<$> batchCount,
            ("BatchSize" Core..=) Core.<$> batchSize,
            ("BufferDuration" Core..=) Core.<$> bufferDuration,
            ("DatetimeFormat" Core..=) Core.<$> datetimeFormat,
            ("Encoding" Core..=) Core.<$> encoding,
            ("File" Core..=) Core.<$> file,
            ("FileFingerprintLines" Core..=) Core.<$> fileFingerprintLines,
            ("InitialPosition" Core..=) Core.<$> initialPosition,
            ("LogGroupName" Core..=) Core.<$> logGroupName,
            ("MultiLineStartPattern" Core..=) Core.<$> multiLineStartPattern,
            ("TimeZone" Core..=) Core.<$> timeZone
          ]
      )

instance Core.FromJSON CloudWatchLogsLogStream where
  parseJSON =
    Core.withObject "CloudWatchLogsLogStream" Core.$
      \x ->
        CloudWatchLogsLogStream'
          Core.<$> (x Core..:? "BatchCount")
          Core.<*> (x Core..:? "BatchSize")
          Core.<*> (x Core..:? "BufferDuration")
          Core.<*> (x Core..:? "DatetimeFormat")
          Core.<*> (x Core..:? "Encoding")
          Core.<*> (x Core..:? "File")
          Core.<*> (x Core..:? "FileFingerprintLines")
          Core.<*> (x Core..:? "InitialPosition")
          Core.<*> (x Core..:? "LogGroupName")
          Core.<*> (x Core..:? "MultiLineStartPattern")
          Core.<*> (x Core..:? "TimeZone")
