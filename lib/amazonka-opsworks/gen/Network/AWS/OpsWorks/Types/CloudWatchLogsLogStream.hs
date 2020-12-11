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
    cwllsFileFingerprintLines,
    cwllsBufferDuration,
    cwllsBatchSize,
    cwllsLogGroupName,
    cwllsMultiLineStartPattern,
    cwllsInitialPosition,
    cwllsDatetimeFormat,
    cwllsEncoding,
    cwllsTimeZone,
    cwllsFile,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.CloudWatchLogsEncoding
import Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
import Network.AWS.OpsWorks.Types.CloudWatchLogsTimeZone
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon CloudWatch logs configuration for a layer. For detailed information about members of this data type, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
--
-- /See:/ 'mkCloudWatchLogsLogStream' smart constructor.
data CloudWatchLogsLogStream = CloudWatchLogsLogStream'
  { batchCount ::
      Lude.Maybe Lude.Int,
    fileFingerprintLines ::
      Lude.Maybe Lude.Text,
    bufferDuration :: Lude.Maybe Lude.Int,
    batchSize :: Lude.Maybe Lude.Int,
    logGroupName :: Lude.Maybe Lude.Text,
    multiLineStartPattern ::
      Lude.Maybe Lude.Text,
    initialPosition ::
      Lude.Maybe CloudWatchLogsInitialPosition,
    datetimeFormat :: Lude.Maybe Lude.Text,
    encoding ::
      Lude.Maybe CloudWatchLogsEncoding,
    timeZone ::
      Lude.Maybe CloudWatchLogsTimeZone,
    file :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CloudWatchLogsLogStream' with the minimum fields required to make a request.
--
-- * 'batchCount' - Specifies the max number of log events in a batch, up to 10000. The default value is 1000.
-- * 'batchSize' - Specifies the maximum size of log events in a batch, in bytes, up to 1048576 bytes. The default value is 32768 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
-- * 'bufferDuration' - Specifies the time duration for the batching of log events. The minimum value is 5000ms and default value is 5000ms.
-- * 'datetimeFormat' - Specifies how the time stamp is extracted from logs. For more information, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
-- * 'encoding' - Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
-- * 'file' - Specifies log files that you want to push to CloudWatch Logs.
--
-- @File@ can point to a specific file or multiple files (by using wild card characters such as @/var/log/system.log*@ ). Only the latest file is pushed to CloudWatch Logs, based on file modification time. We recommend that you use wild card characters to specify a series of files of the same type, such as @access_log.2014-06-01-01@ , @access_log.2014-06-01-02@ , and so on by using a pattern like @access_log.*@ . Don't use a wildcard to match multiple file types, such as @access_log_80@ and @access_log_443@ . To specify multiple, different file types, add another log stream entry to the configuration file, so that each log file type is stored in a different log group.
-- Zipped files are not supported.
-- * 'fileFingerprintLines' - Specifies the range of lines for identifying a file. The valid values are one number, or two dash-delimited numbers, such as '1', '2-5'. The default value is '1', meaning the first line is used to calculate the fingerprint. Fingerprint lines are not sent to CloudWatch Logs unless all specified lines are available.
-- * 'initialPosition' - Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. This setting is only used if there is no state persisted for that log stream.
-- * 'logGroupName' - Specifies the destination log group. A log group is created automatically if it doesn't already exist. Log group names can be between 1 and 512 characters long. Allowed characters include a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
-- * 'multiLineStartPattern' - Specifies the pattern for identifying the start of a log message.
-- * 'timeZone' - Specifies the time zone of log event time stamps.
mkCloudWatchLogsLogStream ::
  CloudWatchLogsLogStream
mkCloudWatchLogsLogStream =
  CloudWatchLogsLogStream'
    { batchCount = Lude.Nothing,
      fileFingerprintLines = Lude.Nothing,
      bufferDuration = Lude.Nothing,
      batchSize = Lude.Nothing,
      logGroupName = Lude.Nothing,
      multiLineStartPattern = Lude.Nothing,
      initialPosition = Lude.Nothing,
      datetimeFormat = Lude.Nothing,
      encoding = Lude.Nothing,
      timeZone = Lude.Nothing,
      file = Lude.Nothing
    }

-- | Specifies the max number of log events in a batch, up to 10000. The default value is 1000.
--
-- /Note:/ Consider using 'batchCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsBatchCount :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Int)
cwllsBatchCount = Lens.lens (batchCount :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Int) (\s a -> s {batchCount = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsBatchCount "Use generic-lens or generic-optics with 'batchCount' instead." #-}

-- | Specifies the range of lines for identifying a file. The valid values are one number, or two dash-delimited numbers, such as '1', '2-5'. The default value is '1', meaning the first line is used to calculate the fingerprint. Fingerprint lines are not sent to CloudWatch Logs unless all specified lines are available.
--
-- /Note:/ Consider using 'fileFingerprintLines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsFileFingerprintLines :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Text)
cwllsFileFingerprintLines = Lens.lens (fileFingerprintLines :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Text) (\s a -> s {fileFingerprintLines = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsFileFingerprintLines "Use generic-lens or generic-optics with 'fileFingerprintLines' instead." #-}

-- | Specifies the time duration for the batching of log events. The minimum value is 5000ms and default value is 5000ms.
--
-- /Note:/ Consider using 'bufferDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsBufferDuration :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Int)
cwllsBufferDuration = Lens.lens (bufferDuration :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Int) (\s a -> s {bufferDuration = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsBufferDuration "Use generic-lens or generic-optics with 'bufferDuration' instead." #-}

-- | Specifies the maximum size of log events in a batch, in bytes, up to 1048576 bytes. The default value is 32768 bytes. This size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsBatchSize :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Int)
cwllsBatchSize = Lens.lens (batchSize :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Int) (\s a -> s {batchSize = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | Specifies the destination log group. A log group is created automatically if it doesn't already exist. Log group names can be between 1 and 512 characters long. Allowed characters include a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsLogGroupName :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Text)
cwllsLogGroupName = Lens.lens (logGroupName :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | Specifies the pattern for identifying the start of a log message.
--
-- /Note:/ Consider using 'multiLineStartPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsMultiLineStartPattern :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Text)
cwllsMultiLineStartPattern = Lens.lens (multiLineStartPattern :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Text) (\s a -> s {multiLineStartPattern = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsMultiLineStartPattern "Use generic-lens or generic-optics with 'multiLineStartPattern' instead." #-}

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. This setting is only used if there is no state persisted for that log stream.
--
-- /Note:/ Consider using 'initialPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsInitialPosition :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe CloudWatchLogsInitialPosition)
cwllsInitialPosition = Lens.lens (initialPosition :: CloudWatchLogsLogStream -> Lude.Maybe CloudWatchLogsInitialPosition) (\s a -> s {initialPosition = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsInitialPosition "Use generic-lens or generic-optics with 'initialPosition' instead." #-}

-- | Specifies how the time stamp is extracted from logs. For more information, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference> .
--
-- /Note:/ Consider using 'datetimeFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsDatetimeFormat :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Text)
cwllsDatetimeFormat = Lens.lens (datetimeFormat :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Text) (\s a -> s {datetimeFormat = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsDatetimeFormat "Use generic-lens or generic-optics with 'datetimeFormat' instead." #-}

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
--
-- /Note:/ Consider using 'encoding' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsEncoding :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe CloudWatchLogsEncoding)
cwllsEncoding = Lens.lens (encoding :: CloudWatchLogsLogStream -> Lude.Maybe CloudWatchLogsEncoding) (\s a -> s {encoding = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsEncoding "Use generic-lens or generic-optics with 'encoding' instead." #-}

-- | Specifies the time zone of log event time stamps.
--
-- /Note:/ Consider using 'timeZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsTimeZone :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe CloudWatchLogsTimeZone)
cwllsTimeZone = Lens.lens (timeZone :: CloudWatchLogsLogStream -> Lude.Maybe CloudWatchLogsTimeZone) (\s a -> s {timeZone = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsTimeZone "Use generic-lens or generic-optics with 'timeZone' instead." #-}

-- | Specifies log files that you want to push to CloudWatch Logs.
--
-- @File@ can point to a specific file or multiple files (by using wild card characters such as @/var/log/system.log*@ ). Only the latest file is pushed to CloudWatch Logs, based on file modification time. We recommend that you use wild card characters to specify a series of files of the same type, such as @access_log.2014-06-01-01@ , @access_log.2014-06-01-02@ , and so on by using a pattern like @access_log.*@ . Don't use a wildcard to match multiple file types, such as @access_log_80@ and @access_log_443@ . To specify multiple, different file types, add another log stream entry to the configuration file, so that each log file type is stored in a different log group.
-- Zipped files are not supported.
--
-- /Note:/ Consider using 'file' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwllsFile :: Lens.Lens' CloudWatchLogsLogStream (Lude.Maybe Lude.Text)
cwllsFile = Lens.lens (file :: CloudWatchLogsLogStream -> Lude.Maybe Lude.Text) (\s a -> s {file = a} :: CloudWatchLogsLogStream)
{-# DEPRECATED cwllsFile "Use generic-lens or generic-optics with 'file' instead." #-}

instance Lude.FromJSON CloudWatchLogsLogStream where
  parseJSON =
    Lude.withObject
      "CloudWatchLogsLogStream"
      ( \x ->
          CloudWatchLogsLogStream'
            Lude.<$> (x Lude..:? "BatchCount")
            Lude.<*> (x Lude..:? "FileFingerprintLines")
            Lude.<*> (x Lude..:? "BufferDuration")
            Lude.<*> (x Lude..:? "BatchSize")
            Lude.<*> (x Lude..:? "LogGroupName")
            Lude.<*> (x Lude..:? "MultiLineStartPattern")
            Lude.<*> (x Lude..:? "InitialPosition")
            Lude.<*> (x Lude..:? "DatetimeFormat")
            Lude.<*> (x Lude..:? "Encoding")
            Lude.<*> (x Lude..:? "TimeZone")
            Lude.<*> (x Lude..:? "File")
      )

instance Lude.ToJSON CloudWatchLogsLogStream where
  toJSON CloudWatchLogsLogStream' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BatchCount" Lude..=) Lude.<$> batchCount,
            ("FileFingerprintLines" Lude..=) Lude.<$> fileFingerprintLines,
            ("BufferDuration" Lude..=) Lude.<$> bufferDuration,
            ("BatchSize" Lude..=) Lude.<$> batchSize,
            ("LogGroupName" Lude..=) Lude.<$> logGroupName,
            ("MultiLineStartPattern" Lude..=) Lude.<$> multiLineStartPattern,
            ("InitialPosition" Lude..=) Lude.<$> initialPosition,
            ("DatetimeFormat" Lude..=) Lude.<$> datetimeFormat,
            ("Encoding" Lude..=) Lude.<$> encoding,
            ("TimeZone" Lude..=) Lude.<$> timeZone,
            ("File" Lude..=) Lude.<$> file
          ]
      )
