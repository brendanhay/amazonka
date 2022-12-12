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
-- Module      : Amazonka.OpsWorks.Types.CloudWatchLogsLogStream
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.CloudWatchLogsLogStream where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types.CloudWatchLogsEncoding
import Amazonka.OpsWorks.Types.CloudWatchLogsInitialPosition
import Amazonka.OpsWorks.Types.CloudWatchLogsTimeZone
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon CloudWatch logs configuration for a layer. For
-- detailed information about members of this data type, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference>.
--
-- /See:/ 'newCloudWatchLogsLogStream' smart constructor.
data CloudWatchLogsLogStream = CloudWatchLogsLogStream'
  { -- | Specifies the max number of log events in a batch, up to 10000. The
    -- default value is 1000.
    batchCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the maximum size of log events in a batch, in bytes, up to
    -- 1048576 bytes. The default value is 32768 bytes. This size is calculated
    -- as the sum of all event messages in UTF-8, plus 26 bytes for each log
    -- event.
    batchSize :: Prelude.Maybe Prelude.Int,
    -- | Specifies the time duration for the batching of log events. The minimum
    -- value is 5000ms and default value is 5000ms.
    bufferDuration :: Prelude.Maybe Prelude.Int,
    -- | Specifies how the time stamp is extracted from logs. For more
    -- information, see the
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference>.
    datetimeFormat :: Prelude.Maybe Prelude.Text,
    -- | Specifies the encoding of the log file so that the file can be read
    -- correctly. The default is @utf_8@. Encodings supported by Python
    -- @codecs.decode()@ can be used here.
    encoding :: Prelude.Maybe CloudWatchLogsEncoding,
    -- | Specifies log files that you want to push to CloudWatch Logs.
    --
    -- @File@ can point to a specific file or multiple files (by using wild
    -- card characters such as @\/var\/log\/system.log*@). Only the latest file
    -- is pushed to CloudWatch Logs, based on file modification time. We
    -- recommend that you use wild card characters to specify a series of files
    -- of the same type, such as @access_log.2014-06-01-01@,
    -- @access_log.2014-06-01-02@, and so on by using a pattern like
    -- @access_log.*@. Don\'t use a wildcard to match multiple file types, such
    -- as @access_log_80@ and @access_log_443@. To specify multiple, different
    -- file types, add another log stream entry to the configuration file, so
    -- that each log file type is stored in a different log group.
    --
    -- Zipped files are not supported.
    file :: Prelude.Maybe Prelude.Text,
    -- | Specifies the range of lines for identifying a file. The valid values
    -- are one number, or two dash-delimited numbers, such as \'1\', \'2-5\'.
    -- The default value is \'1\', meaning the first line is used to calculate
    -- the fingerprint. Fingerprint lines are not sent to CloudWatch Logs
    -- unless all specified lines are available.
    fileFingerprintLines :: Prelude.Maybe Prelude.Text,
    -- | Specifies where to start to read data (start_of_file or end_of_file).
    -- The default is start_of_file. This setting is only used if there is no
    -- state persisted for that log stream.
    initialPosition :: Prelude.Maybe CloudWatchLogsInitialPosition,
    -- | Specifies the destination log group. A log group is created
    -- automatically if it doesn\'t already exist. Log group names can be
    -- between 1 and 512 characters long. Allowed characters include a-z, A-Z,
    -- 0-9, \'_\' (underscore), \'-\' (hyphen), \'\/\' (forward slash), and
    -- \'.\' (period).
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the pattern for identifying the start of a log message.
    multiLineStartPattern :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time zone of log event time stamps.
    timeZone :: Prelude.Maybe CloudWatchLogsTimeZone
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CloudWatchLogsLogStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchCount', 'cloudWatchLogsLogStream_batchCount' - Specifies the max number of log events in a batch, up to 10000. The
-- default value is 1000.
--
-- 'batchSize', 'cloudWatchLogsLogStream_batchSize' - Specifies the maximum size of log events in a batch, in bytes, up to
-- 1048576 bytes. The default value is 32768 bytes. This size is calculated
-- as the sum of all event messages in UTF-8, plus 26 bytes for each log
-- event.
--
-- 'bufferDuration', 'cloudWatchLogsLogStream_bufferDuration' - Specifies the time duration for the batching of log events. The minimum
-- value is 5000ms and default value is 5000ms.
--
-- 'datetimeFormat', 'cloudWatchLogsLogStream_datetimeFormat' - Specifies how the time stamp is extracted from logs. For more
-- information, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference>.
--
-- 'encoding', 'cloudWatchLogsLogStream_encoding' - Specifies the encoding of the log file so that the file can be read
-- correctly. The default is @utf_8@. Encodings supported by Python
-- @codecs.decode()@ can be used here.
--
-- 'file', 'cloudWatchLogsLogStream_file' - Specifies log files that you want to push to CloudWatch Logs.
--
-- @File@ can point to a specific file or multiple files (by using wild
-- card characters such as @\/var\/log\/system.log*@). Only the latest file
-- is pushed to CloudWatch Logs, based on file modification time. We
-- recommend that you use wild card characters to specify a series of files
-- of the same type, such as @access_log.2014-06-01-01@,
-- @access_log.2014-06-01-02@, and so on by using a pattern like
-- @access_log.*@. Don\'t use a wildcard to match multiple file types, such
-- as @access_log_80@ and @access_log_443@. To specify multiple, different
-- file types, add another log stream entry to the configuration file, so
-- that each log file type is stored in a different log group.
--
-- Zipped files are not supported.
--
-- 'fileFingerprintLines', 'cloudWatchLogsLogStream_fileFingerprintLines' - Specifies the range of lines for identifying a file. The valid values
-- are one number, or two dash-delimited numbers, such as \'1\', \'2-5\'.
-- The default value is \'1\', meaning the first line is used to calculate
-- the fingerprint. Fingerprint lines are not sent to CloudWatch Logs
-- unless all specified lines are available.
--
-- 'initialPosition', 'cloudWatchLogsLogStream_initialPosition' - Specifies where to start to read data (start_of_file or end_of_file).
-- The default is start_of_file. This setting is only used if there is no
-- state persisted for that log stream.
--
-- 'logGroupName', 'cloudWatchLogsLogStream_logGroupName' - Specifies the destination log group. A log group is created
-- automatically if it doesn\'t already exist. Log group names can be
-- between 1 and 512 characters long. Allowed characters include a-z, A-Z,
-- 0-9, \'_\' (underscore), \'-\' (hyphen), \'\/\' (forward slash), and
-- \'.\' (period).
--
-- 'multiLineStartPattern', 'cloudWatchLogsLogStream_multiLineStartPattern' - Specifies the pattern for identifying the start of a log message.
--
-- 'timeZone', 'cloudWatchLogsLogStream_timeZone' - Specifies the time zone of log event time stamps.
newCloudWatchLogsLogStream ::
  CloudWatchLogsLogStream
newCloudWatchLogsLogStream =
  CloudWatchLogsLogStream'
    { batchCount =
        Prelude.Nothing,
      batchSize = Prelude.Nothing,
      bufferDuration = Prelude.Nothing,
      datetimeFormat = Prelude.Nothing,
      encoding = Prelude.Nothing,
      file = Prelude.Nothing,
      fileFingerprintLines = Prelude.Nothing,
      initialPosition = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      multiLineStartPattern = Prelude.Nothing,
      timeZone = Prelude.Nothing
    }

-- | Specifies the max number of log events in a batch, up to 10000. The
-- default value is 1000.
cloudWatchLogsLogStream_batchCount :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Int)
cloudWatchLogsLogStream_batchCount = Lens.lens (\CloudWatchLogsLogStream' {batchCount} -> batchCount) (\s@CloudWatchLogsLogStream' {} a -> s {batchCount = a} :: CloudWatchLogsLogStream)

-- | Specifies the maximum size of log events in a batch, in bytes, up to
-- 1048576 bytes. The default value is 32768 bytes. This size is calculated
-- as the sum of all event messages in UTF-8, plus 26 bytes for each log
-- event.
cloudWatchLogsLogStream_batchSize :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Int)
cloudWatchLogsLogStream_batchSize = Lens.lens (\CloudWatchLogsLogStream' {batchSize} -> batchSize) (\s@CloudWatchLogsLogStream' {} a -> s {batchSize = a} :: CloudWatchLogsLogStream)

-- | Specifies the time duration for the batching of log events. The minimum
-- value is 5000ms and default value is 5000ms.
cloudWatchLogsLogStream_bufferDuration :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Int)
cloudWatchLogsLogStream_bufferDuration = Lens.lens (\CloudWatchLogsLogStream' {bufferDuration} -> bufferDuration) (\s@CloudWatchLogsLogStream' {} a -> s {bufferDuration = a} :: CloudWatchLogsLogStream)

-- | Specifies how the time stamp is extracted from logs. For more
-- information, see the
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html CloudWatch Logs Agent Reference>.
cloudWatchLogsLogStream_datetimeFormat :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogStream_datetimeFormat = Lens.lens (\CloudWatchLogsLogStream' {datetimeFormat} -> datetimeFormat) (\s@CloudWatchLogsLogStream' {} a -> s {datetimeFormat = a} :: CloudWatchLogsLogStream)

-- | Specifies the encoding of the log file so that the file can be read
-- correctly. The default is @utf_8@. Encodings supported by Python
-- @codecs.decode()@ can be used here.
cloudWatchLogsLogStream_encoding :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe CloudWatchLogsEncoding)
cloudWatchLogsLogStream_encoding = Lens.lens (\CloudWatchLogsLogStream' {encoding} -> encoding) (\s@CloudWatchLogsLogStream' {} a -> s {encoding = a} :: CloudWatchLogsLogStream)

-- | Specifies log files that you want to push to CloudWatch Logs.
--
-- @File@ can point to a specific file or multiple files (by using wild
-- card characters such as @\/var\/log\/system.log*@). Only the latest file
-- is pushed to CloudWatch Logs, based on file modification time. We
-- recommend that you use wild card characters to specify a series of files
-- of the same type, such as @access_log.2014-06-01-01@,
-- @access_log.2014-06-01-02@, and so on by using a pattern like
-- @access_log.*@. Don\'t use a wildcard to match multiple file types, such
-- as @access_log_80@ and @access_log_443@. To specify multiple, different
-- file types, add another log stream entry to the configuration file, so
-- that each log file type is stored in a different log group.
--
-- Zipped files are not supported.
cloudWatchLogsLogStream_file :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogStream_file = Lens.lens (\CloudWatchLogsLogStream' {file} -> file) (\s@CloudWatchLogsLogStream' {} a -> s {file = a} :: CloudWatchLogsLogStream)

-- | Specifies the range of lines for identifying a file. The valid values
-- are one number, or two dash-delimited numbers, such as \'1\', \'2-5\'.
-- The default value is \'1\', meaning the first line is used to calculate
-- the fingerprint. Fingerprint lines are not sent to CloudWatch Logs
-- unless all specified lines are available.
cloudWatchLogsLogStream_fileFingerprintLines :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogStream_fileFingerprintLines = Lens.lens (\CloudWatchLogsLogStream' {fileFingerprintLines} -> fileFingerprintLines) (\s@CloudWatchLogsLogStream' {} a -> s {fileFingerprintLines = a} :: CloudWatchLogsLogStream)

-- | Specifies where to start to read data (start_of_file or end_of_file).
-- The default is start_of_file. This setting is only used if there is no
-- state persisted for that log stream.
cloudWatchLogsLogStream_initialPosition :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe CloudWatchLogsInitialPosition)
cloudWatchLogsLogStream_initialPosition = Lens.lens (\CloudWatchLogsLogStream' {initialPosition} -> initialPosition) (\s@CloudWatchLogsLogStream' {} a -> s {initialPosition = a} :: CloudWatchLogsLogStream)

-- | Specifies the destination log group. A log group is created
-- automatically if it doesn\'t already exist. Log group names can be
-- between 1 and 512 characters long. Allowed characters include a-z, A-Z,
-- 0-9, \'_\' (underscore), \'-\' (hyphen), \'\/\' (forward slash), and
-- \'.\' (period).
cloudWatchLogsLogStream_logGroupName :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogStream_logGroupName = Lens.lens (\CloudWatchLogsLogStream' {logGroupName} -> logGroupName) (\s@CloudWatchLogsLogStream' {} a -> s {logGroupName = a} :: CloudWatchLogsLogStream)

-- | Specifies the pattern for identifying the start of a log message.
cloudWatchLogsLogStream_multiLineStartPattern :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe Prelude.Text)
cloudWatchLogsLogStream_multiLineStartPattern = Lens.lens (\CloudWatchLogsLogStream' {multiLineStartPattern} -> multiLineStartPattern) (\s@CloudWatchLogsLogStream' {} a -> s {multiLineStartPattern = a} :: CloudWatchLogsLogStream)

-- | Specifies the time zone of log event time stamps.
cloudWatchLogsLogStream_timeZone :: Lens.Lens' CloudWatchLogsLogStream (Prelude.Maybe CloudWatchLogsTimeZone)
cloudWatchLogsLogStream_timeZone = Lens.lens (\CloudWatchLogsLogStream' {timeZone} -> timeZone) (\s@CloudWatchLogsLogStream' {} a -> s {timeZone = a} :: CloudWatchLogsLogStream)

instance Data.FromJSON CloudWatchLogsLogStream where
  parseJSON =
    Data.withObject
      "CloudWatchLogsLogStream"
      ( \x ->
          CloudWatchLogsLogStream'
            Prelude.<$> (x Data..:? "BatchCount")
            Prelude.<*> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "BufferDuration")
            Prelude.<*> (x Data..:? "DatetimeFormat")
            Prelude.<*> (x Data..:? "Encoding")
            Prelude.<*> (x Data..:? "File")
            Prelude.<*> (x Data..:? "FileFingerprintLines")
            Prelude.<*> (x Data..:? "InitialPosition")
            Prelude.<*> (x Data..:? "LogGroupName")
            Prelude.<*> (x Data..:? "MultiLineStartPattern")
            Prelude.<*> (x Data..:? "TimeZone")
      )

instance Prelude.Hashable CloudWatchLogsLogStream where
  hashWithSalt _salt CloudWatchLogsLogStream' {..} =
    _salt `Prelude.hashWithSalt` batchCount
      `Prelude.hashWithSalt` batchSize
      `Prelude.hashWithSalt` bufferDuration
      `Prelude.hashWithSalt` datetimeFormat
      `Prelude.hashWithSalt` encoding
      `Prelude.hashWithSalt` file
      `Prelude.hashWithSalt` fileFingerprintLines
      `Prelude.hashWithSalt` initialPosition
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` multiLineStartPattern
      `Prelude.hashWithSalt` timeZone

instance Prelude.NFData CloudWatchLogsLogStream where
  rnf CloudWatchLogsLogStream' {..} =
    Prelude.rnf batchCount
      `Prelude.seq` Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf bufferDuration
      `Prelude.seq` Prelude.rnf datetimeFormat
      `Prelude.seq` Prelude.rnf encoding
      `Prelude.seq` Prelude.rnf file
      `Prelude.seq` Prelude.rnf fileFingerprintLines
      `Prelude.seq` Prelude.rnf initialPosition
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf multiLineStartPattern
      `Prelude.seq` Prelude.rnf timeZone

instance Data.ToJSON CloudWatchLogsLogStream where
  toJSON CloudWatchLogsLogStream' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchCount" Data..=) Prelude.<$> batchCount,
            ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("BufferDuration" Data..=)
              Prelude.<$> bufferDuration,
            ("DatetimeFormat" Data..=)
              Prelude.<$> datetimeFormat,
            ("Encoding" Data..=) Prelude.<$> encoding,
            ("File" Data..=) Prelude.<$> file,
            ("FileFingerprintLines" Data..=)
              Prelude.<$> fileFingerprintLines,
            ("InitialPosition" Data..=)
              Prelude.<$> initialPosition,
            ("LogGroupName" Data..=) Prelude.<$> logGroupName,
            ("MultiLineStartPattern" Data..=)
              Prelude.<$> multiLineStartPattern,
            ("TimeZone" Data..=) Prelude.<$> timeZone
          ]
      )
