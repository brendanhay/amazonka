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
-- Module      : Amazonka.FSx.Types.WindowsAuditLogCreateConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.WindowsAuditLogCreateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.WindowsAccessAuditLogLevel
import qualified Amazonka.Prelude as Prelude

-- | The Windows file access auditing configuration used when creating or
-- updating an Amazon FSx for Windows File Server file system.
--
-- /See:/ 'newWindowsAuditLogCreateConfiguration' smart constructor.
data WindowsAuditLogCreateConfiguration = WindowsAuditLogCreateConfiguration'
  { -- | The Amazon Resource Name (ARN) that specifies the destination of the
    -- audit logs.
    --
    -- The destination can be any Amazon CloudWatch Logs log group ARN or
    -- Amazon Kinesis Data Firehose delivery stream ARN, with the following
    -- requirements:
    --
    -- -   The destination ARN that you provide (either CloudWatch Logs log
    --     group or Kinesis Data Firehose delivery stream) must be in the same
    --     Amazon Web Services partition, Amazon Web Services Region, and
    --     Amazon Web Services account as your Amazon FSx file system.
    --
    -- -   The name of the Amazon CloudWatch Logs log group must begin with the
    --     @\/aws\/fsx@ prefix. The name of the Amazon Kinesis Data Firehouse
    --     delivery stream must begin with the @aws-fsx@ prefix.
    --
    -- -   If you do not provide a destination in @AuditLogDestination@, Amazon
    --     FSx will create and use a log stream in the CloudWatch Logs
    --     @\/aws\/fsx\/windows@ log group.
    --
    -- -   If @AuditLogDestination@ is provided and the resource does not
    --     exist, the request will fail with a @BadRequest@ error.
    --
    -- -   If @FileAccessAuditLogLevel@ and @FileShareAccessAuditLogLevel@ are
    --     both set to @DISABLED@, you cannot specify a destination in
    --     @AuditLogDestination@.
    auditLogDestination :: Prelude.Maybe Prelude.Text,
    -- | Sets which attempt type is logged by Amazon FSx for file and folder
    -- accesses.
    --
    -- -   @SUCCESS_ONLY@ - only successful attempts to access files or folders
    --     are logged.
    --
    -- -   @FAILURE_ONLY@ - only failed attempts to access files or folders are
    --     logged.
    --
    -- -   @SUCCESS_AND_FAILURE@ - both successful attempts and failed attempts
    --     to access files or folders are logged.
    --
    -- -   @DISABLED@ - access auditing of files and folders is turned off.
    fileAccessAuditLogLevel :: WindowsAccessAuditLogLevel,
    -- | Sets which attempt type is logged by Amazon FSx for file share accesses.
    --
    -- -   @SUCCESS_ONLY@ - only successful attempts to access file shares are
    --     logged.
    --
    -- -   @FAILURE_ONLY@ - only failed attempts to access file shares are
    --     logged.
    --
    -- -   @SUCCESS_AND_FAILURE@ - both successful attempts and failed attempts
    --     to access file shares are logged.
    --
    -- -   @DISABLED@ - access auditing of file shares is turned off.
    fileShareAccessAuditLogLevel :: WindowsAccessAuditLogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WindowsAuditLogCreateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogDestination', 'windowsAuditLogCreateConfiguration_auditLogDestination' - The Amazon Resource Name (ARN) that specifies the destination of the
-- audit logs.
--
-- The destination can be any Amazon CloudWatch Logs log group ARN or
-- Amazon Kinesis Data Firehose delivery stream ARN, with the following
-- requirements:
--
-- -   The destination ARN that you provide (either CloudWatch Logs log
--     group or Kinesis Data Firehose delivery stream) must be in the same
--     Amazon Web Services partition, Amazon Web Services Region, and
--     Amazon Web Services account as your Amazon FSx file system.
--
-- -   The name of the Amazon CloudWatch Logs log group must begin with the
--     @\/aws\/fsx@ prefix. The name of the Amazon Kinesis Data Firehouse
--     delivery stream must begin with the @aws-fsx@ prefix.
--
-- -   If you do not provide a destination in @AuditLogDestination@, Amazon
--     FSx will create and use a log stream in the CloudWatch Logs
--     @\/aws\/fsx\/windows@ log group.
--
-- -   If @AuditLogDestination@ is provided and the resource does not
--     exist, the request will fail with a @BadRequest@ error.
--
-- -   If @FileAccessAuditLogLevel@ and @FileShareAccessAuditLogLevel@ are
--     both set to @DISABLED@, you cannot specify a destination in
--     @AuditLogDestination@.
--
-- 'fileAccessAuditLogLevel', 'windowsAuditLogCreateConfiguration_fileAccessAuditLogLevel' - Sets which attempt type is logged by Amazon FSx for file and folder
-- accesses.
--
-- -   @SUCCESS_ONLY@ - only successful attempts to access files or folders
--     are logged.
--
-- -   @FAILURE_ONLY@ - only failed attempts to access files or folders are
--     logged.
--
-- -   @SUCCESS_AND_FAILURE@ - both successful attempts and failed attempts
--     to access files or folders are logged.
--
-- -   @DISABLED@ - access auditing of files and folders is turned off.
--
-- 'fileShareAccessAuditLogLevel', 'windowsAuditLogCreateConfiguration_fileShareAccessAuditLogLevel' - Sets which attempt type is logged by Amazon FSx for file share accesses.
--
-- -   @SUCCESS_ONLY@ - only successful attempts to access file shares are
--     logged.
--
-- -   @FAILURE_ONLY@ - only failed attempts to access file shares are
--     logged.
--
-- -   @SUCCESS_AND_FAILURE@ - both successful attempts and failed attempts
--     to access file shares are logged.
--
-- -   @DISABLED@ - access auditing of file shares is turned off.
newWindowsAuditLogCreateConfiguration ::
  -- | 'fileAccessAuditLogLevel'
  WindowsAccessAuditLogLevel ->
  -- | 'fileShareAccessAuditLogLevel'
  WindowsAccessAuditLogLevel ->
  WindowsAuditLogCreateConfiguration
newWindowsAuditLogCreateConfiguration
  pFileAccessAuditLogLevel_
  pFileShareAccessAuditLogLevel_ =
    WindowsAuditLogCreateConfiguration'
      { auditLogDestination =
          Prelude.Nothing,
        fileAccessAuditLogLevel =
          pFileAccessAuditLogLevel_,
        fileShareAccessAuditLogLevel =
          pFileShareAccessAuditLogLevel_
      }

-- | The Amazon Resource Name (ARN) that specifies the destination of the
-- audit logs.
--
-- The destination can be any Amazon CloudWatch Logs log group ARN or
-- Amazon Kinesis Data Firehose delivery stream ARN, with the following
-- requirements:
--
-- -   The destination ARN that you provide (either CloudWatch Logs log
--     group or Kinesis Data Firehose delivery stream) must be in the same
--     Amazon Web Services partition, Amazon Web Services Region, and
--     Amazon Web Services account as your Amazon FSx file system.
--
-- -   The name of the Amazon CloudWatch Logs log group must begin with the
--     @\/aws\/fsx@ prefix. The name of the Amazon Kinesis Data Firehouse
--     delivery stream must begin with the @aws-fsx@ prefix.
--
-- -   If you do not provide a destination in @AuditLogDestination@, Amazon
--     FSx will create and use a log stream in the CloudWatch Logs
--     @\/aws\/fsx\/windows@ log group.
--
-- -   If @AuditLogDestination@ is provided and the resource does not
--     exist, the request will fail with a @BadRequest@ error.
--
-- -   If @FileAccessAuditLogLevel@ and @FileShareAccessAuditLogLevel@ are
--     both set to @DISABLED@, you cannot specify a destination in
--     @AuditLogDestination@.
windowsAuditLogCreateConfiguration_auditLogDestination :: Lens.Lens' WindowsAuditLogCreateConfiguration (Prelude.Maybe Prelude.Text)
windowsAuditLogCreateConfiguration_auditLogDestination = Lens.lens (\WindowsAuditLogCreateConfiguration' {auditLogDestination} -> auditLogDestination) (\s@WindowsAuditLogCreateConfiguration' {} a -> s {auditLogDestination = a} :: WindowsAuditLogCreateConfiguration)

-- | Sets which attempt type is logged by Amazon FSx for file and folder
-- accesses.
--
-- -   @SUCCESS_ONLY@ - only successful attempts to access files or folders
--     are logged.
--
-- -   @FAILURE_ONLY@ - only failed attempts to access files or folders are
--     logged.
--
-- -   @SUCCESS_AND_FAILURE@ - both successful attempts and failed attempts
--     to access files or folders are logged.
--
-- -   @DISABLED@ - access auditing of files and folders is turned off.
windowsAuditLogCreateConfiguration_fileAccessAuditLogLevel :: Lens.Lens' WindowsAuditLogCreateConfiguration WindowsAccessAuditLogLevel
windowsAuditLogCreateConfiguration_fileAccessAuditLogLevel = Lens.lens (\WindowsAuditLogCreateConfiguration' {fileAccessAuditLogLevel} -> fileAccessAuditLogLevel) (\s@WindowsAuditLogCreateConfiguration' {} a -> s {fileAccessAuditLogLevel = a} :: WindowsAuditLogCreateConfiguration)

-- | Sets which attempt type is logged by Amazon FSx for file share accesses.
--
-- -   @SUCCESS_ONLY@ - only successful attempts to access file shares are
--     logged.
--
-- -   @FAILURE_ONLY@ - only failed attempts to access file shares are
--     logged.
--
-- -   @SUCCESS_AND_FAILURE@ - both successful attempts and failed attempts
--     to access file shares are logged.
--
-- -   @DISABLED@ - access auditing of file shares is turned off.
windowsAuditLogCreateConfiguration_fileShareAccessAuditLogLevel :: Lens.Lens' WindowsAuditLogCreateConfiguration WindowsAccessAuditLogLevel
windowsAuditLogCreateConfiguration_fileShareAccessAuditLogLevel = Lens.lens (\WindowsAuditLogCreateConfiguration' {fileShareAccessAuditLogLevel} -> fileShareAccessAuditLogLevel) (\s@WindowsAuditLogCreateConfiguration' {} a -> s {fileShareAccessAuditLogLevel = a} :: WindowsAuditLogCreateConfiguration)

instance
  Prelude.Hashable
    WindowsAuditLogCreateConfiguration
  where
  hashWithSalt
    _salt
    WindowsAuditLogCreateConfiguration' {..} =
      _salt `Prelude.hashWithSalt` auditLogDestination
        `Prelude.hashWithSalt` fileAccessAuditLogLevel
        `Prelude.hashWithSalt` fileShareAccessAuditLogLevel

instance
  Prelude.NFData
    WindowsAuditLogCreateConfiguration
  where
  rnf WindowsAuditLogCreateConfiguration' {..} =
    Prelude.rnf auditLogDestination
      `Prelude.seq` Prelude.rnf fileAccessAuditLogLevel
      `Prelude.seq` Prelude.rnf fileShareAccessAuditLogLevel

instance
  Data.ToJSON
    WindowsAuditLogCreateConfiguration
  where
  toJSON WindowsAuditLogCreateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditLogDestination" Data..=)
              Prelude.<$> auditLogDestination,
            Prelude.Just
              ( "FileAccessAuditLogLevel"
                  Data..= fileAccessAuditLogLevel
              ),
            Prelude.Just
              ( "FileShareAccessAuditLogLevel"
                  Data..= fileShareAccessAuditLogLevel
              )
          ]
      )
