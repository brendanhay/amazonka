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
-- Module      : Amazonka.FSx.Types.WindowsAuditLogConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.WindowsAuditLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.WindowsAccessAuditLogLevel
import qualified Amazonka.Prelude as Prelude

-- | The configuration that Amazon FSx for Windows File Server uses to audit
-- and log user accesses of files, folders, and file shares on the Amazon
-- FSx for Windows File Server file system. For more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/file-access-auditing.html File access auditing>.
--
-- /See:/ 'newWindowsAuditLogConfiguration' smart constructor.
data WindowsAuditLogConfiguration = WindowsAuditLogConfiguration'
  { -- | The Amazon Resource Name (ARN) for the destination of the audit logs.
    -- The destination can be any Amazon CloudWatch Logs log group ARN or
    -- Amazon Kinesis Data Firehose delivery stream ARN.
    --
    -- The name of the Amazon CloudWatch Logs log group must begin with the
    -- @\/aws\/fsx@ prefix. The name of the Amazon Kinesis Data Firehouse
    -- delivery stream must begin with the @aws-fsx@ prefix.
    --
    -- The destination ARN (either CloudWatch Logs log group or Kinesis Data
    -- Firehose delivery stream) must be in the same Amazon Web Services
    -- partition, Amazon Web Services Region, and Amazon Web Services account
    -- as your Amazon FSx file system.
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
-- Create a value of 'WindowsAuditLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditLogDestination', 'windowsAuditLogConfiguration_auditLogDestination' - The Amazon Resource Name (ARN) for the destination of the audit logs.
-- The destination can be any Amazon CloudWatch Logs log group ARN or
-- Amazon Kinesis Data Firehose delivery stream ARN.
--
-- The name of the Amazon CloudWatch Logs log group must begin with the
-- @\/aws\/fsx@ prefix. The name of the Amazon Kinesis Data Firehouse
-- delivery stream must begin with the @aws-fsx@ prefix.
--
-- The destination ARN (either CloudWatch Logs log group or Kinesis Data
-- Firehose delivery stream) must be in the same Amazon Web Services
-- partition, Amazon Web Services Region, and Amazon Web Services account
-- as your Amazon FSx file system.
--
-- 'fileAccessAuditLogLevel', 'windowsAuditLogConfiguration_fileAccessAuditLogLevel' - Sets which attempt type is logged by Amazon FSx for file and folder
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
-- 'fileShareAccessAuditLogLevel', 'windowsAuditLogConfiguration_fileShareAccessAuditLogLevel' - Sets which attempt type is logged by Amazon FSx for file share accesses.
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
newWindowsAuditLogConfiguration ::
  -- | 'fileAccessAuditLogLevel'
  WindowsAccessAuditLogLevel ->
  -- | 'fileShareAccessAuditLogLevel'
  WindowsAccessAuditLogLevel ->
  WindowsAuditLogConfiguration
newWindowsAuditLogConfiguration
  pFileAccessAuditLogLevel_
  pFileShareAccessAuditLogLevel_ =
    WindowsAuditLogConfiguration'
      { auditLogDestination =
          Prelude.Nothing,
        fileAccessAuditLogLevel =
          pFileAccessAuditLogLevel_,
        fileShareAccessAuditLogLevel =
          pFileShareAccessAuditLogLevel_
      }

-- | The Amazon Resource Name (ARN) for the destination of the audit logs.
-- The destination can be any Amazon CloudWatch Logs log group ARN or
-- Amazon Kinesis Data Firehose delivery stream ARN.
--
-- The name of the Amazon CloudWatch Logs log group must begin with the
-- @\/aws\/fsx@ prefix. The name of the Amazon Kinesis Data Firehouse
-- delivery stream must begin with the @aws-fsx@ prefix.
--
-- The destination ARN (either CloudWatch Logs log group or Kinesis Data
-- Firehose delivery stream) must be in the same Amazon Web Services
-- partition, Amazon Web Services Region, and Amazon Web Services account
-- as your Amazon FSx file system.
windowsAuditLogConfiguration_auditLogDestination :: Lens.Lens' WindowsAuditLogConfiguration (Prelude.Maybe Prelude.Text)
windowsAuditLogConfiguration_auditLogDestination = Lens.lens (\WindowsAuditLogConfiguration' {auditLogDestination} -> auditLogDestination) (\s@WindowsAuditLogConfiguration' {} a -> s {auditLogDestination = a} :: WindowsAuditLogConfiguration)

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
windowsAuditLogConfiguration_fileAccessAuditLogLevel :: Lens.Lens' WindowsAuditLogConfiguration WindowsAccessAuditLogLevel
windowsAuditLogConfiguration_fileAccessAuditLogLevel = Lens.lens (\WindowsAuditLogConfiguration' {fileAccessAuditLogLevel} -> fileAccessAuditLogLevel) (\s@WindowsAuditLogConfiguration' {} a -> s {fileAccessAuditLogLevel = a} :: WindowsAuditLogConfiguration)

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
windowsAuditLogConfiguration_fileShareAccessAuditLogLevel :: Lens.Lens' WindowsAuditLogConfiguration WindowsAccessAuditLogLevel
windowsAuditLogConfiguration_fileShareAccessAuditLogLevel = Lens.lens (\WindowsAuditLogConfiguration' {fileShareAccessAuditLogLevel} -> fileShareAccessAuditLogLevel) (\s@WindowsAuditLogConfiguration' {} a -> s {fileShareAccessAuditLogLevel = a} :: WindowsAuditLogConfiguration)

instance Core.FromJSON WindowsAuditLogConfiguration where
  parseJSON =
    Core.withObject
      "WindowsAuditLogConfiguration"
      ( \x ->
          WindowsAuditLogConfiguration'
            Prelude.<$> (x Core..:? "AuditLogDestination")
            Prelude.<*> (x Core..: "FileAccessAuditLogLevel")
            Prelude.<*> (x Core..: "FileShareAccessAuditLogLevel")
      )

instance
  Prelude.Hashable
    WindowsAuditLogConfiguration
  where
  hashWithSalt _salt WindowsAuditLogConfiguration' {..} =
    _salt `Prelude.hashWithSalt` auditLogDestination
      `Prelude.hashWithSalt` fileAccessAuditLogLevel
      `Prelude.hashWithSalt` fileShareAccessAuditLogLevel

instance Prelude.NFData WindowsAuditLogConfiguration where
  rnf WindowsAuditLogConfiguration' {..} =
    Prelude.rnf auditLogDestination
      `Prelude.seq` Prelude.rnf fileAccessAuditLogLevel
      `Prelude.seq` Prelude.rnf fileShareAccessAuditLogLevel
