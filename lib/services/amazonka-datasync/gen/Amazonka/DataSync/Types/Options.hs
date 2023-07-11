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
-- Module      : Amazonka.DataSync.Types.Options
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Options where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types.Atime
import Amazonka.DataSync.Types.Gid
import Amazonka.DataSync.Types.LogLevel
import Amazonka.DataSync.Types.Mtime
import Amazonka.DataSync.Types.ObjectTags
import Amazonka.DataSync.Types.OverwriteMode
import Amazonka.DataSync.Types.PosixPermissions
import Amazonka.DataSync.Types.PreserveDeletedFiles
import Amazonka.DataSync.Types.PreserveDevices
import Amazonka.DataSync.Types.SmbSecurityDescriptorCopyFlags
import Amazonka.DataSync.Types.TaskQueueing
import Amazonka.DataSync.Types.TransferMode
import Amazonka.DataSync.Types.Uid
import Amazonka.DataSync.Types.VerifyMode
import qualified Amazonka.Prelude as Prelude

-- | Configures your DataSync task settings. These options include how
-- DataSync handles files, objects, and their associated metadata. You also
-- can specify how DataSync verifies data integrity, set bandwidth limits
-- for your task, among other options.
--
-- Each task setting has a default value. Unless you need to, you don\'t
-- have to configure any of these @Options@ before starting your task.
--
-- /See:/ 'newOptions' smart constructor.
data Options = Options'
  { -- | Specifies whether to preserve metadata indicating the last time a file
    -- was read or written to. If you set @Atime@ to @BEST_EFFORT@, DataSync
    -- attempts to preserve the original @Atime@ attribute on all source files
    -- (that is, the version before the @PREPARING@ phase of the task
    -- execution).
    --
    -- The behavior of @Atime@ isn\'t fully standard across platforms, so
    -- DataSync can only do this on a best-effort basis.
    --
    -- Default value: @BEST_EFFORT@
    --
    -- @BEST_EFFORT@: Attempt to preserve the per-file @Atime@ value
    -- (recommended).
    --
    -- @NONE@: Ignore @Atime@.
    --
    -- If @Atime@ is set to @BEST_EFFORT@, @Mtime@ must be set to @PRESERVE@.
    --
    -- If @Atime@ is set to @NONE@, @Mtime@ must also be @NONE@.
    atime :: Prelude.Maybe Atime,
    -- | Limits the bandwidth used by a DataSync task. For example, if you want
    -- DataSync to use a maximum of 1 MB, set this value to @1048576@
    -- (@=1024*1024@).
    bytesPerSecond :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the POSIX group ID (GID) of the file\'s owners.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
    --
    -- Default value: @INT_VALUE@. This preserves the integer value of the ID.
    --
    -- @INT_VALUE@: Preserve the integer value of user ID (UID) and GID
    -- (recommended).
    --
    -- @NONE@: Ignore UID and GID.
    gid :: Prelude.Maybe Gid,
    -- | Specifies the type of logs that DataSync publishes to a Amazon
    -- CloudWatch Logs log group. To specify the log group, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateTask.html#DataSync-CreateTask-request-CloudWatchLogGroupArn CloudWatchLogGroupArn>.
    --
    -- If you set @LogLevel@ to @OFF@, no logs are published. @BASIC@ publishes
    -- logs on errors for individual files transferred. @TRANSFER@ publishes
    -- logs for every file or object that is transferred and integrity checked.
    logLevel :: Prelude.Maybe LogLevel,
    -- | Specifies whether to preserve metadata indicating the last time that a
    -- file was written to before the @PREPARING@ phase of your task execution.
    -- This option is required when you need to run the a task more than once.
    --
    -- Default Value: @PRESERVE@
    --
    -- @PRESERVE@: Preserve original @Mtime@ (recommended)
    --
    -- @NONE@: Ignore @Mtime@.
    --
    -- If @Mtime@ is set to @PRESERVE@, @Atime@ must be set to @BEST_EFFORT@.
    --
    -- If @Mtime@ is set to @NONE@, @Atime@ must also be set to @NONE@.
    mtime :: Prelude.Maybe Mtime,
    -- | Specifies whether object tags are preserved when transferring between
    -- object storage systems. If you want your DataSync task to ignore object
    -- tags, specify the @NONE@ value.
    --
    -- Default Value: @PRESERVE@
    objectTags :: Prelude.Maybe ObjectTags,
    -- | Specifies whether data at the destination location should be overwritten
    -- or preserved. If set to @NEVER@, a destination file for example will not
    -- be replaced by a source file (even if the destination file differs from
    -- the source file). If you modify files in the destination and you sync
    -- the files, you can use this value to protect against overwriting those
    -- changes.
    --
    -- Some storage classes have specific behaviors that can affect your Amazon
    -- S3 storage cost. For detailed information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
    -- .
    overwriteMode :: Prelude.Maybe OverwriteMode,
    -- | Specifies which users or groups can access a file for a specific purpose
    -- such as reading, writing, or execution of the file.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
    --
    -- Default value: @PRESERVE@
    --
    -- @PRESERVE@: Preserve POSIX-style permissions (recommended).
    --
    -- @NONE@: Ignore permissions.
    --
    -- DataSync can preserve extant permissions of a source location.
    posixPermissions :: Prelude.Maybe PosixPermissions,
    -- | Specifies whether files in the destination location that don\'t exist in
    -- the source should be preserved. This option can affect your Amazon S3
    -- storage cost. If your task deletes objects, you might incur minimum
    -- storage duration charges for certain storage classes. For detailed
    -- information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
    -- .
    --
    -- Default value: @PRESERVE@
    --
    -- @PRESERVE@: Ignore such destination files (recommended).
    --
    -- @REMOVE@: Delete destination files that aren’t present in the source.
    preserveDeletedFiles :: Prelude.Maybe PreserveDeletedFiles,
    -- | Specifies whether DataSync should preserve the metadata of block and
    -- character devices in the source location and recreate the files with
    -- that device name and metadata on the destination. DataSync copies only
    -- the name and metadata of such devices.
    --
    -- DataSync can\'t copy the actual contents of these devices because
    -- they\'re nonterminal and don\'t return an end-of-file (EOF) marker.
    --
    -- Default value: @NONE@
    --
    -- @NONE@: Ignore special devices (recommended).
    --
    -- @PRESERVE@: Preserve character and block device metadata. This option
    -- currently isn\'t supported for Amazon EFS.
    preserveDevices :: Prelude.Maybe PreserveDevices,
    -- | Specifies which components of the SMB security descriptor are copied
    -- from source to destination objects.
    --
    -- This value is only used for transfers between SMB and Amazon FSx for
    -- Windows File Server locations or between two FSx for Windows File Server
    -- locations. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html how DataSync handles metadata>.
    --
    -- Default value: @OWNER_DACL@
    --
    -- @OWNER_DACL@: For each copied object, DataSync copies the following
    -- metadata:
    --
    -- -   The object owner.
    --
    -- -   NTFS discretionary access control lists (DACLs), which determine
    --     whether to grant access to an object.
    --
    --     DataSync won\'t copy NTFS system access control lists (SACLs) with
    --     this option.
    --
    -- @OWNER_DACL_SACL@: For each copied object, DataSync copies the following
    -- metadata:
    --
    -- -   The object owner.
    --
    -- -   NTFS discretionary access control lists (DACLs), which determine
    --     whether to grant access to an object.
    --
    -- -   SACLs, which are used by administrators to log attempts to access a
    --     secured object.
    --
    --     Copying SACLs requires granting additional permissions to the
    --     Windows user that DataSync uses to access your SMB location. For
    --     information about choosing a user that ensures sufficient
    --     permissions to files, folders, and metadata, see
    --     <create-smb-location.html#SMBuser user>.
    --
    -- @NONE@: None of the SMB security descriptor components are copied.
    -- Destination objects are owned by the user that was provided for
    -- accessing the destination location. DACLs and SACLs are set based on the
    -- destination server’s configuration.
    securityDescriptorCopyFlags :: Prelude.Maybe SmbSecurityDescriptorCopyFlags,
    -- | Specifies whether tasks should be queued before executing the tasks. The
    -- default is @ENABLED@, which means the tasks will be queued.
    --
    -- If you use the same agent to run multiple tasks, you can enable the
    -- tasks to run in series. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/run-task.html#queue-task-execution Queueing task executions>.
    taskQueueing :: Prelude.Maybe TaskQueueing,
    -- | Determines whether DataSync transfers only the data and metadata that
    -- differ between the source and the destination location or transfers all
    -- the content from the source (without comparing what\'s in the
    -- destination).
    --
    -- @CHANGED@: DataSync copies only data or metadata that is new or
    -- different content from the source location to the destination location.
    --
    -- @ALL@: DataSync copies all source location content to the destination
    -- (without comparing what\'s in the destination).
    transferMode :: Prelude.Maybe TransferMode,
    -- | Specifies the POSIX user ID (UID) of the file\'s owner.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
    --
    -- Default value: @INT_VALUE@. This preserves the integer value of the ID.
    --
    -- @INT_VALUE@: Preserve the integer value of UID and group ID (GID)
    -- (recommended).
    --
    -- @NONE@: Ignore UID and GID.
    uid :: Prelude.Maybe Uid,
    -- | Specifies how and when DataSync checks the integrity of your data during
    -- a transfer.
    --
    -- Default value: @POINT_IN_TIME_CONSISTENT@
    --
    -- @ONLY_FILES_TRANSFERRED@ (recommended): DataSync calculates the checksum
    -- of transferred files and metadata at the source location. At the end of
    -- the transfer, DataSync then compares this checksum to the checksum
    -- calculated on those files at the destination.
    --
    -- We recommend this option when transferring to S3 Glacier Flexible
    -- Retrieval or S3 Glacier Deep Archive storage classes. For more
    -- information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Storage class considerations with Amazon S3 locations>.
    --
    -- @POINT_IN_TIME_CONSISTENT@: At the end of the transfer, DataSync scans
    -- the entire source and destination to verify that both locations are
    -- fully synchronized.
    --
    -- You can\'t use this option when transferring to S3 Glacier Flexible
    -- Retrieval or S3 Glacier Deep Archive storage classes. For more
    -- information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Storage class considerations with Amazon S3 locations>.
    --
    -- @NONE@: DataSync doesn\'t run additional verification at the end of the
    -- transfer. All data transmissions are still integrity-checked with
    -- checksum verification during the transfer.
    verifyMode :: Prelude.Maybe VerifyMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Options' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'atime', 'options_atime' - Specifies whether to preserve metadata indicating the last time a file
-- was read or written to. If you set @Atime@ to @BEST_EFFORT@, DataSync
-- attempts to preserve the original @Atime@ attribute on all source files
-- (that is, the version before the @PREPARING@ phase of the task
-- execution).
--
-- The behavior of @Atime@ isn\'t fully standard across platforms, so
-- DataSync can only do this on a best-effort basis.
--
-- Default value: @BEST_EFFORT@
--
-- @BEST_EFFORT@: Attempt to preserve the per-file @Atime@ value
-- (recommended).
--
-- @NONE@: Ignore @Atime@.
--
-- If @Atime@ is set to @BEST_EFFORT@, @Mtime@ must be set to @PRESERVE@.
--
-- If @Atime@ is set to @NONE@, @Mtime@ must also be @NONE@.
--
-- 'bytesPerSecond', 'options_bytesPerSecond' - Limits the bandwidth used by a DataSync task. For example, if you want
-- DataSync to use a maximum of 1 MB, set this value to @1048576@
-- (@=1024*1024@).
--
-- 'gid', 'options_gid' - Specifies the POSIX group ID (GID) of the file\'s owners.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
--
-- Default value: @INT_VALUE@. This preserves the integer value of the ID.
--
-- @INT_VALUE@: Preserve the integer value of user ID (UID) and GID
-- (recommended).
--
-- @NONE@: Ignore UID and GID.
--
-- 'logLevel', 'options_logLevel' - Specifies the type of logs that DataSync publishes to a Amazon
-- CloudWatch Logs log group. To specify the log group, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateTask.html#DataSync-CreateTask-request-CloudWatchLogGroupArn CloudWatchLogGroupArn>.
--
-- If you set @LogLevel@ to @OFF@, no logs are published. @BASIC@ publishes
-- logs on errors for individual files transferred. @TRANSFER@ publishes
-- logs for every file or object that is transferred and integrity checked.
--
-- 'mtime', 'options_mtime' - Specifies whether to preserve metadata indicating the last time that a
-- file was written to before the @PREPARING@ phase of your task execution.
-- This option is required when you need to run the a task more than once.
--
-- Default Value: @PRESERVE@
--
-- @PRESERVE@: Preserve original @Mtime@ (recommended)
--
-- @NONE@: Ignore @Mtime@.
--
-- If @Mtime@ is set to @PRESERVE@, @Atime@ must be set to @BEST_EFFORT@.
--
-- If @Mtime@ is set to @NONE@, @Atime@ must also be set to @NONE@.
--
-- 'objectTags', 'options_objectTags' - Specifies whether object tags are preserved when transferring between
-- object storage systems. If you want your DataSync task to ignore object
-- tags, specify the @NONE@ value.
--
-- Default Value: @PRESERVE@
--
-- 'overwriteMode', 'options_overwriteMode' - Specifies whether data at the destination location should be overwritten
-- or preserved. If set to @NEVER@, a destination file for example will not
-- be replaced by a source file (even if the destination file differs from
-- the source file). If you modify files in the destination and you sync
-- the files, you can use this value to protect against overwriting those
-- changes.
--
-- Some storage classes have specific behaviors that can affect your Amazon
-- S3 storage cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- .
--
-- 'posixPermissions', 'options_posixPermissions' - Specifies which users or groups can access a file for a specific purpose
-- such as reading, writing, or execution of the file.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
--
-- Default value: @PRESERVE@
--
-- @PRESERVE@: Preserve POSIX-style permissions (recommended).
--
-- @NONE@: Ignore permissions.
--
-- DataSync can preserve extant permissions of a source location.
--
-- 'preserveDeletedFiles', 'options_preserveDeletedFiles' - Specifies whether files in the destination location that don\'t exist in
-- the source should be preserved. This option can affect your Amazon S3
-- storage cost. If your task deletes objects, you might incur minimum
-- storage duration charges for certain storage classes. For detailed
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- .
--
-- Default value: @PRESERVE@
--
-- @PRESERVE@: Ignore such destination files (recommended).
--
-- @REMOVE@: Delete destination files that aren’t present in the source.
--
-- 'preserveDevices', 'options_preserveDevices' - Specifies whether DataSync should preserve the metadata of block and
-- character devices in the source location and recreate the files with
-- that device name and metadata on the destination. DataSync copies only
-- the name and metadata of such devices.
--
-- DataSync can\'t copy the actual contents of these devices because
-- they\'re nonterminal and don\'t return an end-of-file (EOF) marker.
--
-- Default value: @NONE@
--
-- @NONE@: Ignore special devices (recommended).
--
-- @PRESERVE@: Preserve character and block device metadata. This option
-- currently isn\'t supported for Amazon EFS.
--
-- 'securityDescriptorCopyFlags', 'options_securityDescriptorCopyFlags' - Specifies which components of the SMB security descriptor are copied
-- from source to destination objects.
--
-- This value is only used for transfers between SMB and Amazon FSx for
-- Windows File Server locations or between two FSx for Windows File Server
-- locations. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html how DataSync handles metadata>.
--
-- Default value: @OWNER_DACL@
--
-- @OWNER_DACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   The object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
--     DataSync won\'t copy NTFS system access control lists (SACLs) with
--     this option.
--
-- @OWNER_DACL_SACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   The object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
-- -   SACLs, which are used by administrators to log attempts to access a
--     secured object.
--
--     Copying SACLs requires granting additional permissions to the
--     Windows user that DataSync uses to access your SMB location. For
--     information about choosing a user that ensures sufficient
--     permissions to files, folders, and metadata, see
--     <create-smb-location.html#SMBuser user>.
--
-- @NONE@: None of the SMB security descriptor components are copied.
-- Destination objects are owned by the user that was provided for
-- accessing the destination location. DACLs and SACLs are set based on the
-- destination server’s configuration.
--
-- 'taskQueueing', 'options_taskQueueing' - Specifies whether tasks should be queued before executing the tasks. The
-- default is @ENABLED@, which means the tasks will be queued.
--
-- If you use the same agent to run multiple tasks, you can enable the
-- tasks to run in series. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/run-task.html#queue-task-execution Queueing task executions>.
--
-- 'transferMode', 'options_transferMode' - Determines whether DataSync transfers only the data and metadata that
-- differ between the source and the destination location or transfers all
-- the content from the source (without comparing what\'s in the
-- destination).
--
-- @CHANGED@: DataSync copies only data or metadata that is new or
-- different content from the source location to the destination location.
--
-- @ALL@: DataSync copies all source location content to the destination
-- (without comparing what\'s in the destination).
--
-- 'uid', 'options_uid' - Specifies the POSIX user ID (UID) of the file\'s owner.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
--
-- Default value: @INT_VALUE@. This preserves the integer value of the ID.
--
-- @INT_VALUE@: Preserve the integer value of UID and group ID (GID)
-- (recommended).
--
-- @NONE@: Ignore UID and GID.
--
-- 'verifyMode', 'options_verifyMode' - Specifies how and when DataSync checks the integrity of your data during
-- a transfer.
--
-- Default value: @POINT_IN_TIME_CONSISTENT@
--
-- @ONLY_FILES_TRANSFERRED@ (recommended): DataSync calculates the checksum
-- of transferred files and metadata at the source location. At the end of
-- the transfer, DataSync then compares this checksum to the checksum
-- calculated on those files at the destination.
--
-- We recommend this option when transferring to S3 Glacier Flexible
-- Retrieval or S3 Glacier Deep Archive storage classes. For more
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Storage class considerations with Amazon S3 locations>.
--
-- @POINT_IN_TIME_CONSISTENT@: At the end of the transfer, DataSync scans
-- the entire source and destination to verify that both locations are
-- fully synchronized.
--
-- You can\'t use this option when transferring to S3 Glacier Flexible
-- Retrieval or S3 Glacier Deep Archive storage classes. For more
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Storage class considerations with Amazon S3 locations>.
--
-- @NONE@: DataSync doesn\'t run additional verification at the end of the
-- transfer. All data transmissions are still integrity-checked with
-- checksum verification during the transfer.
newOptions ::
  Options
newOptions =
  Options'
    { atime = Prelude.Nothing,
      bytesPerSecond = Prelude.Nothing,
      gid = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      mtime = Prelude.Nothing,
      objectTags = Prelude.Nothing,
      overwriteMode = Prelude.Nothing,
      posixPermissions = Prelude.Nothing,
      preserveDeletedFiles = Prelude.Nothing,
      preserveDevices = Prelude.Nothing,
      securityDescriptorCopyFlags = Prelude.Nothing,
      taskQueueing = Prelude.Nothing,
      transferMode = Prelude.Nothing,
      uid = Prelude.Nothing,
      verifyMode = Prelude.Nothing
    }

-- | Specifies whether to preserve metadata indicating the last time a file
-- was read or written to. If you set @Atime@ to @BEST_EFFORT@, DataSync
-- attempts to preserve the original @Atime@ attribute on all source files
-- (that is, the version before the @PREPARING@ phase of the task
-- execution).
--
-- The behavior of @Atime@ isn\'t fully standard across platforms, so
-- DataSync can only do this on a best-effort basis.
--
-- Default value: @BEST_EFFORT@
--
-- @BEST_EFFORT@: Attempt to preserve the per-file @Atime@ value
-- (recommended).
--
-- @NONE@: Ignore @Atime@.
--
-- If @Atime@ is set to @BEST_EFFORT@, @Mtime@ must be set to @PRESERVE@.
--
-- If @Atime@ is set to @NONE@, @Mtime@ must also be @NONE@.
options_atime :: Lens.Lens' Options (Prelude.Maybe Atime)
options_atime = Lens.lens (\Options' {atime} -> atime) (\s@Options' {} a -> s {atime = a} :: Options)

-- | Limits the bandwidth used by a DataSync task. For example, if you want
-- DataSync to use a maximum of 1 MB, set this value to @1048576@
-- (@=1024*1024@).
options_bytesPerSecond :: Lens.Lens' Options (Prelude.Maybe Prelude.Integer)
options_bytesPerSecond = Lens.lens (\Options' {bytesPerSecond} -> bytesPerSecond) (\s@Options' {} a -> s {bytesPerSecond = a} :: Options)

-- | Specifies the POSIX group ID (GID) of the file\'s owners.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
--
-- Default value: @INT_VALUE@. This preserves the integer value of the ID.
--
-- @INT_VALUE@: Preserve the integer value of user ID (UID) and GID
-- (recommended).
--
-- @NONE@: Ignore UID and GID.
options_gid :: Lens.Lens' Options (Prelude.Maybe Gid)
options_gid = Lens.lens (\Options' {gid} -> gid) (\s@Options' {} a -> s {gid = a} :: Options)

-- | Specifies the type of logs that DataSync publishes to a Amazon
-- CloudWatch Logs log group. To specify the log group, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateTask.html#DataSync-CreateTask-request-CloudWatchLogGroupArn CloudWatchLogGroupArn>.
--
-- If you set @LogLevel@ to @OFF@, no logs are published. @BASIC@ publishes
-- logs on errors for individual files transferred. @TRANSFER@ publishes
-- logs for every file or object that is transferred and integrity checked.
options_logLevel :: Lens.Lens' Options (Prelude.Maybe LogLevel)
options_logLevel = Lens.lens (\Options' {logLevel} -> logLevel) (\s@Options' {} a -> s {logLevel = a} :: Options)

-- | Specifies whether to preserve metadata indicating the last time that a
-- file was written to before the @PREPARING@ phase of your task execution.
-- This option is required when you need to run the a task more than once.
--
-- Default Value: @PRESERVE@
--
-- @PRESERVE@: Preserve original @Mtime@ (recommended)
--
-- @NONE@: Ignore @Mtime@.
--
-- If @Mtime@ is set to @PRESERVE@, @Atime@ must be set to @BEST_EFFORT@.
--
-- If @Mtime@ is set to @NONE@, @Atime@ must also be set to @NONE@.
options_mtime :: Lens.Lens' Options (Prelude.Maybe Mtime)
options_mtime = Lens.lens (\Options' {mtime} -> mtime) (\s@Options' {} a -> s {mtime = a} :: Options)

-- | Specifies whether object tags are preserved when transferring between
-- object storage systems. If you want your DataSync task to ignore object
-- tags, specify the @NONE@ value.
--
-- Default Value: @PRESERVE@
options_objectTags :: Lens.Lens' Options (Prelude.Maybe ObjectTags)
options_objectTags = Lens.lens (\Options' {objectTags} -> objectTags) (\s@Options' {} a -> s {objectTags = a} :: Options)

-- | Specifies whether data at the destination location should be overwritten
-- or preserved. If set to @NEVER@, a destination file for example will not
-- be replaced by a source file (even if the destination file differs from
-- the source file). If you modify files in the destination and you sync
-- the files, you can use this value to protect against overwriting those
-- changes.
--
-- Some storage classes have specific behaviors that can affect your Amazon
-- S3 storage cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- .
options_overwriteMode :: Lens.Lens' Options (Prelude.Maybe OverwriteMode)
options_overwriteMode = Lens.lens (\Options' {overwriteMode} -> overwriteMode) (\s@Options' {} a -> s {overwriteMode = a} :: Options)

-- | Specifies which users or groups can access a file for a specific purpose
-- such as reading, writing, or execution of the file.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
--
-- Default value: @PRESERVE@
--
-- @PRESERVE@: Preserve POSIX-style permissions (recommended).
--
-- @NONE@: Ignore permissions.
--
-- DataSync can preserve extant permissions of a source location.
options_posixPermissions :: Lens.Lens' Options (Prelude.Maybe PosixPermissions)
options_posixPermissions = Lens.lens (\Options' {posixPermissions} -> posixPermissions) (\s@Options' {} a -> s {posixPermissions = a} :: Options)

-- | Specifies whether files in the destination location that don\'t exist in
-- the source should be preserved. This option can affect your Amazon S3
-- storage cost. If your task deletes objects, you might incur minimum
-- storage duration charges for certain storage classes. For detailed
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- .
--
-- Default value: @PRESERVE@
--
-- @PRESERVE@: Ignore such destination files (recommended).
--
-- @REMOVE@: Delete destination files that aren’t present in the source.
options_preserveDeletedFiles :: Lens.Lens' Options (Prelude.Maybe PreserveDeletedFiles)
options_preserveDeletedFiles = Lens.lens (\Options' {preserveDeletedFiles} -> preserveDeletedFiles) (\s@Options' {} a -> s {preserveDeletedFiles = a} :: Options)

-- | Specifies whether DataSync should preserve the metadata of block and
-- character devices in the source location and recreate the files with
-- that device name and metadata on the destination. DataSync copies only
-- the name and metadata of such devices.
--
-- DataSync can\'t copy the actual contents of these devices because
-- they\'re nonterminal and don\'t return an end-of-file (EOF) marker.
--
-- Default value: @NONE@
--
-- @NONE@: Ignore special devices (recommended).
--
-- @PRESERVE@: Preserve character and block device metadata. This option
-- currently isn\'t supported for Amazon EFS.
options_preserveDevices :: Lens.Lens' Options (Prelude.Maybe PreserveDevices)
options_preserveDevices = Lens.lens (\Options' {preserveDevices} -> preserveDevices) (\s@Options' {} a -> s {preserveDevices = a} :: Options)

-- | Specifies which components of the SMB security descriptor are copied
-- from source to destination objects.
--
-- This value is only used for transfers between SMB and Amazon FSx for
-- Windows File Server locations or between two FSx for Windows File Server
-- locations. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html how DataSync handles metadata>.
--
-- Default value: @OWNER_DACL@
--
-- @OWNER_DACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   The object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
--     DataSync won\'t copy NTFS system access control lists (SACLs) with
--     this option.
--
-- @OWNER_DACL_SACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   The object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
-- -   SACLs, which are used by administrators to log attempts to access a
--     secured object.
--
--     Copying SACLs requires granting additional permissions to the
--     Windows user that DataSync uses to access your SMB location. For
--     information about choosing a user that ensures sufficient
--     permissions to files, folders, and metadata, see
--     <create-smb-location.html#SMBuser user>.
--
-- @NONE@: None of the SMB security descriptor components are copied.
-- Destination objects are owned by the user that was provided for
-- accessing the destination location. DACLs and SACLs are set based on the
-- destination server’s configuration.
options_securityDescriptorCopyFlags :: Lens.Lens' Options (Prelude.Maybe SmbSecurityDescriptorCopyFlags)
options_securityDescriptorCopyFlags = Lens.lens (\Options' {securityDescriptorCopyFlags} -> securityDescriptorCopyFlags) (\s@Options' {} a -> s {securityDescriptorCopyFlags = a} :: Options)

-- | Specifies whether tasks should be queued before executing the tasks. The
-- default is @ENABLED@, which means the tasks will be queued.
--
-- If you use the same agent to run multiple tasks, you can enable the
-- tasks to run in series. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/run-task.html#queue-task-execution Queueing task executions>.
options_taskQueueing :: Lens.Lens' Options (Prelude.Maybe TaskQueueing)
options_taskQueueing = Lens.lens (\Options' {taskQueueing} -> taskQueueing) (\s@Options' {} a -> s {taskQueueing = a} :: Options)

-- | Determines whether DataSync transfers only the data and metadata that
-- differ between the source and the destination location or transfers all
-- the content from the source (without comparing what\'s in the
-- destination).
--
-- @CHANGED@: DataSync copies only data or metadata that is new or
-- different content from the source location to the destination location.
--
-- @ALL@: DataSync copies all source location content to the destination
-- (without comparing what\'s in the destination).
options_transferMode :: Lens.Lens' Options (Prelude.Maybe TransferMode)
options_transferMode = Lens.lens (\Options' {transferMode} -> transferMode) (\s@Options' {} a -> s {transferMode = a} :: Options)

-- | Specifies the POSIX user ID (UID) of the file\'s owner.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html#metadata-copied Metadata copied by DataSync>.
--
-- Default value: @INT_VALUE@. This preserves the integer value of the ID.
--
-- @INT_VALUE@: Preserve the integer value of UID and group ID (GID)
-- (recommended).
--
-- @NONE@: Ignore UID and GID.
options_uid :: Lens.Lens' Options (Prelude.Maybe Uid)
options_uid = Lens.lens (\Options' {uid} -> uid) (\s@Options' {} a -> s {uid = a} :: Options)

-- | Specifies how and when DataSync checks the integrity of your data during
-- a transfer.
--
-- Default value: @POINT_IN_TIME_CONSISTENT@
--
-- @ONLY_FILES_TRANSFERRED@ (recommended): DataSync calculates the checksum
-- of transferred files and metadata at the source location. At the end of
-- the transfer, DataSync then compares this checksum to the checksum
-- calculated on those files at the destination.
--
-- We recommend this option when transferring to S3 Glacier Flexible
-- Retrieval or S3 Glacier Deep Archive storage classes. For more
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Storage class considerations with Amazon S3 locations>.
--
-- @POINT_IN_TIME_CONSISTENT@: At the end of the transfer, DataSync scans
-- the entire source and destination to verify that both locations are
-- fully synchronized.
--
-- You can\'t use this option when transferring to S3 Glacier Flexible
-- Retrieval or S3 Glacier Deep Archive storage classes. For more
-- information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Storage class considerations with Amazon S3 locations>.
--
-- @NONE@: DataSync doesn\'t run additional verification at the end of the
-- transfer. All data transmissions are still integrity-checked with
-- checksum verification during the transfer.
options_verifyMode :: Lens.Lens' Options (Prelude.Maybe VerifyMode)
options_verifyMode = Lens.lens (\Options' {verifyMode} -> verifyMode) (\s@Options' {} a -> s {verifyMode = a} :: Options)

instance Data.FromJSON Options where
  parseJSON =
    Data.withObject
      "Options"
      ( \x ->
          Options'
            Prelude.<$> (x Data..:? "Atime")
            Prelude.<*> (x Data..:? "BytesPerSecond")
            Prelude.<*> (x Data..:? "Gid")
            Prelude.<*> (x Data..:? "LogLevel")
            Prelude.<*> (x Data..:? "Mtime")
            Prelude.<*> (x Data..:? "ObjectTags")
            Prelude.<*> (x Data..:? "OverwriteMode")
            Prelude.<*> (x Data..:? "PosixPermissions")
            Prelude.<*> (x Data..:? "PreserveDeletedFiles")
            Prelude.<*> (x Data..:? "PreserveDevices")
            Prelude.<*> (x Data..:? "SecurityDescriptorCopyFlags")
            Prelude.<*> (x Data..:? "TaskQueueing")
            Prelude.<*> (x Data..:? "TransferMode")
            Prelude.<*> (x Data..:? "Uid")
            Prelude.<*> (x Data..:? "VerifyMode")
      )

instance Prelude.Hashable Options where
  hashWithSalt _salt Options' {..} =
    _salt
      `Prelude.hashWithSalt` atime
      `Prelude.hashWithSalt` bytesPerSecond
      `Prelude.hashWithSalt` gid
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` mtime
      `Prelude.hashWithSalt` objectTags
      `Prelude.hashWithSalt` overwriteMode
      `Prelude.hashWithSalt` posixPermissions
      `Prelude.hashWithSalt` preserveDeletedFiles
      `Prelude.hashWithSalt` preserveDevices
      `Prelude.hashWithSalt` securityDescriptorCopyFlags
      `Prelude.hashWithSalt` taskQueueing
      `Prelude.hashWithSalt` transferMode
      `Prelude.hashWithSalt` uid
      `Prelude.hashWithSalt` verifyMode

instance Prelude.NFData Options where
  rnf Options' {..} =
    Prelude.rnf atime
      `Prelude.seq` Prelude.rnf bytesPerSecond
      `Prelude.seq` Prelude.rnf gid
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf mtime
      `Prelude.seq` Prelude.rnf objectTags
      `Prelude.seq` Prelude.rnf overwriteMode
      `Prelude.seq` Prelude.rnf posixPermissions
      `Prelude.seq` Prelude.rnf preserveDeletedFiles
      `Prelude.seq` Prelude.rnf preserveDevices
      `Prelude.seq` Prelude.rnf securityDescriptorCopyFlags
      `Prelude.seq` Prelude.rnf taskQueueing
      `Prelude.seq` Prelude.rnf transferMode
      `Prelude.seq` Prelude.rnf uid
      `Prelude.seq` Prelude.rnf verifyMode

instance Data.ToJSON Options where
  toJSON Options' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Atime" Data..=) Prelude.<$> atime,
            ("BytesPerSecond" Data..=)
              Prelude.<$> bytesPerSecond,
            ("Gid" Data..=) Prelude.<$> gid,
            ("LogLevel" Data..=) Prelude.<$> logLevel,
            ("Mtime" Data..=) Prelude.<$> mtime,
            ("ObjectTags" Data..=) Prelude.<$> objectTags,
            ("OverwriteMode" Data..=) Prelude.<$> overwriteMode,
            ("PosixPermissions" Data..=)
              Prelude.<$> posixPermissions,
            ("PreserveDeletedFiles" Data..=)
              Prelude.<$> preserveDeletedFiles,
            ("PreserveDevices" Data..=)
              Prelude.<$> preserveDevices,
            ("SecurityDescriptorCopyFlags" Data..=)
              Prelude.<$> securityDescriptorCopyFlags,
            ("TaskQueueing" Data..=) Prelude.<$> taskQueueing,
            ("TransferMode" Data..=) Prelude.<$> transferMode,
            ("Uid" Data..=) Prelude.<$> uid,
            ("VerifyMode" Data..=) Prelude.<$> verifyMode
          ]
      )
