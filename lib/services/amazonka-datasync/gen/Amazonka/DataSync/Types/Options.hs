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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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

-- | Represents the options that are available to control the behavior of a
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>
-- operation. Behavior includes preserving metadata such as user ID (UID),
-- group ID (GID), and file permissions, and also overwriting files in the
-- destination, data integrity verification, and so on.
--
-- A task has a set of default options associated with it. If you don\'t
-- specify an option in
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>,
-- the default value is used. You can override the defaults options on each
-- task execution by specifying an overriding @Options@ value to
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_StartTaskExecution.html StartTaskExecution>.
--
-- /See:/ 'newOptions' smart constructor.
data Options = Options'
  { -- | Specifies whether object tags are maintained when transferring between
    -- object storage systems. If you want your DataSync task to ignore object
    -- tags, specify the @NONE@ value.
    --
    -- Default Value: @PRESERVE@
    objectTags :: Prelude.Maybe ObjectTags,
    -- | The POSIX group ID (GID) of the file\'s owners.
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
    -- | A value that determines the type of logs that DataSync publishes to a
    -- log stream in the Amazon CloudWatch log group that you provide. For more
    -- information about providing a log group for DataSync, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateTask.html#DataSync-CreateTask-request-CloudWatchLogGroupArn CloudWatchLogGroupArn>.
    -- If set to @OFF@, no logs are published. @BASIC@ publishes logs on errors
    -- for individual files transferred, and @TRANSFER@ publishes logs for
    -- every file or object that is transferred and integrity checked.
    logLevel :: Prelude.Maybe LogLevel,
    -- | A value that determines whether tasks should be queued before executing
    -- the tasks. If set to @ENABLED@, the tasks will be queued. The default is
    -- @ENABLED@.
    --
    -- If you use the same agent to run multiple tasks, you can enable the
    -- tasks to run in series. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/run-task.html#queue-task-execution Queueing task executions>.
    taskQueueing :: Prelude.Maybe TaskQueueing,
    -- | A value that determines whether DataSync should preserve the metadata of
    -- block and character devices in the source file system, and re-create the
    -- files with that device name and metadata on the destination. DataSync
    -- does not copy the contents of such devices, only the name and metadata.
    --
    -- DataSync can\'t sync the actual contents of such devices, because they
    -- are nonterminal and don\'t return an end-of-file (EOF) marker.
    --
    -- Default value: @NONE@
    --
    -- @NONE@: Ignore special devices (recommended).
    --
    -- @PRESERVE@: Preserve character and block device metadata. This option
    -- isn\'t currently supported for Amazon EFS.
    preserveDevices :: Prelude.Maybe PreserveDevices,
    -- | A value that determines whether files at the destination should be
    -- overwritten or preserved when copying files. If set to @NEVER@ a
    -- destination file will not be replaced by a source file, even if the
    -- destination file differs from the source file. If you modify files in
    -- the destination and you sync the files, you can use this value to
    -- protect against overwriting those changes.
    --
    -- Some storage classes have specific behaviors that can affect your S3
    -- storage cost. For detailed information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
    -- in the /DataSync User Guide/.
    overwriteMode :: Prelude.Maybe OverwriteMode,
    -- | A value that indicates the last time that a file was modified (that is,
    -- a file was written to) before the @PREPARING@ phase. This option is
    -- required for cases when you need to run the same task more than one
    -- time.
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
    -- | A value that determines whether DataSync transfers only the data and
    -- metadata that differ between the source and the destination location, or
    -- whether DataSync transfers all the content from the source, without
    -- comparing to the destination location.
    --
    -- @CHANGED@: DataSync copies only data or metadata that is new or
    -- different content from the source location to the destination location.
    --
    -- @ALL@: DataSync copies all source location content to the destination,
    -- without comparing to existing content on the destination.
    transferMode :: Prelude.Maybe TransferMode,
    -- | The POSIX user ID (UID) of the file\'s owner.
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
    -- | A value that determines whether a data integrity verification should be
    -- performed at the end of a task execution after all data and metadata
    -- have been transferred. For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-task.html Configure task settings>.
    --
    -- Default value: @POINT_IN_TIME_CONSISTENT@
    --
    -- @ONLY_FILES_TRANSFERRED@ (recommended): Perform verification only on
    -- files that were transferred.
    --
    -- @POINT_IN_TIME_CONSISTENT@: Scan the entire source and entire
    -- destination at the end of the transfer to verify that source and
    -- destination are fully synchronized. This option isn\'t supported when
    -- transferring to S3 Glacier Flexible Retrieval or S3 Glacier Deep Archive
    -- storage classes.
    --
    -- @NONE@: No additional verification is done at the end of the transfer,
    -- but all data transmissions are integrity-checked with checksum
    -- verification during the transfer.
    verifyMode :: Prelude.Maybe VerifyMode,
    -- | A value that specifies whether files in the destination that don\'t
    -- exist in the source file system should be preserved. This option can
    -- affect your storage cost. If your task deletes objects, you might incur
    -- minimum storage duration charges for certain storage classes. For
    -- detailed information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
    -- in the /DataSync User Guide/.
    --
    -- Default value: @PRESERVE@
    --
    -- @PRESERVE@: Ignore such destination files (recommended).
    --
    -- @REMOVE@: Delete destination files that aren’t present in the source.
    preserveDeletedFiles :: Prelude.Maybe PreserveDeletedFiles,
    -- | A file metadata value that shows the last time a file was accessed (that
    -- is, when the file was read or written to). If you set @Atime@ to
    -- @BEST_EFFORT@, DataSync attempts to preserve the original @Atime@
    -- attribute on all source files (that is, the version before the
    -- @PREPARING@ phase). However, @Atime@\'s behavior is not fully standard
    -- across platforms, so DataSync can only do this on a best-effort basis.
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
    -- | A value that determines which users or groups can access a file for a
    -- specific purpose such as reading, writing, or execution of the file.
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
    -- | A value that determines which components of the SMB security descriptor
    -- are copied from source to destination objects.
    --
    -- This value is only used for transfers between SMB and Amazon FSx for
    -- Windows File Server locations, or between two Amazon FSx for Windows
    -- File Server locations. For more information about how DataSync handles
    -- metadata, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html How DataSync Handles Metadata and Special Files>.
    --
    -- Default value: @OWNER_DACL@
    --
    -- @OWNER_DACL@: For each copied object, DataSync copies the following
    -- metadata:
    --
    -- -   Object owner.
    --
    -- -   NTFS discretionary access control lists (DACLs), which determine
    --     whether to grant access to an object.
    --
    -- When choosing this option, DataSync does NOT copy the NTFS system access
    -- control lists (SACLs), which are used by administrators to log attempts
    -- to access a secured object.
    --
    -- @OWNER_DACL_SACL@: For each copied object, DataSync copies the following
    -- metadata:
    --
    -- -   Object owner.
    --
    -- -   NTFS discretionary access control lists (DACLs), which determine
    --     whether to grant access to an object.
    --
    -- -   NTFS system access control lists (SACLs), which are used by
    --     administrators to log attempts to access a secured object.
    --
    -- Copying SACLs requires granting additional permissions to the Windows
    -- user that DataSync uses to access your SMB location. For information
    -- about choosing a user that ensures sufficient permissions to files,
    -- folders, and metadata, see <create-smb-location.html#SMBuser user>.
    --
    -- @NONE@: None of the SMB security descriptor components are copied.
    -- Destination objects are owned by the user that was provided for
    -- accessing the destination location. DACLs and SACLs are set based on the
    -- destination server’s configuration.
    securityDescriptorCopyFlags :: Prelude.Maybe SmbSecurityDescriptorCopyFlags,
    -- | A value that limits the bandwidth used by DataSync. For example, if you
    -- want DataSync to use a maximum of 1 MB, set this value to @1048576@
    -- (@=1024*1024@).
    bytesPerSecond :: Prelude.Maybe Prelude.Integer
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
-- 'objectTags', 'options_objectTags' - Specifies whether object tags are maintained when transferring between
-- object storage systems. If you want your DataSync task to ignore object
-- tags, specify the @NONE@ value.
--
-- Default Value: @PRESERVE@
--
-- 'gid', 'options_gid' - The POSIX group ID (GID) of the file\'s owners.
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
-- 'logLevel', 'options_logLevel' - A value that determines the type of logs that DataSync publishes to a
-- log stream in the Amazon CloudWatch log group that you provide. For more
-- information about providing a log group for DataSync, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateTask.html#DataSync-CreateTask-request-CloudWatchLogGroupArn CloudWatchLogGroupArn>.
-- If set to @OFF@, no logs are published. @BASIC@ publishes logs on errors
-- for individual files transferred, and @TRANSFER@ publishes logs for
-- every file or object that is transferred and integrity checked.
--
-- 'taskQueueing', 'options_taskQueueing' - A value that determines whether tasks should be queued before executing
-- the tasks. If set to @ENABLED@, the tasks will be queued. The default is
-- @ENABLED@.
--
-- If you use the same agent to run multiple tasks, you can enable the
-- tasks to run in series. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/run-task.html#queue-task-execution Queueing task executions>.
--
-- 'preserveDevices', 'options_preserveDevices' - A value that determines whether DataSync should preserve the metadata of
-- block and character devices in the source file system, and re-create the
-- files with that device name and metadata on the destination. DataSync
-- does not copy the contents of such devices, only the name and metadata.
--
-- DataSync can\'t sync the actual contents of such devices, because they
-- are nonterminal and don\'t return an end-of-file (EOF) marker.
--
-- Default value: @NONE@
--
-- @NONE@: Ignore special devices (recommended).
--
-- @PRESERVE@: Preserve character and block device metadata. This option
-- isn\'t currently supported for Amazon EFS.
--
-- 'overwriteMode', 'options_overwriteMode' - A value that determines whether files at the destination should be
-- overwritten or preserved when copying files. If set to @NEVER@ a
-- destination file will not be replaced by a source file, even if the
-- destination file differs from the source file. If you modify files in
-- the destination and you sync the files, you can use this value to
-- protect against overwriting those changes.
--
-- Some storage classes have specific behaviors that can affect your S3
-- storage cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- in the /DataSync User Guide/.
--
-- 'mtime', 'options_mtime' - A value that indicates the last time that a file was modified (that is,
-- a file was written to) before the @PREPARING@ phase. This option is
-- required for cases when you need to run the same task more than one
-- time.
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
-- 'transferMode', 'options_transferMode' - A value that determines whether DataSync transfers only the data and
-- metadata that differ between the source and the destination location, or
-- whether DataSync transfers all the content from the source, without
-- comparing to the destination location.
--
-- @CHANGED@: DataSync copies only data or metadata that is new or
-- different content from the source location to the destination location.
--
-- @ALL@: DataSync copies all source location content to the destination,
-- without comparing to existing content on the destination.
--
-- 'uid', 'options_uid' - The POSIX user ID (UID) of the file\'s owner.
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
-- 'verifyMode', 'options_verifyMode' - A value that determines whether a data integrity verification should be
-- performed at the end of a task execution after all data and metadata
-- have been transferred. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-task.html Configure task settings>.
--
-- Default value: @POINT_IN_TIME_CONSISTENT@
--
-- @ONLY_FILES_TRANSFERRED@ (recommended): Perform verification only on
-- files that were transferred.
--
-- @POINT_IN_TIME_CONSISTENT@: Scan the entire source and entire
-- destination at the end of the transfer to verify that source and
-- destination are fully synchronized. This option isn\'t supported when
-- transferring to S3 Glacier Flexible Retrieval or S3 Glacier Deep Archive
-- storage classes.
--
-- @NONE@: No additional verification is done at the end of the transfer,
-- but all data transmissions are integrity-checked with checksum
-- verification during the transfer.
--
-- 'preserveDeletedFiles', 'options_preserveDeletedFiles' - A value that specifies whether files in the destination that don\'t
-- exist in the source file system should be preserved. This option can
-- affect your storage cost. If your task deletes objects, you might incur
-- minimum storage duration charges for certain storage classes. For
-- detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- in the /DataSync User Guide/.
--
-- Default value: @PRESERVE@
--
-- @PRESERVE@: Ignore such destination files (recommended).
--
-- @REMOVE@: Delete destination files that aren’t present in the source.
--
-- 'atime', 'options_atime' - A file metadata value that shows the last time a file was accessed (that
-- is, when the file was read or written to). If you set @Atime@ to
-- @BEST_EFFORT@, DataSync attempts to preserve the original @Atime@
-- attribute on all source files (that is, the version before the
-- @PREPARING@ phase). However, @Atime@\'s behavior is not fully standard
-- across platforms, so DataSync can only do this on a best-effort basis.
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
-- 'posixPermissions', 'options_posixPermissions' - A value that determines which users or groups can access a file for a
-- specific purpose such as reading, writing, or execution of the file.
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
-- 'securityDescriptorCopyFlags', 'options_securityDescriptorCopyFlags' - A value that determines which components of the SMB security descriptor
-- are copied from source to destination objects.
--
-- This value is only used for transfers between SMB and Amazon FSx for
-- Windows File Server locations, or between two Amazon FSx for Windows
-- File Server locations. For more information about how DataSync handles
-- metadata, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html How DataSync Handles Metadata and Special Files>.
--
-- Default value: @OWNER_DACL@
--
-- @OWNER_DACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   Object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
-- When choosing this option, DataSync does NOT copy the NTFS system access
-- control lists (SACLs), which are used by administrators to log attempts
-- to access a secured object.
--
-- @OWNER_DACL_SACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   Object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
-- -   NTFS system access control lists (SACLs), which are used by
--     administrators to log attempts to access a secured object.
--
-- Copying SACLs requires granting additional permissions to the Windows
-- user that DataSync uses to access your SMB location. For information
-- about choosing a user that ensures sufficient permissions to files,
-- folders, and metadata, see <create-smb-location.html#SMBuser user>.
--
-- @NONE@: None of the SMB security descriptor components are copied.
-- Destination objects are owned by the user that was provided for
-- accessing the destination location. DACLs and SACLs are set based on the
-- destination server’s configuration.
--
-- 'bytesPerSecond', 'options_bytesPerSecond' - A value that limits the bandwidth used by DataSync. For example, if you
-- want DataSync to use a maximum of 1 MB, set this value to @1048576@
-- (@=1024*1024@).
newOptions ::
  Options
newOptions =
  Options'
    { objectTags = Prelude.Nothing,
      gid = Prelude.Nothing,
      logLevel = Prelude.Nothing,
      taskQueueing = Prelude.Nothing,
      preserveDevices = Prelude.Nothing,
      overwriteMode = Prelude.Nothing,
      mtime = Prelude.Nothing,
      transferMode = Prelude.Nothing,
      uid = Prelude.Nothing,
      verifyMode = Prelude.Nothing,
      preserveDeletedFiles = Prelude.Nothing,
      atime = Prelude.Nothing,
      posixPermissions = Prelude.Nothing,
      securityDescriptorCopyFlags = Prelude.Nothing,
      bytesPerSecond = Prelude.Nothing
    }

-- | Specifies whether object tags are maintained when transferring between
-- object storage systems. If you want your DataSync task to ignore object
-- tags, specify the @NONE@ value.
--
-- Default Value: @PRESERVE@
options_objectTags :: Lens.Lens' Options (Prelude.Maybe ObjectTags)
options_objectTags = Lens.lens (\Options' {objectTags} -> objectTags) (\s@Options' {} a -> s {objectTags = a} :: Options)

-- | The POSIX group ID (GID) of the file\'s owners.
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

-- | A value that determines the type of logs that DataSync publishes to a
-- log stream in the Amazon CloudWatch log group that you provide. For more
-- information about providing a log group for DataSync, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_CreateTask.html#DataSync-CreateTask-request-CloudWatchLogGroupArn CloudWatchLogGroupArn>.
-- If set to @OFF@, no logs are published. @BASIC@ publishes logs on errors
-- for individual files transferred, and @TRANSFER@ publishes logs for
-- every file or object that is transferred and integrity checked.
options_logLevel :: Lens.Lens' Options (Prelude.Maybe LogLevel)
options_logLevel = Lens.lens (\Options' {logLevel} -> logLevel) (\s@Options' {} a -> s {logLevel = a} :: Options)

-- | A value that determines whether tasks should be queued before executing
-- the tasks. If set to @ENABLED@, the tasks will be queued. The default is
-- @ENABLED@.
--
-- If you use the same agent to run multiple tasks, you can enable the
-- tasks to run in series. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/run-task.html#queue-task-execution Queueing task executions>.
options_taskQueueing :: Lens.Lens' Options (Prelude.Maybe TaskQueueing)
options_taskQueueing = Lens.lens (\Options' {taskQueueing} -> taskQueueing) (\s@Options' {} a -> s {taskQueueing = a} :: Options)

-- | A value that determines whether DataSync should preserve the metadata of
-- block and character devices in the source file system, and re-create the
-- files with that device name and metadata on the destination. DataSync
-- does not copy the contents of such devices, only the name and metadata.
--
-- DataSync can\'t sync the actual contents of such devices, because they
-- are nonterminal and don\'t return an end-of-file (EOF) marker.
--
-- Default value: @NONE@
--
-- @NONE@: Ignore special devices (recommended).
--
-- @PRESERVE@: Preserve character and block device metadata. This option
-- isn\'t currently supported for Amazon EFS.
options_preserveDevices :: Lens.Lens' Options (Prelude.Maybe PreserveDevices)
options_preserveDevices = Lens.lens (\Options' {preserveDevices} -> preserveDevices) (\s@Options' {} a -> s {preserveDevices = a} :: Options)

-- | A value that determines whether files at the destination should be
-- overwritten or preserved when copying files. If set to @NEVER@ a
-- destination file will not be replaced by a source file, even if the
-- destination file differs from the source file. If you modify files in
-- the destination and you sync the files, you can use this value to
-- protect against overwriting those changes.
--
-- Some storage classes have specific behaviors that can affect your S3
-- storage cost. For detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- in the /DataSync User Guide/.
options_overwriteMode :: Lens.Lens' Options (Prelude.Maybe OverwriteMode)
options_overwriteMode = Lens.lens (\Options' {overwriteMode} -> overwriteMode) (\s@Options' {} a -> s {overwriteMode = a} :: Options)

-- | A value that indicates the last time that a file was modified (that is,
-- a file was written to) before the @PREPARING@ phase. This option is
-- required for cases when you need to run the same task more than one
-- time.
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

-- | A value that determines whether DataSync transfers only the data and
-- metadata that differ between the source and the destination location, or
-- whether DataSync transfers all the content from the source, without
-- comparing to the destination location.
--
-- @CHANGED@: DataSync copies only data or metadata that is new or
-- different content from the source location to the destination location.
--
-- @ALL@: DataSync copies all source location content to the destination,
-- without comparing to existing content on the destination.
options_transferMode :: Lens.Lens' Options (Prelude.Maybe TransferMode)
options_transferMode = Lens.lens (\Options' {transferMode} -> transferMode) (\s@Options' {} a -> s {transferMode = a} :: Options)

-- | The POSIX user ID (UID) of the file\'s owner.
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

-- | A value that determines whether a data integrity verification should be
-- performed at the end of a task execution after all data and metadata
-- have been transferred. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-task.html Configure task settings>.
--
-- Default value: @POINT_IN_TIME_CONSISTENT@
--
-- @ONLY_FILES_TRANSFERRED@ (recommended): Perform verification only on
-- files that were transferred.
--
-- @POINT_IN_TIME_CONSISTENT@: Scan the entire source and entire
-- destination at the end of the transfer to verify that source and
-- destination are fully synchronized. This option isn\'t supported when
-- transferring to S3 Glacier Flexible Retrieval or S3 Glacier Deep Archive
-- storage classes.
--
-- @NONE@: No additional verification is done at the end of the transfer,
-- but all data transmissions are integrity-checked with checksum
-- verification during the transfer.
options_verifyMode :: Lens.Lens' Options (Prelude.Maybe VerifyMode)
options_verifyMode = Lens.lens (\Options' {verifyMode} -> verifyMode) (\s@Options' {} a -> s {verifyMode = a} :: Options)

-- | A value that specifies whether files in the destination that don\'t
-- exist in the source file system should be preserved. This option can
-- affect your storage cost. If your task deletes objects, you might incur
-- minimum storage duration charges for certain storage classes. For
-- detailed information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-s3-location.html#using-storage-classes Considerations when working with Amazon S3 storage classes in DataSync>
-- in the /DataSync User Guide/.
--
-- Default value: @PRESERVE@
--
-- @PRESERVE@: Ignore such destination files (recommended).
--
-- @REMOVE@: Delete destination files that aren’t present in the source.
options_preserveDeletedFiles :: Lens.Lens' Options (Prelude.Maybe PreserveDeletedFiles)
options_preserveDeletedFiles = Lens.lens (\Options' {preserveDeletedFiles} -> preserveDeletedFiles) (\s@Options' {} a -> s {preserveDeletedFiles = a} :: Options)

-- | A file metadata value that shows the last time a file was accessed (that
-- is, when the file was read or written to). If you set @Atime@ to
-- @BEST_EFFORT@, DataSync attempts to preserve the original @Atime@
-- attribute on all source files (that is, the version before the
-- @PREPARING@ phase). However, @Atime@\'s behavior is not fully standard
-- across platforms, so DataSync can only do this on a best-effort basis.
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

-- | A value that determines which users or groups can access a file for a
-- specific purpose such as reading, writing, or execution of the file.
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

-- | A value that determines which components of the SMB security descriptor
-- are copied from source to destination objects.
--
-- This value is only used for transfers between SMB and Amazon FSx for
-- Windows File Server locations, or between two Amazon FSx for Windows
-- File Server locations. For more information about how DataSync handles
-- metadata, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/special-files.html How DataSync Handles Metadata and Special Files>.
--
-- Default value: @OWNER_DACL@
--
-- @OWNER_DACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   Object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
-- When choosing this option, DataSync does NOT copy the NTFS system access
-- control lists (SACLs), which are used by administrators to log attempts
-- to access a secured object.
--
-- @OWNER_DACL_SACL@: For each copied object, DataSync copies the following
-- metadata:
--
-- -   Object owner.
--
-- -   NTFS discretionary access control lists (DACLs), which determine
--     whether to grant access to an object.
--
-- -   NTFS system access control lists (SACLs), which are used by
--     administrators to log attempts to access a secured object.
--
-- Copying SACLs requires granting additional permissions to the Windows
-- user that DataSync uses to access your SMB location. For information
-- about choosing a user that ensures sufficient permissions to files,
-- folders, and metadata, see <create-smb-location.html#SMBuser user>.
--
-- @NONE@: None of the SMB security descriptor components are copied.
-- Destination objects are owned by the user that was provided for
-- accessing the destination location. DACLs and SACLs are set based on the
-- destination server’s configuration.
options_securityDescriptorCopyFlags :: Lens.Lens' Options (Prelude.Maybe SmbSecurityDescriptorCopyFlags)
options_securityDescriptorCopyFlags = Lens.lens (\Options' {securityDescriptorCopyFlags} -> securityDescriptorCopyFlags) (\s@Options' {} a -> s {securityDescriptorCopyFlags = a} :: Options)

-- | A value that limits the bandwidth used by DataSync. For example, if you
-- want DataSync to use a maximum of 1 MB, set this value to @1048576@
-- (@=1024*1024@).
options_bytesPerSecond :: Lens.Lens' Options (Prelude.Maybe Prelude.Integer)
options_bytesPerSecond = Lens.lens (\Options' {bytesPerSecond} -> bytesPerSecond) (\s@Options' {} a -> s {bytesPerSecond = a} :: Options)

instance Data.FromJSON Options where
  parseJSON =
    Data.withObject
      "Options"
      ( \x ->
          Options'
            Prelude.<$> (x Data..:? "ObjectTags")
            Prelude.<*> (x Data..:? "Gid")
            Prelude.<*> (x Data..:? "LogLevel")
            Prelude.<*> (x Data..:? "TaskQueueing")
            Prelude.<*> (x Data..:? "PreserveDevices")
            Prelude.<*> (x Data..:? "OverwriteMode")
            Prelude.<*> (x Data..:? "Mtime")
            Prelude.<*> (x Data..:? "TransferMode")
            Prelude.<*> (x Data..:? "Uid")
            Prelude.<*> (x Data..:? "VerifyMode")
            Prelude.<*> (x Data..:? "PreserveDeletedFiles")
            Prelude.<*> (x Data..:? "Atime")
            Prelude.<*> (x Data..:? "PosixPermissions")
            Prelude.<*> (x Data..:? "SecurityDescriptorCopyFlags")
            Prelude.<*> (x Data..:? "BytesPerSecond")
      )

instance Prelude.Hashable Options where
  hashWithSalt _salt Options' {..} =
    _salt `Prelude.hashWithSalt` objectTags
      `Prelude.hashWithSalt` gid
      `Prelude.hashWithSalt` logLevel
      `Prelude.hashWithSalt` taskQueueing
      `Prelude.hashWithSalt` preserveDevices
      `Prelude.hashWithSalt` overwriteMode
      `Prelude.hashWithSalt` mtime
      `Prelude.hashWithSalt` transferMode
      `Prelude.hashWithSalt` uid
      `Prelude.hashWithSalt` verifyMode
      `Prelude.hashWithSalt` preserveDeletedFiles
      `Prelude.hashWithSalt` atime
      `Prelude.hashWithSalt` posixPermissions
      `Prelude.hashWithSalt` securityDescriptorCopyFlags
      `Prelude.hashWithSalt` bytesPerSecond

instance Prelude.NFData Options where
  rnf Options' {..} =
    Prelude.rnf objectTags
      `Prelude.seq` Prelude.rnf gid
      `Prelude.seq` Prelude.rnf logLevel
      `Prelude.seq` Prelude.rnf taskQueueing
      `Prelude.seq` Prelude.rnf preserveDevices
      `Prelude.seq` Prelude.rnf overwriteMode
      `Prelude.seq` Prelude.rnf mtime
      `Prelude.seq` Prelude.rnf transferMode
      `Prelude.seq` Prelude.rnf uid
      `Prelude.seq` Prelude.rnf verifyMode
      `Prelude.seq` Prelude.rnf preserveDeletedFiles
      `Prelude.seq` Prelude.rnf atime
      `Prelude.seq` Prelude.rnf posixPermissions
      `Prelude.seq` Prelude.rnf securityDescriptorCopyFlags
      `Prelude.seq` Prelude.rnf bytesPerSecond

instance Data.ToJSON Options where
  toJSON Options' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ObjectTags" Data..=) Prelude.<$> objectTags,
            ("Gid" Data..=) Prelude.<$> gid,
            ("LogLevel" Data..=) Prelude.<$> logLevel,
            ("TaskQueueing" Data..=) Prelude.<$> taskQueueing,
            ("PreserveDevices" Data..=)
              Prelude.<$> preserveDevices,
            ("OverwriteMode" Data..=) Prelude.<$> overwriteMode,
            ("Mtime" Data..=) Prelude.<$> mtime,
            ("TransferMode" Data..=) Prelude.<$> transferMode,
            ("Uid" Data..=) Prelude.<$> uid,
            ("VerifyMode" Data..=) Prelude.<$> verifyMode,
            ("PreserveDeletedFiles" Data..=)
              Prelude.<$> preserveDeletedFiles,
            ("Atime" Data..=) Prelude.<$> atime,
            ("PosixPermissions" Data..=)
              Prelude.<$> posixPermissions,
            ("SecurityDescriptorCopyFlags" Data..=)
              Prelude.<$> securityDescriptorCopyFlags,
            ("BytesPerSecond" Data..=)
              Prelude.<$> bytesPerSecond
          ]
      )
