{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EFS.CreateFileSystem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty file system. The operation requires a creation
-- token in the request that Amazon EFS uses to ensure idempotent creation
-- (calling the operation with same creation token has no effect). If a
-- file system does not currently exist that is owned by the caller\'s
-- Amazon Web Services account with the specified creation token, this
-- operation does the following:
--
-- -   Creates a new, empty file system. The file system will have an
--     Amazon EFS assigned ID, and an initial lifecycle state @creating@.
--
-- -   Returns with the description of the created file system.
--
-- Otherwise, this operation returns a @FileSystemAlreadyExists@ error with
-- the ID of the existing file system.
--
-- For basic use cases, you can use a randomly generated UUID for the
-- creation token.
--
-- The idempotent operation allows you to retry a @CreateFileSystem@ call
-- without risk of creating an extra file system. This can happen when an
-- initial call fails in a way that leaves it uncertain whether or not a
-- file system was actually created. An example might be that a transport
-- level timeout occurred or your connection was reset. As long as you use
-- the same creation token, if the initial call had succeeded in creating a
-- file system, the client can learn of its existence from the
-- @FileSystemAlreadyExists@ error.
--
-- For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/creating-using-create-fs.html#creating-using-create-fs-part1 Creating a file system>
-- in the /Amazon EFS User Guide/.
--
-- The @CreateFileSystem@ call returns while the file system\'s lifecycle
-- state is still @creating@. You can check the file system creation status
-- by calling the DescribeFileSystems operation, which among other things
-- returns the file system state.
--
-- This operation accepts an optional @PerformanceMode@ parameter that you
-- choose for your file system. We recommend @generalPurpose@ performance
-- mode for most file systems. File systems using the @maxIO@ performance
-- mode can scale to higher levels of aggregate throughput and operations
-- per second with a tradeoff of slightly higher latencies for most file
-- operations. The performance mode can\'t be changed after the file system
-- has been created. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html Amazon EFS performance modes>.
--
-- You can set the throughput mode for the file system using the
-- @ThroughputMode@ parameter.
--
-- After the file system is fully created, Amazon EFS sets its lifecycle
-- state to @available@, at which point you can create one or more mount
-- targets for the file system in your VPC. For more information, see
-- CreateMountTarget. You mount your Amazon EFS file system on an EC2
-- instances in your VPC by using the mount target. For more information,
-- see
-- <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works>.
--
-- This operation requires permissions for the
-- @elasticfilesystem:CreateFileSystem@ action.
module Amazonka.EFS.CreateFileSystem
  ( -- * Creating a Request
    CreateFileSystem (..),
    newCreateFileSystem,

    -- * Request Lenses
    createFileSystem_availabilityZoneName,
    createFileSystem_backup,
    createFileSystem_encrypted,
    createFileSystem_kmsKeyId,
    createFileSystem_performanceMode,
    createFileSystem_provisionedThroughputInMibps,
    createFileSystem_tags,
    createFileSystem_throughputMode,
    createFileSystem_creationToken,

    -- * Destructuring the Response
    FileSystemDescription (..),
    newFileSystemDescription,

    -- * Response Lenses
    fileSystemDescription_availabilityZoneId,
    fileSystemDescription_availabilityZoneName,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_throughputMode,
    fileSystemDescription_ownerId,
    fileSystemDescription_creationToken,
    fileSystemDescription_fileSystemId,
    fileSystemDescription_creationTime,
    fileSystemDescription_lifeCycleState,
    fileSystemDescription_numberOfMountTargets,
    fileSystemDescription_sizeInBytes,
    fileSystemDescription_performanceMode,
    fileSystemDescription_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFileSystem' smart constructor.
data CreateFileSystem = CreateFileSystem'
  { -- | Used to create a file system that uses One Zone storage classes. It
    -- specifies the Amazon Web Services Availability Zone in which to create
    -- the file system. Use the format @us-east-1a@ to specify the Availability
    -- Zone. For more information about One Zone storage classes, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Using EFS storage classes>
    -- in the /Amazon EFS User Guide/.
    --
    -- One Zone storage classes are not available in all Availability Zones in
    -- Amazon Web Services Regions where Amazon EFS is available.
    availabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether automatic backups are enabled on the file system that
    -- you are creating. Set the value to @true@ to enable automatic backups.
    -- If you are creating a file system that uses One Zone storage classes,
    -- automatic backups are enabled by default. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#automatic-backups Automatic backups>
    -- in the /Amazon EFS User Guide/.
    --
    -- Default is @false@. However, if you specify an @AvailabilityZoneName@,
    -- the default is @true@.
    --
    -- Backup is not available in all Amazon Web Services Regions where Amazon
    -- EFS is available.
    backup :: Prelude.Maybe Prelude.Bool,
    -- | A Boolean value that, if true, creates an encrypted file system. When
    -- creating an encrypted file system, you have the option of specifying an
    -- existing Key Management Service key (KMS key). If you don\'t specify a
    -- KMS key, then the default KMS key for Amazon EFS,
    -- @\/aws\/elasticfilesystem@, is used to protect the encrypted file
    -- system.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the KMS key that you want to use to protect the encrypted file
    -- system. This parameter is required only if you want to use a non-default
    -- KMS key. If this parameter is not specified, the default KMS key for
    -- Amazon EFS is used. You can specify a KMS key ID using the following
    -- formats:
    --
    -- -   Key ID - A unique identifier of the key, for example
    --     @1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- -   ARN - An Amazon Resource Name (ARN) for the key, for example
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    --
    -- -   Key alias - A previously created display name for a key, for example
    --     @alias\/projectKey1@.
    --
    -- -   Key alias ARN - An ARN for a key alias, for example
    --     @arn:aws:kms:us-west-2:444455556666:alias\/projectKey1@.
    --
    -- If you use @KmsKeyId@, you must set the
    -- CreateFileSystemRequest$Encrypted parameter to true.
    --
    -- EFS accepts only symmetric KMS keys. You cannot use asymmetric KMS keys
    -- with Amazon EFS file systems.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The performance mode of the file system. We recommend @generalPurpose@
    -- performance mode for most file systems. File systems using the @maxIO@
    -- performance mode can scale to higher levels of aggregate throughput and
    -- operations per second with a tradeoff of slightly higher latencies for
    -- most file operations. The performance mode can\'t be changed after the
    -- file system has been created.
    --
    -- The @maxIO@ mode is not supported on file systems using One Zone storage
    -- classes.
    performanceMode :: Prelude.Maybe PerformanceMode,
    -- | The throughput, measured in MiB\/s, that you want to provision for a
    -- file system that you\'re creating. Valid values are 1-1024. Required if
    -- @ThroughputMode@ is set to @provisioned@. The upper limit for throughput
    -- is 1024 MiB\/s. To increase this limit, contact Amazon Web Services
    -- Support. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS quotas that you can increase>
    -- in the /Amazon EFS User Guide/.
    provisionedThroughputInMibps :: Prelude.Maybe Prelude.Double,
    -- | Use to create one or more tags associated with the file system. Each tag
    -- is a user-defined key-value pair. Name your file system on creation by
    -- including a @\"Key\":\"Name\",\"Value\":\"{value}\"@ key-value pair.
    -- Each key must be unique. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | Specifies the throughput mode for the file system. The mode can be
    -- @bursting@, @provisioned@, or @elastic@. If you set @ThroughputMode@ to
    -- @provisioned@, you must also set a value for
    -- @ProvisionedThroughputInMibps@. After you create the file system, you
    -- can decrease your file system\'s throughput in Provisioned Throughput
    -- mode or change between the throughput modes, with certain time
    -- restrictions. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying throughput with provisioned mode>
    -- in the /Amazon EFS User Guide/.
    --
    -- Default is @bursting@.
    throughputMode :: Prelude.Maybe ThroughputMode,
    -- | A string of up to 64 ASCII characters. Amazon EFS uses this to ensure
    -- idempotent creation.
    creationToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneName', 'createFileSystem_availabilityZoneName' - Used to create a file system that uses One Zone storage classes. It
-- specifies the Amazon Web Services Availability Zone in which to create
-- the file system. Use the format @us-east-1a@ to specify the Availability
-- Zone. For more information about One Zone storage classes, see
-- <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Using EFS storage classes>
-- in the /Amazon EFS User Guide/.
--
-- One Zone storage classes are not available in all Availability Zones in
-- Amazon Web Services Regions where Amazon EFS is available.
--
-- 'backup', 'createFileSystem_backup' - Specifies whether automatic backups are enabled on the file system that
-- you are creating. Set the value to @true@ to enable automatic backups.
-- If you are creating a file system that uses One Zone storage classes,
-- automatic backups are enabled by default. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#automatic-backups Automatic backups>
-- in the /Amazon EFS User Guide/.
--
-- Default is @false@. However, if you specify an @AvailabilityZoneName@,
-- the default is @true@.
--
-- Backup is not available in all Amazon Web Services Regions where Amazon
-- EFS is available.
--
-- 'encrypted', 'createFileSystem_encrypted' - A Boolean value that, if true, creates an encrypted file system. When
-- creating an encrypted file system, you have the option of specifying an
-- existing Key Management Service key (KMS key). If you don\'t specify a
-- KMS key, then the default KMS key for Amazon EFS,
-- @\/aws\/elasticfilesystem@, is used to protect the encrypted file
-- system.
--
-- 'kmsKeyId', 'createFileSystem_kmsKeyId' - The ID of the KMS key that you want to use to protect the encrypted file
-- system. This parameter is required only if you want to use a non-default
-- KMS key. If this parameter is not specified, the default KMS key for
-- Amazon EFS is used. You can specify a KMS key ID using the following
-- formats:
--
-- -   Key ID - A unique identifier of the key, for example
--     @1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   ARN - An Amazon Resource Name (ARN) for the key, for example
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   Key alias - A previously created display name for a key, for example
--     @alias\/projectKey1@.
--
-- -   Key alias ARN - An ARN for a key alias, for example
--     @arn:aws:kms:us-west-2:444455556666:alias\/projectKey1@.
--
-- If you use @KmsKeyId@, you must set the
-- CreateFileSystemRequest$Encrypted parameter to true.
--
-- EFS accepts only symmetric KMS keys. You cannot use asymmetric KMS keys
-- with Amazon EFS file systems.
--
-- 'performanceMode', 'createFileSystem_performanceMode' - The performance mode of the file system. We recommend @generalPurpose@
-- performance mode for most file systems. File systems using the @maxIO@
-- performance mode can scale to higher levels of aggregate throughput and
-- operations per second with a tradeoff of slightly higher latencies for
-- most file operations. The performance mode can\'t be changed after the
-- file system has been created.
--
-- The @maxIO@ mode is not supported on file systems using One Zone storage
-- classes.
--
-- 'provisionedThroughputInMibps', 'createFileSystem_provisionedThroughputInMibps' - The throughput, measured in MiB\/s, that you want to provision for a
-- file system that you\'re creating. Valid values are 1-1024. Required if
-- @ThroughputMode@ is set to @provisioned@. The upper limit for throughput
-- is 1024 MiB\/s. To increase this limit, contact Amazon Web Services
-- Support. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS quotas that you can increase>
-- in the /Amazon EFS User Guide/.
--
-- 'tags', 'createFileSystem_tags' - Use to create one or more tags associated with the file system. Each tag
-- is a user-defined key-value pair. Name your file system on creation by
-- including a @\"Key\":\"Name\",\"Value\":\"{value}\"@ key-value pair.
-- Each key must be unique. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'throughputMode', 'createFileSystem_throughputMode' - Specifies the throughput mode for the file system. The mode can be
-- @bursting@, @provisioned@, or @elastic@. If you set @ThroughputMode@ to
-- @provisioned@, you must also set a value for
-- @ProvisionedThroughputInMibps@. After you create the file system, you
-- can decrease your file system\'s throughput in Provisioned Throughput
-- mode or change between the throughput modes, with certain time
-- restrictions. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying throughput with provisioned mode>
-- in the /Amazon EFS User Guide/.
--
-- Default is @bursting@.
--
-- 'creationToken', 'createFileSystem_creationToken' - A string of up to 64 ASCII characters. Amazon EFS uses this to ensure
-- idempotent creation.
newCreateFileSystem ::
  -- | 'creationToken'
  Prelude.Text ->
  CreateFileSystem
newCreateFileSystem pCreationToken_ =
  CreateFileSystem'
    { availabilityZoneName =
        Prelude.Nothing,
      backup = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      performanceMode = Prelude.Nothing,
      provisionedThroughputInMibps = Prelude.Nothing,
      tags = Prelude.Nothing,
      throughputMode = Prelude.Nothing,
      creationToken = pCreationToken_
    }

-- | Used to create a file system that uses One Zone storage classes. It
-- specifies the Amazon Web Services Availability Zone in which to create
-- the file system. Use the format @us-east-1a@ to specify the Availability
-- Zone. For more information about One Zone storage classes, see
-- <https://docs.aws.amazon.com/efs/latest/ug/storage-classes.html Using EFS storage classes>
-- in the /Amazon EFS User Guide/.
--
-- One Zone storage classes are not available in all Availability Zones in
-- Amazon Web Services Regions where Amazon EFS is available.
createFileSystem_availabilityZoneName :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Text)
createFileSystem_availabilityZoneName = Lens.lens (\CreateFileSystem' {availabilityZoneName} -> availabilityZoneName) (\s@CreateFileSystem' {} a -> s {availabilityZoneName = a} :: CreateFileSystem)

-- | Specifies whether automatic backups are enabled on the file system that
-- you are creating. Set the value to @true@ to enable automatic backups.
-- If you are creating a file system that uses One Zone storage classes,
-- automatic backups are enabled by default. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/awsbackup.html#automatic-backups Automatic backups>
-- in the /Amazon EFS User Guide/.
--
-- Default is @false@. However, if you specify an @AvailabilityZoneName@,
-- the default is @true@.
--
-- Backup is not available in all Amazon Web Services Regions where Amazon
-- EFS is available.
createFileSystem_backup :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Bool)
createFileSystem_backup = Lens.lens (\CreateFileSystem' {backup} -> backup) (\s@CreateFileSystem' {} a -> s {backup = a} :: CreateFileSystem)

-- | A Boolean value that, if true, creates an encrypted file system. When
-- creating an encrypted file system, you have the option of specifying an
-- existing Key Management Service key (KMS key). If you don\'t specify a
-- KMS key, then the default KMS key for Amazon EFS,
-- @\/aws\/elasticfilesystem@, is used to protect the encrypted file
-- system.
createFileSystem_encrypted :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Bool)
createFileSystem_encrypted = Lens.lens (\CreateFileSystem' {encrypted} -> encrypted) (\s@CreateFileSystem' {} a -> s {encrypted = a} :: CreateFileSystem)

-- | The ID of the KMS key that you want to use to protect the encrypted file
-- system. This parameter is required only if you want to use a non-default
-- KMS key. If this parameter is not specified, the default KMS key for
-- Amazon EFS is used. You can specify a KMS key ID using the following
-- formats:
--
-- -   Key ID - A unique identifier of the key, for example
--     @1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   ARN - An Amazon Resource Name (ARN) for the key, for example
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- -   Key alias - A previously created display name for a key, for example
--     @alias\/projectKey1@.
--
-- -   Key alias ARN - An ARN for a key alias, for example
--     @arn:aws:kms:us-west-2:444455556666:alias\/projectKey1@.
--
-- If you use @KmsKeyId@, you must set the
-- CreateFileSystemRequest$Encrypted parameter to true.
--
-- EFS accepts only symmetric KMS keys. You cannot use asymmetric KMS keys
-- with Amazon EFS file systems.
createFileSystem_kmsKeyId :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Text)
createFileSystem_kmsKeyId = Lens.lens (\CreateFileSystem' {kmsKeyId} -> kmsKeyId) (\s@CreateFileSystem' {} a -> s {kmsKeyId = a} :: CreateFileSystem)

-- | The performance mode of the file system. We recommend @generalPurpose@
-- performance mode for most file systems. File systems using the @maxIO@
-- performance mode can scale to higher levels of aggregate throughput and
-- operations per second with a tradeoff of slightly higher latencies for
-- most file operations. The performance mode can\'t be changed after the
-- file system has been created.
--
-- The @maxIO@ mode is not supported on file systems using One Zone storage
-- classes.
createFileSystem_performanceMode :: Lens.Lens' CreateFileSystem (Prelude.Maybe PerformanceMode)
createFileSystem_performanceMode = Lens.lens (\CreateFileSystem' {performanceMode} -> performanceMode) (\s@CreateFileSystem' {} a -> s {performanceMode = a} :: CreateFileSystem)

-- | The throughput, measured in MiB\/s, that you want to provision for a
-- file system that you\'re creating. Valid values are 1-1024. Required if
-- @ThroughputMode@ is set to @provisioned@. The upper limit for throughput
-- is 1024 MiB\/s. To increase this limit, contact Amazon Web Services
-- Support. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS quotas that you can increase>
-- in the /Amazon EFS User Guide/.
createFileSystem_provisionedThroughputInMibps :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Double)
createFileSystem_provisionedThroughputInMibps = Lens.lens (\CreateFileSystem' {provisionedThroughputInMibps} -> provisionedThroughputInMibps) (\s@CreateFileSystem' {} a -> s {provisionedThroughputInMibps = a} :: CreateFileSystem)

-- | Use to create one or more tags associated with the file system. Each tag
-- is a user-defined key-value pair. Name your file system on creation by
-- including a @\"Key\":\"Name\",\"Value\":\"{value}\"@ key-value pair.
-- Each key must be unique. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
createFileSystem_tags :: Lens.Lens' CreateFileSystem (Prelude.Maybe [Tag])
createFileSystem_tags = Lens.lens (\CreateFileSystem' {tags} -> tags) (\s@CreateFileSystem' {} a -> s {tags = a} :: CreateFileSystem) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the throughput mode for the file system. The mode can be
-- @bursting@, @provisioned@, or @elastic@. If you set @ThroughputMode@ to
-- @provisioned@, you must also set a value for
-- @ProvisionedThroughputInMibps@. After you create the file system, you
-- can decrease your file system\'s throughput in Provisioned Throughput
-- mode or change between the throughput modes, with certain time
-- restrictions. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying throughput with provisioned mode>
-- in the /Amazon EFS User Guide/.
--
-- Default is @bursting@.
createFileSystem_throughputMode :: Lens.Lens' CreateFileSystem (Prelude.Maybe ThroughputMode)
createFileSystem_throughputMode = Lens.lens (\CreateFileSystem' {throughputMode} -> throughputMode) (\s@CreateFileSystem' {} a -> s {throughputMode = a} :: CreateFileSystem)

-- | A string of up to 64 ASCII characters. Amazon EFS uses this to ensure
-- idempotent creation.
createFileSystem_creationToken :: Lens.Lens' CreateFileSystem Prelude.Text
createFileSystem_creationToken = Lens.lens (\CreateFileSystem' {creationToken} -> creationToken) (\s@CreateFileSystem' {} a -> s {creationToken = a} :: CreateFileSystem)

instance Core.AWSRequest CreateFileSystem where
  type
    AWSResponse CreateFileSystem =
      FileSystemDescription
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateFileSystem where
  hashWithSalt _salt CreateFileSystem' {..} =
    _salt `Prelude.hashWithSalt` availabilityZoneName
      `Prelude.hashWithSalt` backup
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` performanceMode
      `Prelude.hashWithSalt` provisionedThroughputInMibps
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` throughputMode
      `Prelude.hashWithSalt` creationToken

instance Prelude.NFData CreateFileSystem where
  rnf CreateFileSystem' {..} =
    Prelude.rnf availabilityZoneName
      `Prelude.seq` Prelude.rnf backup
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf performanceMode
      `Prelude.seq` Prelude.rnf provisionedThroughputInMibps
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf throughputMode
      `Prelude.seq` Prelude.rnf creationToken

instance Data.ToHeaders CreateFileSystem where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateFileSystem where
  toJSON CreateFileSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AvailabilityZoneName" Data..=)
              Prelude.<$> availabilityZoneName,
            ("Backup" Data..=) Prelude.<$> backup,
            ("Encrypted" Data..=) Prelude.<$> encrypted,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("PerformanceMode" Data..=)
              Prelude.<$> performanceMode,
            ("ProvisionedThroughputInMibps" Data..=)
              Prelude.<$> provisionedThroughputInMibps,
            ("Tags" Data..=) Prelude.<$> tags,
            ("ThroughputMode" Data..=)
              Prelude.<$> throughputMode,
            Prelude.Just
              ("CreationToken" Data..= creationToken)
          ]
      )

instance Data.ToPath CreateFileSystem where
  toPath = Prelude.const "/2015-02-01/file-systems"

instance Data.ToQuery CreateFileSystem where
  toQuery = Prelude.const Prelude.mempty
