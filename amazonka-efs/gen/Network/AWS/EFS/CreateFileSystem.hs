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
-- Module      : Network.AWS.EFS.CreateFileSystem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty file system. The operation requires a creation
-- token in the request that Amazon EFS uses to ensure idempotent creation
-- (calling the operation with same creation token has no effect). If a
-- file system does not currently exist that is owned by the caller\'s AWS
-- account with the specified creation token, this operation does the
-- following:
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
-- The @CreateFileSystem@ call returns while the file system\'s lifecycle
-- state is still @creating@. You can check the file system creation status
-- by calling the DescribeFileSystems operation, which among other things
-- returns the file system state.
--
-- This operation also takes an optional @PerformanceMode@ parameter that
-- you choose for your file system. We recommend @generalPurpose@
-- performance mode for most file systems. File systems using the @maxIO@
-- performance mode can scale to higher levels of aggregate throughput and
-- operations per second with a tradeoff of slightly higher latencies for
-- most file operations. The performance mode can\'t be changed after the
-- file system has been created. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html Amazon EFS: Performance Modes>.
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
module Network.AWS.EFS.CreateFileSystem
  ( -- * Creating a Request
    CreateFileSystem (..),
    newCreateFileSystem,

    -- * Request Lenses
    createFileSystem_throughputMode,
    createFileSystem_encrypted,
    createFileSystem_provisionedThroughputInMibps,
    createFileSystem_kmsKeyId,
    createFileSystem_tags,
    createFileSystem_performanceMode,
    createFileSystem_creationToken,

    -- * Destructuring the Response
    FileSystemDescription (..),
    newFileSystemDescription,

    -- * Response Lenses
    fileSystemDescription_throughputMode,
    fileSystemDescription_encrypted,
    fileSystemDescription_fileSystemArn,
    fileSystemDescription_provisionedThroughputInMibps,
    fileSystemDescription_kmsKeyId,
    fileSystemDescription_name,
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

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFileSystem' smart constructor.
data CreateFileSystem = CreateFileSystem'
  { -- | The throughput mode for the file system to be created. There are two
    -- throughput modes to choose from for your file system: @bursting@ and
    -- @provisioned@. If you set @ThroughputMode@ to @provisioned@, you must
    -- also set a value for @ProvisionedThroughPutInMibps@. You can decrease
    -- your file system\'s throughput in Provisioned Throughput mode or change
    -- between the throughput modes as long as it’s been more than 24 hours
    -- since the last decrease or throughput mode change. For more, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode>
    -- in the /Amazon EFS User Guide./
    throughputMode :: Prelude.Maybe ThroughputMode,
    -- | A Boolean value that, if true, creates an encrypted file system. When
    -- creating an encrypted file system, you have the option of specifying
    -- CreateFileSystemRequest$KmsKeyId for an existing AWS Key Management
    -- Service (AWS KMS) customer master key (CMK). If you don\'t specify a
    -- CMK, then the default CMK for Amazon EFS, @\/aws\/elasticfilesystem@, is
    -- used to protect the encrypted file system.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The throughput, measured in MiB\/s, that you want to provision for a
    -- file system that you\'re creating. Valid values are 1-1024. Required if
    -- @ThroughputMode@ is set to @provisioned@. The upper limit for throughput
    -- is 1024 MiB\/s. You can get this limit increased by contacting AWS
    -- Support. For more information, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase>
    -- in the /Amazon EFS User Guide./
    provisionedThroughputInMibps :: Prelude.Maybe Prelude.Double,
    -- | The ID of the AWS KMS CMK to be used to protect the encrypted file
    -- system. This parameter is only required if you want to use a nondefault
    -- CMK. If this parameter is not specified, the default CMK for Amazon EFS
    -- is used. This ID can be in one of the following formats:
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
    -- If @KmsKeyId@ is specified, the CreateFileSystemRequest$Encrypted
    -- parameter must be set to true.
    --
    -- EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS
    -- file systems.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies to create one or more tags associated with the
    -- file system. Each tag is a user-defined key-value pair. Name your file
    -- system on creation by including a
    -- @\"Key\":\"Name\",\"Value\":\"{value}\"@ key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | The performance mode of the file system. We recommend @generalPurpose@
    -- performance mode for most file systems. File systems using the @maxIO@
    -- performance mode can scale to higher levels of aggregate throughput and
    -- operations per second with a tradeoff of slightly higher latencies for
    -- most file operations. The performance mode can\'t be changed after the
    -- file system has been created.
    performanceMode :: Prelude.Maybe PerformanceMode,
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
-- 'throughputMode', 'createFileSystem_throughputMode' - The throughput mode for the file system to be created. There are two
-- throughput modes to choose from for your file system: @bursting@ and
-- @provisioned@. If you set @ThroughputMode@ to @provisioned@, you must
-- also set a value for @ProvisionedThroughPutInMibps@. You can decrease
-- your file system\'s throughput in Provisioned Throughput mode or change
-- between the throughput modes as long as it’s been more than 24 hours
-- since the last decrease or throughput mode change. For more, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode>
-- in the /Amazon EFS User Guide./
--
-- 'encrypted', 'createFileSystem_encrypted' - A Boolean value that, if true, creates an encrypted file system. When
-- creating an encrypted file system, you have the option of specifying
-- CreateFileSystemRequest$KmsKeyId for an existing AWS Key Management
-- Service (AWS KMS) customer master key (CMK). If you don\'t specify a
-- CMK, then the default CMK for Amazon EFS, @\/aws\/elasticfilesystem@, is
-- used to protect the encrypted file system.
--
-- 'provisionedThroughputInMibps', 'createFileSystem_provisionedThroughputInMibps' - The throughput, measured in MiB\/s, that you want to provision for a
-- file system that you\'re creating. Valid values are 1-1024. Required if
-- @ThroughputMode@ is set to @provisioned@. The upper limit for throughput
-- is 1024 MiB\/s. You can get this limit increased by contacting AWS
-- Support. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase>
-- in the /Amazon EFS User Guide./
--
-- 'kmsKeyId', 'createFileSystem_kmsKeyId' - The ID of the AWS KMS CMK to be used to protect the encrypted file
-- system. This parameter is only required if you want to use a nondefault
-- CMK. If this parameter is not specified, the default CMK for Amazon EFS
-- is used. This ID can be in one of the following formats:
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
-- If @KmsKeyId@ is specified, the CreateFileSystemRequest$Encrypted
-- parameter must be set to true.
--
-- EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS
-- file systems.
--
-- 'tags', 'createFileSystem_tags' - A value that specifies to create one or more tags associated with the
-- file system. Each tag is a user-defined key-value pair. Name your file
-- system on creation by including a
-- @\"Key\":\"Name\",\"Value\":\"{value}\"@ key-value pair.
--
-- 'performanceMode', 'createFileSystem_performanceMode' - The performance mode of the file system. We recommend @generalPurpose@
-- performance mode for most file systems. File systems using the @maxIO@
-- performance mode can scale to higher levels of aggregate throughput and
-- operations per second with a tradeoff of slightly higher latencies for
-- most file operations. The performance mode can\'t be changed after the
-- file system has been created.
--
-- 'creationToken', 'createFileSystem_creationToken' - A string of up to 64 ASCII characters. Amazon EFS uses this to ensure
-- idempotent creation.
newCreateFileSystem ::
  -- | 'creationToken'
  Prelude.Text ->
  CreateFileSystem
newCreateFileSystem pCreationToken_ =
  CreateFileSystem'
    { throughputMode = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      provisionedThroughputInMibps = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      tags = Prelude.Nothing,
      performanceMode = Prelude.Nothing,
      creationToken = pCreationToken_
    }

-- | The throughput mode for the file system to be created. There are two
-- throughput modes to choose from for your file system: @bursting@ and
-- @provisioned@. If you set @ThroughputMode@ to @provisioned@, you must
-- also set a value for @ProvisionedThroughPutInMibps@. You can decrease
-- your file system\'s throughput in Provisioned Throughput mode or change
-- between the throughput modes as long as it’s been more than 24 hours
-- since the last decrease or throughput mode change. For more, see
-- <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode>
-- in the /Amazon EFS User Guide./
createFileSystem_throughputMode :: Lens.Lens' CreateFileSystem (Prelude.Maybe ThroughputMode)
createFileSystem_throughputMode = Lens.lens (\CreateFileSystem' {throughputMode} -> throughputMode) (\s@CreateFileSystem' {} a -> s {throughputMode = a} :: CreateFileSystem)

-- | A Boolean value that, if true, creates an encrypted file system. When
-- creating an encrypted file system, you have the option of specifying
-- CreateFileSystemRequest$KmsKeyId for an existing AWS Key Management
-- Service (AWS KMS) customer master key (CMK). If you don\'t specify a
-- CMK, then the default CMK for Amazon EFS, @\/aws\/elasticfilesystem@, is
-- used to protect the encrypted file system.
createFileSystem_encrypted :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Bool)
createFileSystem_encrypted = Lens.lens (\CreateFileSystem' {encrypted} -> encrypted) (\s@CreateFileSystem' {} a -> s {encrypted = a} :: CreateFileSystem)

-- | The throughput, measured in MiB\/s, that you want to provision for a
-- file system that you\'re creating. Valid values are 1-1024. Required if
-- @ThroughputMode@ is set to @provisioned@. The upper limit for throughput
-- is 1024 MiB\/s. You can get this limit increased by contacting AWS
-- Support. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase>
-- in the /Amazon EFS User Guide./
createFileSystem_provisionedThroughputInMibps :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Double)
createFileSystem_provisionedThroughputInMibps = Lens.lens (\CreateFileSystem' {provisionedThroughputInMibps} -> provisionedThroughputInMibps) (\s@CreateFileSystem' {} a -> s {provisionedThroughputInMibps = a} :: CreateFileSystem)

-- | The ID of the AWS KMS CMK to be used to protect the encrypted file
-- system. This parameter is only required if you want to use a nondefault
-- CMK. If this parameter is not specified, the default CMK for Amazon EFS
-- is used. This ID can be in one of the following formats:
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
-- If @KmsKeyId@ is specified, the CreateFileSystemRequest$Encrypted
-- parameter must be set to true.
--
-- EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS
-- file systems.
createFileSystem_kmsKeyId :: Lens.Lens' CreateFileSystem (Prelude.Maybe Prelude.Text)
createFileSystem_kmsKeyId = Lens.lens (\CreateFileSystem' {kmsKeyId} -> kmsKeyId) (\s@CreateFileSystem' {} a -> s {kmsKeyId = a} :: CreateFileSystem)

-- | A value that specifies to create one or more tags associated with the
-- file system. Each tag is a user-defined key-value pair. Name your file
-- system on creation by including a
-- @\"Key\":\"Name\",\"Value\":\"{value}\"@ key-value pair.
createFileSystem_tags :: Lens.Lens' CreateFileSystem (Prelude.Maybe [Tag])
createFileSystem_tags = Lens.lens (\CreateFileSystem' {tags} -> tags) (\s@CreateFileSystem' {} a -> s {tags = a} :: CreateFileSystem) Prelude.. Lens.mapping Lens._Coerce

-- | The performance mode of the file system. We recommend @generalPurpose@
-- performance mode for most file systems. File systems using the @maxIO@
-- performance mode can scale to higher levels of aggregate throughput and
-- operations per second with a tradeoff of slightly higher latencies for
-- most file operations. The performance mode can\'t be changed after the
-- file system has been created.
createFileSystem_performanceMode :: Lens.Lens' CreateFileSystem (Prelude.Maybe PerformanceMode)
createFileSystem_performanceMode = Lens.lens (\CreateFileSystem' {performanceMode} -> performanceMode) (\s@CreateFileSystem' {} a -> s {performanceMode = a} :: CreateFileSystem)

-- | A string of up to 64 ASCII characters. Amazon EFS uses this to ensure
-- idempotent creation.
createFileSystem_creationToken :: Lens.Lens' CreateFileSystem Prelude.Text
createFileSystem_creationToken = Lens.lens (\CreateFileSystem' {creationToken} -> creationToken) (\s@CreateFileSystem' {} a -> s {creationToken = a} :: CreateFileSystem)

instance Core.AWSRequest CreateFileSystem where
  type
    AWSResponse CreateFileSystem =
      FileSystemDescription
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateFileSystem

instance Prelude.NFData CreateFileSystem

instance Core.ToHeaders CreateFileSystem where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateFileSystem where
  toJSON CreateFileSystem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ThroughputMode" Core..=)
              Prelude.<$> throughputMode,
            ("Encrypted" Core..=) Prelude.<$> encrypted,
            ("ProvisionedThroughputInMibps" Core..=)
              Prelude.<$> provisionedThroughputInMibps,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("Tags" Core..=) Prelude.<$> tags,
            ("PerformanceMode" Core..=)
              Prelude.<$> performanceMode,
            Prelude.Just
              ("CreationToken" Core..= creationToken)
          ]
      )

instance Core.ToPath CreateFileSystem where
  toPath = Prelude.const "/2015-02-01/file-systems"

instance Core.ToQuery CreateFileSystem where
  toQuery = Prelude.const Prelude.mempty
