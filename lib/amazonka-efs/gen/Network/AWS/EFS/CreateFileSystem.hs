{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateFileSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty file system. The operation requires a creation token in the request that Amazon EFS uses to ensure idempotent creation (calling the operation with same creation token has no effect). If a file system does not currently exist that is owned by the caller's AWS account with the specified creation token, this operation does the following:
--
--
--     * Creates a new, empty file system. The file system will have an Amazon EFS assigned ID, and an initial lifecycle state @creating@ .
--
--
--     * Returns with the description of the created file system.
--
--
-- Otherwise, this operation returns a @FileSystemAlreadyExists@ error with the ID of the existing file system.
-- The idempotent operation allows you to retry a @CreateFileSystem@ call without risk of creating an extra file system. This can happen when an initial call fails in a way that leaves it uncertain whether or not a file system was actually created. An example might be that a transport level timeout occurred or your connection was reset. As long as you use the same creation token, if the initial call had succeeded in creating a file system, the client can learn of its existence from the @FileSystemAlreadyExists@ error.
-- This operation also takes an optional @PerformanceMode@ parameter that you choose for your file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html Amazon EFS: Performance Modes> .
-- After the file system is fully created, Amazon EFS sets its lifecycle state to @available@ , at which point you can create one or more mount targets for the file system in your VPC. For more information, see 'CreateMountTarget' . You mount your Amazon EFS file system on an EC2 instances in your VPC by using the mount target. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works> .
-- This operation requires permissions for the @elasticfilesystem:CreateFileSystem@ action.
module Network.AWS.EFS.CreateFileSystem
  ( -- * Creating a request
    CreateFileSystem (..),
    mkCreateFileSystem,

    -- ** Request lenses
    cfsProvisionedThroughputInMibps,
    cfsPerformanceMode,
    cfsEncrypted,
    cfsThroughputMode,
    cfsKMSKeyId,
    cfsTags,
    cfsCreationToken,

    -- * Destructuring the response
    FileSystemDescription (..),
    mkFileSystemDescription,

    -- ** Response lenses
    fsdProvisionedThroughputInMibps,
    fsdFileSystemARN,
    fsdEncrypted,
    fsdThroughputMode,
    fsdKMSKeyId,
    fsdName,
    fsdOwnerId,
    fsdCreationToken,
    fsdFileSystemId,
    fsdCreationTime,
    fsdLifeCycleState,
    fsdNumberOfMountTargets,
    fsdSizeInBytes,
    fsdPerformanceMode,
    fsdTags,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateFileSystem' smart constructor.
data CreateFileSystem = CreateFileSystem'
  { provisionedThroughputInMibps ::
      Lude.Maybe Lude.Double,
    performanceMode :: Lude.Maybe PerformanceMode,
    encrypted :: Lude.Maybe Lude.Bool,
    throughputMode :: Lude.Maybe ThroughputMode,
    kmsKeyId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    creationToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateFileSystem' with the minimum fields required to make a request.
--
-- * 'creationToken' - A string of up to 64 ASCII characters. Amazon EFS uses this to ensure idempotent creation.
-- * 'encrypted' - A Boolean value that, if true, creates an encrypted file system. When creating an encrypted file system, you have the option of specifying 'CreateFileSystemRequest$KmsKeyId' for an existing AWS Key Management Service (AWS KMS) customer master key (CMK). If you don't specify a CMK, then the default CMK for Amazon EFS, @/aws/elasticfilesystem@ , is used to protect the encrypted file system.
-- * 'kmsKeyId' - The ID of the AWS KMS CMK to be used to protect the encrypted file system. This parameter is only required if you want to use a nondefault CMK. If this parameter is not specified, the default CMK for Amazon EFS is used. This ID can be in one of the following formats:
--
--
--     * Key ID - A unique identifier of the key, for example @1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
--
--     * ARN - An Amazon Resource Name (ARN) for the key, for example @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
--
--     * Key alias - A previously created display name for a key, for example @alias/projectKey1@ .
--
--
--     * Key alias ARN - An ARN for a key alias, for example @arn:aws:kms:us-west-2:444455556666:alias/projectKey1@ .
--
--
-- If @KmsKeyId@ is specified, the 'CreateFileSystemRequest$Encrypted' parameter must be set to true.
-- /Important:/ EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS file systems.
-- * 'performanceMode' - The performance mode of the file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created.
-- * 'provisionedThroughputInMibps' - The throughput, measured in MiB/s, that you want to provision for a file system that you're creating. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The upper limit for throughput is 1024 MiB/s. You can get this limit increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
-- * 'tags' - A value that specifies to create one or more tags associated with the file system. Each tag is a user-defined key-value pair. Name your file system on creation by including a @"Key":"Name","Value":"{value}"@ key-value pair.
-- * 'throughputMode' - The throughput mode for the file system to be created. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change. For more, see <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode> in the /Amazon EFS User Guide./
mkCreateFileSystem ::
  -- | 'creationToken'
  Lude.Text ->
  CreateFileSystem
mkCreateFileSystem pCreationToken_ =
  CreateFileSystem'
    { provisionedThroughputInMibps = Lude.Nothing,
      performanceMode = Lude.Nothing,
      encrypted = Lude.Nothing,
      throughputMode = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      tags = Lude.Nothing,
      creationToken = pCreationToken_
    }

-- | The throughput, measured in MiB/s, that you want to provision for a file system that you're creating. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The upper limit for throughput is 1024 MiB/s. You can get this limit increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
--
-- /Note:/ Consider using 'provisionedThroughputInMibps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsProvisionedThroughputInMibps :: Lens.Lens' CreateFileSystem (Lude.Maybe Lude.Double)
cfsProvisionedThroughputInMibps = Lens.lens (provisionedThroughputInMibps :: CreateFileSystem -> Lude.Maybe Lude.Double) (\s a -> s {provisionedThroughputInMibps = a} :: CreateFileSystem)
{-# DEPRECATED cfsProvisionedThroughputInMibps "Use generic-lens or generic-optics with 'provisionedThroughputInMibps' instead." #-}

-- | The performance mode of the file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created.
--
-- /Note:/ Consider using 'performanceMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsPerformanceMode :: Lens.Lens' CreateFileSystem (Lude.Maybe PerformanceMode)
cfsPerformanceMode = Lens.lens (performanceMode :: CreateFileSystem -> Lude.Maybe PerformanceMode) (\s a -> s {performanceMode = a} :: CreateFileSystem)
{-# DEPRECATED cfsPerformanceMode "Use generic-lens or generic-optics with 'performanceMode' instead." #-}

-- | A Boolean value that, if true, creates an encrypted file system. When creating an encrypted file system, you have the option of specifying 'CreateFileSystemRequest$KmsKeyId' for an existing AWS Key Management Service (AWS KMS) customer master key (CMK). If you don't specify a CMK, then the default CMK for Amazon EFS, @/aws/elasticfilesystem@ , is used to protect the encrypted file system.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsEncrypted :: Lens.Lens' CreateFileSystem (Lude.Maybe Lude.Bool)
cfsEncrypted = Lens.lens (encrypted :: CreateFileSystem -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CreateFileSystem)
{-# DEPRECATED cfsEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The throughput mode for the file system to be created. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change. For more, see <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode> in the /Amazon EFS User Guide./
--
-- /Note:/ Consider using 'throughputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsThroughputMode :: Lens.Lens' CreateFileSystem (Lude.Maybe ThroughputMode)
cfsThroughputMode = Lens.lens (throughputMode :: CreateFileSystem -> Lude.Maybe ThroughputMode) (\s a -> s {throughputMode = a} :: CreateFileSystem)
{-# DEPRECATED cfsThroughputMode "Use generic-lens or generic-optics with 'throughputMode' instead." #-}

-- | The ID of the AWS KMS CMK to be used to protect the encrypted file system. This parameter is only required if you want to use a nondefault CMK. If this parameter is not specified, the default CMK for Amazon EFS is used. This ID can be in one of the following formats:
--
--
--     * Key ID - A unique identifier of the key, for example @1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
--
--     * ARN - An Amazon Resource Name (ARN) for the key, for example @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .
--
--
--     * Key alias - A previously created display name for a key, for example @alias/projectKey1@ .
--
--
--     * Key alias ARN - An ARN for a key alias, for example @arn:aws:kms:us-west-2:444455556666:alias/projectKey1@ .
--
--
-- If @KmsKeyId@ is specified, the 'CreateFileSystemRequest$Encrypted' parameter must be set to true.
-- /Important:/ EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS file systems.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsKMSKeyId :: Lens.Lens' CreateFileSystem (Lude.Maybe Lude.Text)
cfsKMSKeyId = Lens.lens (kmsKeyId :: CreateFileSystem -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateFileSystem)
{-# DEPRECATED cfsKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A value that specifies to create one or more tags associated with the file system. Each tag is a user-defined key-value pair. Name your file system on creation by including a @"Key":"Name","Value":"{value}"@ key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsTags :: Lens.Lens' CreateFileSystem (Lude.Maybe [Tag])
cfsTags = Lens.lens (tags :: CreateFileSystem -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateFileSystem)
{-# DEPRECATED cfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | A string of up to 64 ASCII characters. Amazon EFS uses this to ensure idempotent creation.
--
-- /Note:/ Consider using 'creationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsCreationToken :: Lens.Lens' CreateFileSystem Lude.Text
cfsCreationToken = Lens.lens (creationToken :: CreateFileSystem -> Lude.Text) (\s a -> s {creationToken = a} :: CreateFileSystem)
{-# DEPRECATED cfsCreationToken "Use generic-lens or generic-optics with 'creationToken' instead." #-}

instance Lude.AWSRequest CreateFileSystem where
  type Rs CreateFileSystem = FileSystemDescription
  request = Req.postJSON efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateFileSystem where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateFileSystem where
  toJSON CreateFileSystem' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ProvisionedThroughputInMibps" Lude..=)
              Lude.<$> provisionedThroughputInMibps,
            ("PerformanceMode" Lude..=) Lude.<$> performanceMode,
            ("Encrypted" Lude..=) Lude.<$> encrypted,
            ("ThroughputMode" Lude..=) Lude.<$> throughputMode,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("CreationToken" Lude..= creationToken)
          ]
      )

instance Lude.ToPath CreateFileSystem where
  toPath = Lude.const "/2015-02-01/file-systems"

instance Lude.ToQuery CreateFileSystem where
  toQuery = Lude.const Lude.mempty
