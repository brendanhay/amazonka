{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
--     * Returns with the description of the created file system.
--
--
--
-- Otherwise, this operation returns a @FileSystemAlreadyExists@ error with the ID of the existing file system.
--
-- The idempotent operation allows you to retry a @CreateFileSystem@ call without risk of creating an extra file system. This can happen when an initial call fails in a way that leaves it uncertain whether or not a file system was actually created. An example might be that a transport level timeout occurred or your connection was reset. As long as you use the same creation token, if the initial call had succeeded in creating a file system, the client can learn of its existence from the @FileSystemAlreadyExists@ error.
--
-- This operation also takes an optional @PerformanceMode@ parameter that you choose for your file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html Amazon EFS: Performance Modes> .
--
-- After the file system is fully created, Amazon EFS sets its lifecycle state to @available@ , at which point you can create one or more mount targets for the file system in your VPC. For more information, see 'CreateMountTarget' . You mount your Amazon EFS file system on an EC2 instances in your VPC by using the mount target. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works> .
--
-- This operation requires permissions for the @elasticfilesystem:CreateFileSystem@ action.
module Network.AWS.EFS.CreateFileSystem
  ( -- * Creating a Request
    createFileSystem,
    CreateFileSystem,

    -- * Request Lenses
    cfsProvisionedThroughputInMibps,
    cfsPerformanceMode,
    cfsEncrypted,
    cfsThroughputMode,
    cfsKMSKeyId,
    cfsTags,
    cfsCreationToken,

    -- * Destructuring the Response
    fileSystemDescription,
    FileSystemDescription,

    -- * Response Lenses
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFileSystem' smart constructor.
data CreateFileSystem = CreateFileSystem'
  { _cfsProvisionedThroughputInMibps ::
      !(Maybe Double),
    _cfsPerformanceMode :: !(Maybe PerformanceMode),
    _cfsEncrypted :: !(Maybe Bool),
    _cfsThroughputMode :: !(Maybe ThroughputMode),
    _cfsKMSKeyId :: !(Maybe Text),
    _cfsTags :: !(Maybe [Tag]),
    _cfsCreationToken :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateFileSystem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsProvisionedThroughputInMibps' - The throughput, measured in MiB/s, that you want to provision for a file system that you're creating. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The upper limit for throughput is 1024 MiB/s. You can get this limit increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
--
-- * 'cfsPerformanceMode' - The performance mode of the file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created.
--
-- * 'cfsEncrypted' - A Boolean value that, if true, creates an encrypted file system. When creating an encrypted file system, you have the option of specifying 'CreateFileSystemRequest$KmsKeyId' for an existing AWS Key Management Service (AWS KMS) customer master key (CMK). If you don't specify a CMK, then the default CMK for Amazon EFS, @/aws/elasticfilesystem@ , is used to protect the encrypted file system.
--
-- * 'cfsThroughputMode' - The throughput mode for the file system to be created. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change. For more, see <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode> in the /Amazon EFS User Guide./
--
-- * 'cfsKMSKeyId' - The ID of the AWS KMS CMK to be used to protect the encrypted file system. This parameter is only required if you want to use a nondefault CMK. If this parameter is not specified, the default CMK for Amazon EFS is used. This ID can be in one of the following formats:     * Key ID - A unique identifier of the key, for example @1234abcd-12ab-34cd-56ef-1234567890ab@ .     * ARN - An Amazon Resource Name (ARN) for the key, for example @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .     * Key alias - A previously created display name for a key, for example @alias/projectKey1@ .     * Key alias ARN - An ARN for a key alias, for example @arn:aws:kms:us-west-2:444455556666:alias/projectKey1@ . If @KmsKeyId@ is specified, the 'CreateFileSystemRequest$Encrypted' parameter must be set to true. /Important:/ EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS file systems.
--
-- * 'cfsTags' - A value that specifies to create one or more tags associated with the file system. Each tag is a user-defined key-value pair. Name your file system on creation by including a @"Key":"Name","Value":"{value}"@ key-value pair.
--
-- * 'cfsCreationToken' - A string of up to 64 ASCII characters. Amazon EFS uses this to ensure idempotent creation.
createFileSystem ::
  -- | 'cfsCreationToken'
  Text ->
  CreateFileSystem
createFileSystem pCreationToken_ =
  CreateFileSystem'
    { _cfsProvisionedThroughputInMibps = Nothing,
      _cfsPerformanceMode = Nothing,
      _cfsEncrypted = Nothing,
      _cfsThroughputMode = Nothing,
      _cfsKMSKeyId = Nothing,
      _cfsTags = Nothing,
      _cfsCreationToken = pCreationToken_
    }

-- | The throughput, measured in MiB/s, that you want to provision for a file system that you're creating. Valid values are 1-1024. Required if @ThroughputMode@ is set to @provisioned@ . The upper limit for throughput is 1024 MiB/s. You can get this limit increased by contacting AWS Support. For more information, see <https://docs.aws.amazon.com/efs/latest/ug/limits.html#soft-limits Amazon EFS Limits That You Can Increase> in the /Amazon EFS User Guide./
cfsProvisionedThroughputInMibps :: Lens' CreateFileSystem (Maybe Double)
cfsProvisionedThroughputInMibps = lens _cfsProvisionedThroughputInMibps (\s a -> s {_cfsProvisionedThroughputInMibps = a})

-- | The performance mode of the file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created.
cfsPerformanceMode :: Lens' CreateFileSystem (Maybe PerformanceMode)
cfsPerformanceMode = lens _cfsPerformanceMode (\s a -> s {_cfsPerformanceMode = a})

-- | A Boolean value that, if true, creates an encrypted file system. When creating an encrypted file system, you have the option of specifying 'CreateFileSystemRequest$KmsKeyId' for an existing AWS Key Management Service (AWS KMS) customer master key (CMK). If you don't specify a CMK, then the default CMK for Amazon EFS, @/aws/elasticfilesystem@ , is used to protect the encrypted file system.
cfsEncrypted :: Lens' CreateFileSystem (Maybe Bool)
cfsEncrypted = lens _cfsEncrypted (\s a -> s {_cfsEncrypted = a})

-- | The throughput mode for the file system to be created. There are two throughput modes to choose from for your file system: @bursting@ and @provisioned@ . If you set @ThroughputMode@ to @provisioned@ , you must also set a value for @ProvisionedThroughPutInMibps@ . You can decrease your file system's throughput in Provisioned Throughput mode or change between the throughput modes as long as it’s been more than 24 hours since the last decrease or throughput mode change. For more, see <https://docs.aws.amazon.com/efs/latest/ug/performance.html#provisioned-throughput Specifying Throughput with Provisioned Mode> in the /Amazon EFS User Guide./
cfsThroughputMode :: Lens' CreateFileSystem (Maybe ThroughputMode)
cfsThroughputMode = lens _cfsThroughputMode (\s a -> s {_cfsThroughputMode = a})

-- | The ID of the AWS KMS CMK to be used to protect the encrypted file system. This parameter is only required if you want to use a nondefault CMK. If this parameter is not specified, the default CMK for Amazon EFS is used. This ID can be in one of the following formats:     * Key ID - A unique identifier of the key, for example @1234abcd-12ab-34cd-56ef-1234567890ab@ .     * ARN - An Amazon Resource Name (ARN) for the key, for example @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .     * Key alias - A previously created display name for a key, for example @alias/projectKey1@ .     * Key alias ARN - An ARN for a key alias, for example @arn:aws:kms:us-west-2:444455556666:alias/projectKey1@ . If @KmsKeyId@ is specified, the 'CreateFileSystemRequest$Encrypted' parameter must be set to true. /Important:/ EFS accepts only symmetric CMKs. You cannot use asymmetric CMKs with EFS file systems.
cfsKMSKeyId :: Lens' CreateFileSystem (Maybe Text)
cfsKMSKeyId = lens _cfsKMSKeyId (\s a -> s {_cfsKMSKeyId = a})

-- | A value that specifies to create one or more tags associated with the file system. Each tag is a user-defined key-value pair. Name your file system on creation by including a @"Key":"Name","Value":"{value}"@ key-value pair.
cfsTags :: Lens' CreateFileSystem [Tag]
cfsTags = lens _cfsTags (\s a -> s {_cfsTags = a}) . _Default . _Coerce

-- | A string of up to 64 ASCII characters. Amazon EFS uses this to ensure idempotent creation.
cfsCreationToken :: Lens' CreateFileSystem Text
cfsCreationToken = lens _cfsCreationToken (\s a -> s {_cfsCreationToken = a})

instance AWSRequest CreateFileSystem where
  type Rs CreateFileSystem = FileSystemDescription
  request = postJSON efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable CreateFileSystem

instance NFData CreateFileSystem

instance ToHeaders CreateFileSystem where
  toHeaders = const mempty

instance ToJSON CreateFileSystem where
  toJSON CreateFileSystem' {..} =
    object
      ( catMaybes
          [ ("ProvisionedThroughputInMibps" .=)
              <$> _cfsProvisionedThroughputInMibps,
            ("PerformanceMode" .=) <$> _cfsPerformanceMode,
            ("Encrypted" .=) <$> _cfsEncrypted,
            ("ThroughputMode" .=) <$> _cfsThroughputMode,
            ("KmsKeyId" .=) <$> _cfsKMSKeyId,
            ("Tags" .=) <$> _cfsTags,
            Just ("CreationToken" .= _cfsCreationToken)
          ]
      )

instance ToPath CreateFileSystem where
  toPath = const "/2015-02-01/file-systems"

instance ToQuery CreateFileSystem where
  toQuery = const mempty
