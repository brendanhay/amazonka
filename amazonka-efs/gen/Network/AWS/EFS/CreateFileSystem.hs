{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.CreateFileSystem
-- Copyright   : (c) 2013-2018 Brendan Hay
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
-- This operation also takes an optional @PerformanceMode@ parameter that you choose for your file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. The performance mode can't be changed after the file system has been created. For more information, see <http://docs.aws.amazon.com/efs/latest/ug/performance.html#performancemodes.html Amazon EFS: Performance Modes> .
--
-- After the file system is fully created, Amazon EFS sets its lifecycle state to @available@ , at which point you can create one or more mount targets for the file system in your VPC. For more information, see 'CreateMountTarget' . You mount your Amazon EFS file system on an EC2 instances in your VPC via the mount target. For more information, see <http://docs.aws.amazon.com/efs/latest/ug/how-it-works.html Amazon EFS: How it Works> .
--
-- This operation requires permissions for the @elasticfilesystem:CreateFileSystem@ action.
--
module Network.AWS.EFS.CreateFileSystem
    (
    -- * Creating a Request
      createFileSystem
    , CreateFileSystem
    -- * Request Lenses
    , cfsPerformanceMode
    , cfsEncrypted
    , cfsKMSKeyId
    , cfsCreationToken

    -- * Destructuring the Response
    , fileSystemDescription
    , FileSystemDescription
    -- * Response Lenses
    , fsdEncrypted
    , fsdKMSKeyId
    , fsdName
    , fsdOwnerId
    , fsdCreationToken
    , fsdFileSystemId
    , fsdCreationTime
    , fsdLifeCycleState
    , fsdNumberOfMountTargets
    , fsdSizeInBytes
    , fsdPerformanceMode
    ) where

import Network.AWS.EFS.Types
import Network.AWS.EFS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createFileSystem' smart constructor.
data CreateFileSystem = CreateFileSystem'
  { _cfsPerformanceMode :: !(Maybe PerformanceMode)
  , _cfsEncrypted       :: !(Maybe Bool)
  , _cfsKMSKeyId        :: !(Maybe Text)
  , _cfsCreationToken   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateFileSystem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfsPerformanceMode' - The @PerformanceMode@ of the file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. This can't be changed after the file system has been created.
--
-- * 'cfsEncrypted' - A boolean value that, if true, creates an encrypted file system. When creating an encrypted file system, you have the option of specifying a 'CreateFileSystemRequest$KmsKeyId' for an existing AWS Key Management Service (AWS KMS) customer master key (CMK). If you don't specify a CMK, then the default CMK for Amazon EFS, @/aws/elasticfilesystem@ , is used to protect the encrypted file system.
--
-- * 'cfsKMSKeyId' - The id of the AWS KMS CMK that will be used to protect the encrypted file system. This parameter is only required if you want to use a non-default CMK. If this parameter is not specified, the default CMK for Amazon EFS is used. This id can be in one of the following formats:     * Key ID - A unique identifier of the key. For example, @1234abcd-12ab-34cd-56ef-1234567890ab@ .     * ARN - An Amazon Resource Name for the key. For example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .     * Key alias - A previously created display name for a key. For example, @alias/projectKey1@ .     * Key alias ARN - An Amazon Resource Name for a key alias. For example, @arn:aws:kms:us-west-2:444455556666:alias/projectKey1@ . Note that if the KmsKeyId is specified, the 'CreateFileSystemRequest$Encrypted' parameter must be set to true.
--
-- * 'cfsCreationToken' - String of up to 64 ASCII characters. Amazon EFS uses this to ensure idempotent creation.
createFileSystem
    :: Text -- ^ 'cfsCreationToken'
    -> CreateFileSystem
createFileSystem pCreationToken_ =
  CreateFileSystem'
    { _cfsPerformanceMode = Nothing
    , _cfsEncrypted = Nothing
    , _cfsKMSKeyId = Nothing
    , _cfsCreationToken = pCreationToken_
    }


-- | The @PerformanceMode@ of the file system. We recommend @generalPurpose@ performance mode for most file systems. File systems using the @maxIO@ performance mode can scale to higher levels of aggregate throughput and operations per second with a tradeoff of slightly higher latencies for most file operations. This can't be changed after the file system has been created.
cfsPerformanceMode :: Lens' CreateFileSystem (Maybe PerformanceMode)
cfsPerformanceMode = lens _cfsPerformanceMode (\ s a -> s{_cfsPerformanceMode = a})

-- | A boolean value that, if true, creates an encrypted file system. When creating an encrypted file system, you have the option of specifying a 'CreateFileSystemRequest$KmsKeyId' for an existing AWS Key Management Service (AWS KMS) customer master key (CMK). If you don't specify a CMK, then the default CMK for Amazon EFS, @/aws/elasticfilesystem@ , is used to protect the encrypted file system.
cfsEncrypted :: Lens' CreateFileSystem (Maybe Bool)
cfsEncrypted = lens _cfsEncrypted (\ s a -> s{_cfsEncrypted = a})

-- | The id of the AWS KMS CMK that will be used to protect the encrypted file system. This parameter is only required if you want to use a non-default CMK. If this parameter is not specified, the default CMK for Amazon EFS is used. This id can be in one of the following formats:     * Key ID - A unique identifier of the key. For example, @1234abcd-12ab-34cd-56ef-1234567890ab@ .     * ARN - An Amazon Resource Name for the key. For example, @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@ .     * Key alias - A previously created display name for a key. For example, @alias/projectKey1@ .     * Key alias ARN - An Amazon Resource Name for a key alias. For example, @arn:aws:kms:us-west-2:444455556666:alias/projectKey1@ . Note that if the KmsKeyId is specified, the 'CreateFileSystemRequest$Encrypted' parameter must be set to true.
cfsKMSKeyId :: Lens' CreateFileSystem (Maybe Text)
cfsKMSKeyId = lens _cfsKMSKeyId (\ s a -> s{_cfsKMSKeyId = a})

-- | String of up to 64 ASCII characters. Amazon EFS uses this to ensure idempotent creation.
cfsCreationToken :: Lens' CreateFileSystem Text
cfsCreationToken = lens _cfsCreationToken (\ s a -> s{_cfsCreationToken = a})

instance AWSRequest CreateFileSystem where
        type Rs CreateFileSystem = FileSystemDescription
        request = postJSON efs
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateFileSystem where

instance NFData CreateFileSystem where

instance ToHeaders CreateFileSystem where
        toHeaders = const mempty

instance ToJSON CreateFileSystem where
        toJSON CreateFileSystem'{..}
          = object
              (catMaybes
                 [("PerformanceMode" .=) <$> _cfsPerformanceMode,
                  ("Encrypted" .=) <$> _cfsEncrypted,
                  ("KmsKeyId" .=) <$> _cfsKMSKeyId,
                  Just ("CreationToken" .= _cfsCreationToken)])

instance ToPath CreateFileSystem where
        toPath = const "/2015-02-01/file-systems"

instance ToQuery CreateFileSystem where
        toQuery = const mempty
