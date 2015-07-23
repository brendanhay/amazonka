{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CopySnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copies a point-in-time snapshot of an EBS volume and stores it in Amazon
-- S3. You can copy the snapshot within the same region or from one region
-- to another. You can use the snapshot to create EBS volumes or Amazon
-- Machine Images (AMIs). The snapshot is copied to the regional endpoint
-- that you send the HTTP request to.
--
-- Copies of encrypted EBS snapshots remain encrypted. Copies of
-- unencrypted snapshots remain unencrypted, unless the @Encrypted@ flag is
-- specified during the snapshot copy operation. By default, encrypted
-- snapshot copies use the default AWS Key Management Service (KMS)
-- Customer Master Key (CMK); however, you can specify a non-default CMK
-- with the @KmsKeyId@ parameter.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copying an Amazon EBS Snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopySnapshot.html>
module Network.AWS.EC2.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , copySnapshot
    -- ** Request lenses
    , csrqEncrypted
    , csrqPresignedURL
    , csrqDestinationRegion
    , csrqKMSKeyId
    , csrqDryRun
    , csrqDescription
    , csrqSourceRegion
    , csrqSourceSnapshotId

    -- * Response
    , CopySnapshotResponse
    -- ** Response constructor
    , copySnapshotResponse
    -- ** Response lenses
    , csrsSnapshotId
    , csrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'copySnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrqEncrypted'
--
-- * 'csrqPresignedURL'
--
-- * 'csrqDestinationRegion'
--
-- * 'csrqKMSKeyId'
--
-- * 'csrqDryRun'
--
-- * 'csrqDescription'
--
-- * 'csrqSourceRegion'
--
-- * 'csrqSourceSnapshotId'
data CopySnapshot = CopySnapshot'
    { _csrqEncrypted         :: !(Maybe Bool)
    , _csrqPresignedURL      :: !(Maybe Text)
    , _csrqDestinationRegion :: !(Maybe Text)
    , _csrqKMSKeyId          :: !(Maybe Text)
    , _csrqDryRun            :: !(Maybe Bool)
    , _csrqDescription       :: !(Maybe Text)
    , _csrqSourceRegion      :: !Text
    , _csrqSourceSnapshotId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopySnapshot' smart constructor.
copySnapshot :: Text -> Text -> CopySnapshot
copySnapshot pSourceRegion_ pSourceSnapshotId_ =
    CopySnapshot'
    { _csrqEncrypted = Nothing
    , _csrqPresignedURL = Nothing
    , _csrqDestinationRegion = Nothing
    , _csrqKMSKeyId = Nothing
    , _csrqDryRun = Nothing
    , _csrqDescription = Nothing
    , _csrqSourceRegion = pSourceRegion_
    , _csrqSourceSnapshotId = pSourceSnapshotId_
    }

-- | Specifies whether the destination snapshot should be encrypted. There is
-- no way to create an unencrypted snapshot copy from an encrypted
-- snapshot; however, you can encrypt a copy of an unencrypted snapshot
-- with this flag. The default CMK for EBS is used unless a non-default AWS
-- Key Management Service (KMS) CMK is specified with @KmsKeyId@. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
csrqEncrypted :: Lens' CopySnapshot (Maybe Bool)
csrqEncrypted = lens _csrqEncrypted (\ s a -> s{_csrqEncrypted = a});

-- | The pre-signed URL that facilitates copying an encrypted snapshot. This
-- parameter is only required when copying an encrypted snapshot with the
-- Amazon EC2 Query API; it is available as an optional parameter in all
-- other cases. The @PresignedUrl@ should use the snapshot source endpoint,
-- the @CopySnapshot@ action, and include the @SourceRegion@,
-- @SourceSnapshotId@, and @DestinationRegion@ parameters. The
-- @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS
-- snapshots are stored in Amazon S3, the signing algorithm for this
-- parameter uses the same logic that is described in
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)>
-- in the /Amazon Simple Storage Service API Reference/. An invalid or
-- improperly signed @PresignedUrl@ will cause the copy operation to fail
-- asynchronously, and the snapshot will move to an @error@ state.
csrqPresignedURL :: Lens' CopySnapshot (Maybe Text)
csrqPresignedURL = lens _csrqPresignedURL (\ s a -> s{_csrqPresignedURL = a});

-- | The destination region to use in the @PresignedUrl@ parameter of a
-- snapshot copy operation. This parameter is only valid for specifying the
-- destination region in a @PresignedUrl@ parameter, where it is required.
--
-- @CopySnapshot@ sends the snapshot copy to the regional endpoint that you
-- send the HTTP request to, such as @ec2.us-east-1.amazonaws.com@ (in the
-- AWS CLI, this is specified with the @--region@ parameter or the default
-- region in your AWS configuration file).
csrqDestinationRegion :: Lens' CopySnapshot (Maybe Text)
csrqDestinationRegion = lens _csrqDestinationRegion (\ s a -> s{_csrqDestinationRegion = a});

-- | The full ARN of the AWS Key Management Service (KMS) CMK to use when
-- creating the snapshot copy. This parameter is only required if you want
-- to use a non-default CMK; if this parameter is not specified, the
-- default CMK for EBS is used. The ARN contains the @arn:aws:kms@
-- namespace, followed by the region of the CMK, the AWS account ID of the
-- CMK owner, the @key@ namespace, and then the CMK ID. For example,
-- arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
-- The specified CMK must exist in the region that the snapshot is being
-- copied to. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also
-- be set.
csrqKMSKeyId :: Lens' CopySnapshot (Maybe Text)
csrqKMSKeyId = lens _csrqKMSKeyId (\ s a -> s{_csrqKMSKeyId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csrqDryRun :: Lens' CopySnapshot (Maybe Bool)
csrqDryRun = lens _csrqDryRun (\ s a -> s{_csrqDryRun = a});

-- | A description for the EBS snapshot.
csrqDescription :: Lens' CopySnapshot (Maybe Text)
csrqDescription = lens _csrqDescription (\ s a -> s{_csrqDescription = a});

-- | The ID of the region that contains the snapshot to be copied.
csrqSourceRegion :: Lens' CopySnapshot Text
csrqSourceRegion = lens _csrqSourceRegion (\ s a -> s{_csrqSourceRegion = a});

-- | The ID of the EBS snapshot to copy.
csrqSourceSnapshotId :: Lens' CopySnapshot Text
csrqSourceSnapshotId = lens _csrqSourceSnapshotId (\ s a -> s{_csrqSourceSnapshotId = a});

instance AWSRequest CopySnapshot where
        type Sv CopySnapshot = EC2
        type Rs CopySnapshot = CopySnapshotResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CopySnapshotResponse' <$>
                   (x .@? "snapshotId") <*> (pure (fromEnum s)))

instance ToHeaders CopySnapshot where
        toHeaders = const mempty

instance ToPath CopySnapshot where
        toPath = const "/"

instance ToQuery CopySnapshot where
        toQuery CopySnapshot'{..}
          = mconcat
              ["Action" =: ("CopySnapshot" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Encrypted" =: _csrqEncrypted,
               "PresignedUrl" =: _csrqPresignedURL,
               "DestinationRegion" =: _csrqDestinationRegion,
               "KmsKeyId" =: _csrqKMSKeyId, "DryRun" =: _csrqDryRun,
               "Description" =: _csrqDescription,
               "SourceRegion" =: _csrqSourceRegion,
               "SourceSnapshotId" =: _csrqSourceSnapshotId]

-- | /See:/ 'copySnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrsSnapshotId'
--
-- * 'csrsStatus'
data CopySnapshotResponse = CopySnapshotResponse'
    { _csrsSnapshotId :: !(Maybe Text)
    , _csrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopySnapshotResponse' smart constructor.
copySnapshotResponse :: Int -> CopySnapshotResponse
copySnapshotResponse pStatus_ =
    CopySnapshotResponse'
    { _csrsSnapshotId = Nothing
    , _csrsStatus = pStatus_
    }

-- | The ID of the new snapshot.
csrsSnapshotId :: Lens' CopySnapshotResponse (Maybe Text)
csrsSnapshotId = lens _csrsSnapshotId (\ s a -> s{_csrsSnapshotId = a});

-- | FIXME: Undocumented member.
csrsStatus :: Lens' CopySnapshotResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
