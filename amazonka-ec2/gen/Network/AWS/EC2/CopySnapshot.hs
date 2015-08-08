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
-- Stability   : auto-generated
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
-- snapshot copies use the default AWS Key Management Service (AWS KMS)
-- customer master key (CMK); however, you can specify a non-default CMK
-- with the @KmsKeyId@ parameter.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copying an Amazon EBS Snapshot>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopySnapshot.html AWS API Reference> for CopySnapshot.
module Network.AWS.EC2.CopySnapshot
    (
    -- * Creating a Request
      CopySnapshot
    , copySnapshot
    -- * Request Lenses
    , csEncrypted
    , csPresignedURL
    , csDestinationRegion
    , csKMSKeyId
    , csDryRun
    , csDescription
    , csSourceRegion
    , csSourceSnapshotId

    -- * Destructuring the Response
    , CopySnapshotResponse
    , copySnapshotResponse
    -- * Response Lenses
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
-- * 'csEncrypted'
--
-- * 'csPresignedURL'
--
-- * 'csDestinationRegion'
--
-- * 'csKMSKeyId'
--
-- * 'csDryRun'
--
-- * 'csDescription'
--
-- * 'csSourceRegion'
--
-- * 'csSourceSnapshotId'
data CopySnapshot = CopySnapshot'
    { _csEncrypted         :: !(Maybe Bool)
    , _csPresignedURL      :: !(Maybe Text)
    , _csDestinationRegion :: !(Maybe Text)
    , _csKMSKeyId          :: !(Maybe Text)
    , _csDryRun            :: !(Maybe Bool)
    , _csDescription       :: !(Maybe Text)
    , _csSourceRegion      :: !Text
    , _csSourceSnapshotId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopySnapshot' smart constructor.
copySnapshot :: Text -> Text -> CopySnapshot
copySnapshot pSourceRegion_ pSourceSnapshotId_ =
    CopySnapshot'
    { _csEncrypted = Nothing
    , _csPresignedURL = Nothing
    , _csDestinationRegion = Nothing
    , _csKMSKeyId = Nothing
    , _csDryRun = Nothing
    , _csDescription = Nothing
    , _csSourceRegion = pSourceRegion_
    , _csSourceSnapshotId = pSourceSnapshotId_
    }

-- | Specifies whether the destination snapshot should be encrypted. There is
-- no way to create an unencrypted snapshot copy from an encrypted
-- snapshot; however, you can encrypt a copy of an unencrypted snapshot
-- with this flag. The default CMK for EBS is used unless a non-default AWS
-- Key Management Service (AWS KMS) CMK is specified with @KmsKeyId@. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
csEncrypted :: Lens' CopySnapshot (Maybe Bool)
csEncrypted = lens _csEncrypted (\ s a -> s{_csEncrypted = a});

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
csPresignedURL :: Lens' CopySnapshot (Maybe Text)
csPresignedURL = lens _csPresignedURL (\ s a -> s{_csPresignedURL = a});

-- | The destination region to use in the @PresignedUrl@ parameter of a
-- snapshot copy operation. This parameter is only valid for specifying the
-- destination region in a @PresignedUrl@ parameter, where it is required.
--
-- @CopySnapshot@ sends the snapshot copy to the regional endpoint that you
-- send the HTTP request to, such as @ec2.us-east-1.amazonaws.com@ (in the
-- AWS CLI, this is specified with the @--region@ parameter or the default
-- region in your AWS configuration file).
csDestinationRegion :: Lens' CopySnapshot (Maybe Text)
csDestinationRegion = lens _csDestinationRegion (\ s a -> s{_csDestinationRegion = a});

-- | The full ARN of the AWS Key Management Service (AWS KMS) CMK to use when
-- creating the snapshot copy. This parameter is only required if you want
-- to use a non-default CMK; if this parameter is not specified, the
-- default CMK for EBS is used. The ARN contains the @arn:aws:kms@
-- namespace, followed by the region of the CMK, the AWS account ID of the
-- CMK owner, the @key@ namespace, and then the CMK ID. For example,
-- arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
-- The specified CMK must exist in the region that the snapshot is being
-- copied to. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also
-- be set.
csKMSKeyId :: Lens' CopySnapshot (Maybe Text)
csKMSKeyId = lens _csKMSKeyId (\ s a -> s{_csKMSKeyId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
csDryRun :: Lens' CopySnapshot (Maybe Bool)
csDryRun = lens _csDryRun (\ s a -> s{_csDryRun = a});

-- | A description for the EBS snapshot.
csDescription :: Lens' CopySnapshot (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a});

-- | The ID of the region that contains the snapshot to be copied.
csSourceRegion :: Lens' CopySnapshot Text
csSourceRegion = lens _csSourceRegion (\ s a -> s{_csSourceRegion = a});

-- | The ID of the EBS snapshot to copy.
csSourceSnapshotId :: Lens' CopySnapshot Text
csSourceSnapshotId = lens _csSourceSnapshotId (\ s a -> s{_csSourceSnapshotId = a});

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
               "Encrypted" =: _csEncrypted,
               "PresignedUrl" =: _csPresignedURL,
               "DestinationRegion" =: _csDestinationRegion,
               "KmsKeyId" =: _csKMSKeyId, "DryRun" =: _csDryRun,
               "Description" =: _csDescription,
               "SourceRegion" =: _csSourceRegion,
               "SourceSnapshotId" =: _csSourceSnapshotId]

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

-- | Undocumented member.
csrsStatus :: Lens' CopySnapshotResponse Int
csrsStatus = lens _csrsStatus (\ s a -> s{_csrsStatus = a});
