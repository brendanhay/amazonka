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
-- Module      : Network.AWS.EC2.CopySnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a point-in-time snapshot of an EBS volume and stores it in Amazon S3. You can copy the snapshot within the same region or from one region to another. You can use the snapshot to create EBS volumes or Amazon Machine Images (AMIs). The snapshot is copied to the regional endpoint that you send the HTTP request to.
--
--
-- Copies of encrypted EBS snapshots remain encrypted. Copies of unencrypted snapshots remain unencrypted, unless the @Encrypted@ flag is specified during the snapshot copy operation. By default, encrypted snapshot copies use the default AWS Key Management Service (AWS KMS) customer master key (CMK); however, you can specify a non-default CMK with the @KmsKeyId@ parameter.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copying an Amazon EBS Snapshot> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CopySnapshot
    (
    -- * Creating a Request
      copySnapshot
    , CopySnapshot
    -- * Request Lenses
    , csPresignedURL
    , csEncrypted
    , csDestinationRegion
    , csKMSKeyId
    , csDescription
    , csDryRun
    , csSourceRegion
    , csSourceSnapshotId

    -- * Destructuring the Response
    , copySnapshotResponse
    , CopySnapshotResponse
    -- * Response Lenses
    , csrsSnapshotId
    , csrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CopySnapshot.
--
--
--
-- /See:/ 'copySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { _csPresignedURL      :: !(Maybe Text)
  , _csEncrypted         :: !(Maybe Bool)
  , _csDestinationRegion :: !(Maybe Text)
  , _csKMSKeyId          :: !(Maybe Text)
  , _csDescription       :: !(Maybe Text)
  , _csDryRun            :: !(Maybe Bool)
  , _csSourceRegion      :: !Text
  , _csSourceSnapshotId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csPresignedURL' - When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> . The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
--
-- * 'csEncrypted' - Specifies whether the destination snapshot should be encrypted. You can encrypt a copy of an unencrypted snapshot using this flag, but you cannot use it to create an unencrypted copy from an encrypted snapshot. Your default CMK for EBS is used unless a non-default AWS Key Management Service (AWS KMS) CMK is specified with @KmsKeyId@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'csDestinationRegion' - The destination region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination region in a @PresignedUrl@ parameter, where it is required.
--
-- * 'csKMSKeyId' - An identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .      * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. The action will eventually fail.
--
-- * 'csDescription' - A description for the EBS snapshot.
--
-- * 'csDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'csSourceRegion' - The ID of the region that contains the snapshot to be copied.
--
-- * 'csSourceSnapshotId' - The ID of the EBS snapshot to copy.
copySnapshot
    :: Text -- ^ 'csSourceRegion'
    -> Text -- ^ 'csSourceSnapshotId'
    -> CopySnapshot
copySnapshot pSourceRegion_ pSourceSnapshotId_ =
  CopySnapshot'
    { _csPresignedURL = Nothing
    , _csEncrypted = Nothing
    , _csDestinationRegion = Nothing
    , _csKMSKeyId = Nothing
    , _csDescription = Nothing
    , _csDryRun = Nothing
    , _csSourceRegion = pSourceRegion_
    , _csSourceSnapshotId = pSourceSnapshotId_
    }


-- | When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> . The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
csPresignedURL :: Lens' CopySnapshot (Maybe Text)
csPresignedURL = lens _csPresignedURL (\ s a -> s{_csPresignedURL = a})

-- | Specifies whether the destination snapshot should be encrypted. You can encrypt a copy of an unencrypted snapshot using this flag, but you cannot use it to create an unencrypted copy from an encrypted snapshot. Your default CMK for EBS is used unless a non-default AWS Key Management Service (AWS KMS) CMK is specified with @KmsKeyId@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
csEncrypted :: Lens' CopySnapshot (Maybe Bool)
csEncrypted = lens _csEncrypted (\ s a -> s{_csEncrypted = a})

-- | The destination region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination region in a @PresignedUrl@ parameter, where it is required.
csDestinationRegion :: Lens' CopySnapshot (Maybe Text)
csDestinationRegion = lens _csDestinationRegion (\ s a -> s{_csDestinationRegion = a})

-- | An identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .      * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. The action will eventually fail.
csKMSKeyId :: Lens' CopySnapshot (Maybe Text)
csKMSKeyId = lens _csKMSKeyId (\ s a -> s{_csKMSKeyId = a})

-- | A description for the EBS snapshot.
csDescription :: Lens' CopySnapshot (Maybe Text)
csDescription = lens _csDescription (\ s a -> s{_csDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
csDryRun :: Lens' CopySnapshot (Maybe Bool)
csDryRun = lens _csDryRun (\ s a -> s{_csDryRun = a})

-- | The ID of the region that contains the snapshot to be copied.
csSourceRegion :: Lens' CopySnapshot Text
csSourceRegion = lens _csSourceRegion (\ s a -> s{_csSourceRegion = a})

-- | The ID of the EBS snapshot to copy.
csSourceSnapshotId :: Lens' CopySnapshot Text
csSourceSnapshotId = lens _csSourceSnapshotId (\ s a -> s{_csSourceSnapshotId = a})

instance AWSRequest CopySnapshot where
        type Rs CopySnapshot = CopySnapshotResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CopySnapshotResponse' <$>
                   (x .@? "snapshotId") <*> (pure (fromEnum s)))

instance Hashable CopySnapshot where

instance NFData CopySnapshot where

instance ToHeaders CopySnapshot where
        toHeaders = const mempty

instance ToPath CopySnapshot where
        toPath = const "/"

instance ToQuery CopySnapshot where
        toQuery CopySnapshot'{..}
          = mconcat
              ["Action" =: ("CopySnapshot" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "PresignedUrl" =: _csPresignedURL,
               "Encrypted" =: _csEncrypted,
               "DestinationRegion" =: _csDestinationRegion,
               "KmsKeyId" =: _csKMSKeyId,
               "Description" =: _csDescription,
               "DryRun" =: _csDryRun,
               "SourceRegion" =: _csSourceRegion,
               "SourceSnapshotId" =: _csSourceSnapshotId]

-- | Contains the output of CopySnapshot.
--
--
--
-- /See:/ 'copySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { _csrsSnapshotId     :: !(Maybe Text)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsSnapshotId' - The ID of the new snapshot.
--
-- * 'csrsResponseStatus' - -- | The response status code.
copySnapshotResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CopySnapshotResponse
copySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    {_csrsSnapshotId = Nothing, _csrsResponseStatus = pResponseStatus_}


-- | The ID of the new snapshot.
csrsSnapshotId :: Lens' CopySnapshotResponse (Maybe Text)
csrsSnapshotId = lens _csrsSnapshotId (\ s a -> s{_csrsSnapshotId = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CopySnapshotResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CopySnapshotResponse where
