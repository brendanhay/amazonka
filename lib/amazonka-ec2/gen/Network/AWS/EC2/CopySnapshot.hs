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
-- Module      : Network.AWS.EC2.CopySnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a point-in-time snapshot of an EBS volume and stores it in Amazon S3. You can copy the snapshot within the same Region or from one Region to another. You can use the snapshot to create EBS volumes or Amazon Machine Images (AMIs).
--
--
-- Copies of encrypted EBS snapshots remain encrypted. Copies of unencrypted snapshots remain unencrypted, unless you enable encryption for the snapshot copy operation. By default, encrypted snapshot copies use the default AWS Key Management Service (AWS KMS) customer master key (CMK); however, you can specify a different CMK.
--
-- To copy an encrypted snapshot that has been shared from another account, you must have permissions for the CMK used to encrypt the snapshot.
--
-- Snapshots created by copying another snapshot have an arbitrary volume ID that should not be used for any purpose.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copying an Amazon EBS snapshot> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CopySnapshot
  ( -- * Creating a Request
    copySnapshot,
    CopySnapshot,

    -- * Request Lenses
    copPresignedURL,
    copEncrypted,
    copTagSpecifications,
    copDestinationRegion,
    copKMSKeyId,
    copDescription,
    copDryRun,
    copSourceRegion,
    copSourceSnapshotId,

    -- * Destructuring the Response
    copySnapshotResponse,
    CopySnapshotResponse,

    -- * Response Lenses
    csrsTags,
    csrsSnapshotId,
    csrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { _copPresignedURL ::
      !(Maybe Text),
    _copEncrypted :: !(Maybe Bool),
    _copTagSpecifications :: !(Maybe [TagSpecification]),
    _copDestinationRegion :: !(Maybe Text),
    _copKMSKeyId :: !(Maybe Text),
    _copDescription :: !(Maybe Text),
    _copDryRun :: !(Maybe Bool),
    _copSourceRegion :: !Text,
    _copSourceSnapshotId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'copPresignedURL' - When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> . The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
--
-- * 'copEncrypted' - To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'copTagSpecifications' - The tags to apply to the new snapshot.
--
-- * 'copDestinationRegion' - The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required. The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
--
-- * 'copKMSKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
--
-- * 'copDescription' - A description for the EBS snapshot.
--
-- * 'copDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'copSourceRegion' - The ID of the Region that contains the snapshot to be copied.
--
-- * 'copSourceSnapshotId' - The ID of the EBS snapshot to copy.
copySnapshot ::
  -- | 'copSourceRegion'
  Text ->
  -- | 'copSourceSnapshotId'
  Text ->
  CopySnapshot
copySnapshot pSourceRegion_ pSourceSnapshotId_ =
  CopySnapshot'
    { _copPresignedURL = Nothing,
      _copEncrypted = Nothing,
      _copTagSpecifications = Nothing,
      _copDestinationRegion = Nothing,
      _copKMSKeyId = Nothing,
      _copDescription = Nothing,
      _copDryRun = Nothing,
      _copSourceRegion = pSourceRegion_,
      _copSourceSnapshotId = pSourceSnapshotId_
    }

-- | When you copy an encrypted source snapshot using the Amazon EC2 Query API, you must supply a pre-signed URL. This parameter is optional for unencrypted snapshots. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html Query Requests> . The @PresignedUrl@ should use the snapshot source endpoint, the @CopySnapshot@ action, and include the @SourceRegion@ , @SourceSnapshotId@ , and @DestinationRegion@ parameters. The @PresignedUrl@ must be signed using AWS Signature Version 4. Because EBS snapshots are stored in Amazon S3, the signing algorithm for this parameter uses the same logic that is described in <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests by Using Query Parameters (AWS Signature Version 4)> in the /Amazon Simple Storage Service API Reference/ . An invalid or improperly signed @PresignedUrl@ will cause the copy operation to fail asynchronously, and the snapshot will move to an @error@ state.
copPresignedURL :: Lens' CopySnapshot (Maybe Text)
copPresignedURL = lens _copPresignedURL (\s a -> s {_copPresignedURL = a})

-- | To encrypt a copy of an unencrypted snapshot if encryption by default is not enabled, enable encryption using this parameter. Otherwise, omit this parameter. Encrypted snapshots are encrypted, even if you omit this parameter and encryption by default is not enabled. You cannot set this parameter to false. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
copEncrypted :: Lens' CopySnapshot (Maybe Bool)
copEncrypted = lens _copEncrypted (\s a -> s {_copEncrypted = a})

-- | The tags to apply to the new snapshot.
copTagSpecifications :: Lens' CopySnapshot [TagSpecification]
copTagSpecifications = lens _copTagSpecifications (\s a -> s {_copTagSpecifications = a}) . _Default . _Coerce

-- | The destination Region to use in the @PresignedUrl@ parameter of a snapshot copy operation. This parameter is only valid for specifying the destination Region in a @PresignedUrl@ parameter, where it is required. The snapshot copy is sent to the regional endpoint that you sent the HTTP request to (for example, @ec2.us-east-1.amazonaws.com@ ). With the AWS CLI, this is specified using the @--region@ parameter or the default Region in your AWS configuration file.
copDestinationRegion :: Lens' CopySnapshot (Maybe Text)
copDestinationRegion = lens _copDestinationRegion (\s a -> s {_copDestinationRegion = a})

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
copKMSKeyId :: Lens' CopySnapshot (Maybe Text)
copKMSKeyId = lens _copKMSKeyId (\s a -> s {_copKMSKeyId = a})

-- | A description for the EBS snapshot.
copDescription :: Lens' CopySnapshot (Maybe Text)
copDescription = lens _copDescription (\s a -> s {_copDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
copDryRun :: Lens' CopySnapshot (Maybe Bool)
copDryRun = lens _copDryRun (\s a -> s {_copDryRun = a})

-- | The ID of the Region that contains the snapshot to be copied.
copSourceRegion :: Lens' CopySnapshot Text
copSourceRegion = lens _copSourceRegion (\s a -> s {_copSourceRegion = a})

-- | The ID of the EBS snapshot to copy.
copSourceSnapshotId :: Lens' CopySnapshot Text
copSourceSnapshotId = lens _copSourceSnapshotId (\s a -> s {_copSourceSnapshotId = a})

instance AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CopySnapshotResponse'
            <$> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "snapshotId")
            <*> (pure (fromEnum s))
      )

instance Hashable CopySnapshot

instance NFData CopySnapshot

instance ToHeaders CopySnapshot where
  toHeaders = const mempty

instance ToPath CopySnapshot where
  toPath = const "/"

instance ToQuery CopySnapshot where
  toQuery CopySnapshot' {..} =
    mconcat
      [ "Action" =: ("CopySnapshot" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "PresignedUrl" =: _copPresignedURL,
        "Encrypted" =: _copEncrypted,
        toQuery (toQueryList "TagSpecification" <$> _copTagSpecifications),
        "DestinationRegion" =: _copDestinationRegion,
        "KmsKeyId" =: _copKMSKeyId,
        "Description" =: _copDescription,
        "DryRun" =: _copDryRun,
        "SourceRegion" =: _copSourceRegion,
        "SourceSnapshotId" =: _copSourceSnapshotId
      ]

-- | /See:/ 'copySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { _csrsTags ::
      !(Maybe [Tag]),
    _csrsSnapshotId :: !(Maybe Text),
    _csrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsTags' - Any tags applied to the new snapshot.
--
-- * 'csrsSnapshotId' - The ID of the new snapshot.
--
-- * 'csrsResponseStatus' - -- | The response status code.
copySnapshotResponse ::
  -- | 'csrsResponseStatus'
  Int ->
  CopySnapshotResponse
copySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    { _csrsTags = Nothing,
      _csrsSnapshotId = Nothing,
      _csrsResponseStatus = pResponseStatus_
    }

-- | Any tags applied to the new snapshot.
csrsTags :: Lens' CopySnapshotResponse [Tag]
csrsTags = lens _csrsTags (\s a -> s {_csrsTags = a}) . _Default . _Coerce

-- | The ID of the new snapshot.
csrsSnapshotId :: Lens' CopySnapshotResponse (Maybe Text)
csrsSnapshotId = lens _csrsSnapshotId (\s a -> s {_csrsSnapshotId = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CopySnapshotResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\s a -> s {_csrsResponseStatus = a})

instance NFData CopySnapshotResponse
