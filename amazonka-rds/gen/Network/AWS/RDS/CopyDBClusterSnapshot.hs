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
-- Module      : Network.AWS.RDS.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a snapshot of a DB cluster.
--
--
-- To copy a DB cluster snapshot from a shared manual DB cluster snapshot, @SourceDBClusterSnapshotIdentifier@ must be the Amazon Resource Name (ARN) of the shared DB cluster snapshot.
--
-- You can copy an encrypted DB cluster snapshot from another AWS Region. In that case, the AWS Region where you call the @CopyDBClusterSnapshot@ action is the destination AWS Region for the encrypted DB cluster snapshot to be copied to. To copy an encrypted DB cluster snapshot from another AWS Region, you must provide the following values:
--
--     * @KmsKeyId@ - The AWS Key Management System (AWS KMS) key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region.
--
--     * @PreSignedUrl@ - A URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ action to be called in the source AWS Region where the DB cluster snapshot is copied from. The pre-signed URL must be a valid request for the @CopyDBClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied.
--
-- The pre-signed URL request must contain the following parameter values:
--
--     * @KmsKeyId@ - The KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.
--
--     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot will be created in.
--
--     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@ .
--
--
--
-- To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
--     * @TargetDBClusterSnapshotIdentifier@ - The identifier for the new copy of the DB cluster snapshot in the destination AWS Region.
--
--     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the ARN format for the source AWS Region and is the same value as the @SourceDBClusterSnapshotIdentifier@ in the pre-signed URL.
--
--
--
-- To cancel the copy operation once it is in progress, delete the target DB cluster snapshot identified by @TargetDBClusterSnapshotIdentifier@ while that DB cluster snapshot is in "copying" status.
--
-- For more information on copying encrypted DB cluster snapshots from one AWS Region to another, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopyDBClusterSnapshot.CrossRegion Copying a DB Cluster Snapshot in the Same Account, Either in the Same Region or Across Regions> in the Amazon RDS User Guide.
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.CopyDBClusterSnapshot
    (
    -- * Creating a Request
      copyDBClusterSnapshot
    , CopyDBClusterSnapshot
    -- * Request Lenses
    , cdbcsPreSignedURL
    , cdbcsCopyTags
    , cdbcsKMSKeyId
    , cdbcsTags
    , cdbcsSourceDBClusterSnapshotIdentifier
    , cdbcsTargetDBClusterSnapshotIdentifier

    -- * Destructuring the Response
    , copyDBClusterSnapshotResponse
    , CopyDBClusterSnapshotResponse
    -- * Response Lenses
    , cdcsrsDBClusterSnapshot
    , cdcsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'copyDBClusterSnapshot' smart constructor.
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
  { _cdbcsPreSignedURL                      :: !(Maybe Text)
  , _cdbcsCopyTags                          :: !(Maybe Bool)
  , _cdbcsKMSKeyId                          :: !(Maybe Text)
  , _cdbcsTags                              :: !(Maybe [Tag])
  , _cdbcsSourceDBClusterSnapshotIdentifier :: !Text
  , _cdbcsTargetDBClusterSnapshotIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbcsPreSignedURL' - The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB cluster snapshot from another AWS Region. The pre-signed URL must be a valid request for the @CopyDBSClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied. The pre-signed URL request must contain the following parameter values:     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot will be created in.     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@ . To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- * 'cdbcsCopyTags' - True to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot, and otherwise false. The default is false.
--
-- * 'cdbcsKMSKeyId' - The AWS AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot.  If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .  To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region. If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
--
-- * 'cdbcsTags' - Undocumented member.
--
-- * 'cdbcsSourceDBClusterSnapshotIdentifier' - The identifier of the DB cluster snapshot to copy. This parameter is not case-sensitive. You can't copy an encrypted, shared DB cluster snapshot from one AWS Region to another. Constraints:     * Must specify a valid system snapshot in the "available" state.     * If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier.     * If the source snapshot is in a different AWS Region than the copy, specify a valid DB cluster snapshot ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html Copying a DB Snapshot or DB Cluster Snapshot> . Example: @my-cluster-snapshot1@
--
-- * 'cdbcsTargetDBClusterSnapshotIdentifier' - The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter is not case-sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster-snapshot2@
copyDBClusterSnapshot
    :: Text -- ^ 'cdbcsSourceDBClusterSnapshotIdentifier'
    -> Text -- ^ 'cdbcsTargetDBClusterSnapshotIdentifier'
    -> CopyDBClusterSnapshot
copyDBClusterSnapshot pSourceDBClusterSnapshotIdentifier_ pTargetDBClusterSnapshotIdentifier_ =
  CopyDBClusterSnapshot'
    { _cdbcsPreSignedURL = Nothing
    , _cdbcsCopyTags = Nothing
    , _cdbcsKMSKeyId = Nothing
    , _cdbcsTags = Nothing
    , _cdbcsSourceDBClusterSnapshotIdentifier =
        pSourceDBClusterSnapshotIdentifier_
    , _cdbcsTargetDBClusterSnapshotIdentifier =
        pTargetDBClusterSnapshotIdentifier_
    }


-- | The URL that contains a Signature Version 4 signed request for the @CopyDBClusterSnapshot@ API action in the AWS Region that contains the source DB cluster snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB cluster snapshot from another AWS Region. The pre-signed URL must be a valid request for the @CopyDBSClusterSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB cluster snapshot to be copied. The pre-signed URL request must contain the following parameter values:     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBClusterSnapshot@ action that is called in the destination AWS Region, and the action contained in the pre-signed URL.     * @DestinationRegion@ - The name of the AWS Region that the DB cluster snapshot will be created in.     * @SourceDBClusterSnapshotIdentifier@ - The DB cluster snapshot identifier for the encrypted DB cluster snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB cluster snapshot from the us-west-2 AWS Region, then your @SourceDBClusterSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:cluster-snapshot:aurora-cluster1-snapshot-20161115@ . To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
cdbcsPreSignedURL :: Lens' CopyDBClusterSnapshot (Maybe Text)
cdbcsPreSignedURL = lens _cdbcsPreSignedURL (\ s a -> s{_cdbcsPreSignedURL = a})

-- | True to copy all tags from the source DB cluster snapshot to the target DB cluster snapshot, and otherwise false. The default is false.
cdbcsCopyTags :: Lens' CopyDBClusterSnapshot (Maybe Bool)
cdbcsCopyTags = lens _cdbcsCopyTags (\ s a -> s{_cdbcsCopyTags = a})

-- | The AWS AWS KMS key ID for an encrypted DB cluster snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you copy an encrypted DB cluster snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB cluster snapshot is encrypted with the same KMS key as the source DB cluster snapshot.  If you copy an encrypted DB cluster snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .  To copy an encrypted DB cluster snapshot to another AWS Region, you must set @KmsKeyId@ to the KMS key ID you want to use to encrypt the copy of the DB cluster snapshot in the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region. If you copy an unencrypted DB cluster snapshot and specify a value for the @KmsKeyId@ parameter, an error is returned.
cdbcsKMSKeyId :: Lens' CopyDBClusterSnapshot (Maybe Text)
cdbcsKMSKeyId = lens _cdbcsKMSKeyId (\ s a -> s{_cdbcsKMSKeyId = a})

-- | Undocumented member.
cdbcsTags :: Lens' CopyDBClusterSnapshot [Tag]
cdbcsTags = lens _cdbcsTags (\ s a -> s{_cdbcsTags = a}) . _Default . _Coerce

-- | The identifier of the DB cluster snapshot to copy. This parameter is not case-sensitive. You can't copy an encrypted, shared DB cluster snapshot from one AWS Region to another. Constraints:     * Must specify a valid system snapshot in the "available" state.     * If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier.     * If the source snapshot is in a different AWS Region than the copy, specify a valid DB cluster snapshot ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html Copying a DB Snapshot or DB Cluster Snapshot> . Example: @my-cluster-snapshot1@
cdbcsSourceDBClusterSnapshotIdentifier :: Lens' CopyDBClusterSnapshot Text
cdbcsSourceDBClusterSnapshotIdentifier = lens _cdbcsSourceDBClusterSnapshotIdentifier (\ s a -> s{_cdbcsSourceDBClusterSnapshotIdentifier = a})

-- | The identifier of the new DB cluster snapshot to create from the source DB cluster snapshot. This parameter is not case-sensitive. Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens.     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens. Example: @my-cluster-snapshot2@
cdbcsTargetDBClusterSnapshotIdentifier :: Lens' CopyDBClusterSnapshot Text
cdbcsTargetDBClusterSnapshotIdentifier = lens _cdbcsTargetDBClusterSnapshotIdentifier (\ s a -> s{_cdbcsTargetDBClusterSnapshotIdentifier = a})

instance AWSRequest CopyDBClusterSnapshot where
        type Rs CopyDBClusterSnapshot =
             CopyDBClusterSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CopyDBClusterSnapshotResult"
              (\ s h x ->
                 CopyDBClusterSnapshotResponse' <$>
                   (x .@? "DBClusterSnapshot") <*> (pure (fromEnum s)))

instance Hashable CopyDBClusterSnapshot where

instance NFData CopyDBClusterSnapshot where

instance ToHeaders CopyDBClusterSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBClusterSnapshot where
        toPath = const "/"

instance ToQuery CopyDBClusterSnapshot where
        toQuery CopyDBClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBClusterSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PreSignedUrl" =: _cdbcsPreSignedURL,
               "CopyTags" =: _cdbcsCopyTags,
               "KmsKeyId" =: _cdbcsKMSKeyId,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbcsTags),
               "SourceDBClusterSnapshotIdentifier" =:
                 _cdbcsSourceDBClusterSnapshotIdentifier,
               "TargetDBClusterSnapshotIdentifier" =:
                 _cdbcsTargetDBClusterSnapshotIdentifier]

-- | /See:/ 'copyDBClusterSnapshotResponse' smart constructor.
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
  { _cdcsrsDBClusterSnapshot :: !(Maybe DBClusterSnapshot)
  , _cdcsrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcsrsDBClusterSnapshot' - Undocumented member.
--
-- * 'cdcsrsResponseStatus' - -- | The response status code.
copyDBClusterSnapshotResponse
    :: Int -- ^ 'cdcsrsResponseStatus'
    -> CopyDBClusterSnapshotResponse
copyDBClusterSnapshotResponse pResponseStatus_ =
  CopyDBClusterSnapshotResponse'
    { _cdcsrsDBClusterSnapshot = Nothing
    , _cdcsrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cdcsrsDBClusterSnapshot :: Lens' CopyDBClusterSnapshotResponse (Maybe DBClusterSnapshot)
cdcsrsDBClusterSnapshot = lens _cdcsrsDBClusterSnapshot (\ s a -> s{_cdcsrsDBClusterSnapshot = a})

-- | -- | The response status code.
cdcsrsResponseStatus :: Lens' CopyDBClusterSnapshotResponse Int
cdcsrsResponseStatus = lens _cdcsrsResponseStatus (\ s a -> s{_cdcsrsResponseStatus = a})

instance NFData CopyDBClusterSnapshotResponse where
