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
-- Module      : Network.AWS.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB snapshot. The source DB snapshot must be in the "available" state.
--
--
-- To copy a DB snapshot from a shared manual DB snapshot, @SourceDBSnapshotIdentifier@ must be the Amazon Resource Name (ARN) of the shared DB snapshot.
--
-- You can copy an encrypted DB snapshot from another AWS Region. In that case, the region where you call the @CopyDBSnapshot@ action is the destination region for the encrypted DB snapshot to be copied to. To copy an encrypted DB snapshot from another region, you must provide the following values:
--
--     * @KmsKeyId@ - The AWS Key Management System (KMS) key identifier for the key to use to encrypt the copy of the DB snapshot in the destination region.
--
--     * @PreSignedUrl@ - A URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ action to be called in the source region where the DB snapshot will be copied from. The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source region that contains the encrypted DB snapshot to be copied.
--
-- The presigned URL request must contain the following parameter values:
--
--     * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot will be copied to. This region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.
--
-- For example, if you copy an encrypted DB snapshot from the us-west-2 region to the us-east-1 region, then you will call the @CopyDBSnapshot@ action in the us-east-1 region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 region.
--
--     * @KmsKeyId@ - The KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination region. This identifier is the same for both the @CopyDBSnapshot@ action that is called in the destination region, and the action contained in the presigned URL.
--
--     * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source region. For example, if you copy an encrypted DB snapshot from the us-west-2 region, then your @SourceDBSnapshotIdentifier@ looks like this example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ .
--
--
--
-- To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
--     * @TargetDBSnapshotIdentifier@ - The identifier for the new copy of the DB snapshot in the destination region.
--
--     * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the ARN format for the source region and is the same value as the @SourceDBSnapshotIdentifier@ in the presigned URL.
--
--
--
-- For more information on copying encrypted snapshots from one region to another, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Encrypted.CrossRegion Copying an Encrypted DB Snapshot to Another Region> in the Amazon RDS User Guide.
--
module Network.AWS.RDS.CopyDBSnapshot
    (
    -- * Creating a Request
      copyDBSnapshot
    , CopyDBSnapshot
    -- * Request Lenses
    , cdsPreSignedURL
    , cdsCopyTags
    , cdsKMSKeyId
    , cdsTags
    , cdsSourceDBSnapshotIdentifier
    , cdsTargetDBSnapshotIdentifier

    -- * Destructuring the Response
    , copyDBSnapshotResponse
    , CopyDBSnapshotResponse
    -- * Response Lenses
    , cdsrsDBSnapshot
    , cdsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
--
--
-- /See:/ 'copyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
    { _cdsPreSignedURL               :: !(Maybe Text)
    , _cdsCopyTags                   :: !(Maybe Bool)
    , _cdsKMSKeyId                   :: !(Maybe Text)
    , _cdsTags                       :: !(Maybe [Tag])
    , _cdsSourceDBSnapshotIdentifier :: !Text
    , _cdsTargetDBSnapshotIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsPreSignedURL' - The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the AWS region that contains the source DB snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB snapshot from another AWS region. The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source region that contains the encrypted DB snapshot to be copied. The presigned URL request must contain the following parameter values:     * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot will be copied to. This region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.  For example, if you copy an encrypted DB snapshot from the us-west-2 region to the us-east-1 region, then you will call the @CopyDBSnapshot@ action in the us-east-1 region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 region.     * @KmsKeyId@ - The KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination region. This is the same identifier for both the @CopyDBSnapshot@ action that is called in the destination region, and the action contained in the presigned URL.     * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source region. For example, if you are copying an encrypted DB snapshot from the us-west-2 region, then your @SourceDBSnapshotIdentifier@ would look like Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ . To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- * 'cdsCopyTags' - True to copy all tags from the source DB snapshot to the target DB snapshot; otherwise false. The default is false.
--
-- * 'cdsKMSKeyId' - The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you copy an unencrypted DB snapshot and specify a value for the @KmsKeyId@ parameter, Amazon RDS encrypts the target DB snapshot using the specified KMS encryption key.  If you copy an encrypted DB snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.  If you copy an encrypted snapshot to a different AWS region, then you must specify a KMS key for the destination AWS region. If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .  To copy an encrypted DB snapshot to another region, you must set @KmsKeyId@ to the KMS key ID used to encrypt the copy of the DB snapshot in the destination region. KMS encryption keys are specific to the region that they are created in, and you cannot use encryption keys from one region in another region.
--
-- * 'cdsTags' - Undocumented member.
--
-- * 'cdsSourceDBSnapshotIdentifier' - The identifier for the source DB snapshot. If you are copying from a shared manual DB snapshot, this must be the ARN of the shared DB snapshot. You cannot copy an encrypted, shared DB snapshot from one AWS region to another. Constraints:     * Must specify a valid system snapshot in the "available" state.     * If the source snapshot is in the same region as the copy, specify a valid DB snapshot identifier.     * If the source snapshot is in a different region than the copy, specify a valid DB snapshot ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html Copying a DB Snapshot> . Example: @rds:mydb-2012-04-02-00-01@  Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
--
-- * 'cdsTargetDBSnapshotIdentifier' - The identifier for the copied snapshot. Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-db-snapshot@
copyDBSnapshot
    :: Text -- ^ 'cdsSourceDBSnapshotIdentifier'
    -> Text -- ^ 'cdsTargetDBSnapshotIdentifier'
    -> CopyDBSnapshot
copyDBSnapshot pSourceDBSnapshotIdentifier_ pTargetDBSnapshotIdentifier_ =
    CopyDBSnapshot'
    { _cdsPreSignedURL = Nothing
    , _cdsCopyTags = Nothing
    , _cdsKMSKeyId = Nothing
    , _cdsTags = Nothing
    , _cdsSourceDBSnapshotIdentifier = pSourceDBSnapshotIdentifier_
    , _cdsTargetDBSnapshotIdentifier = pTargetDBSnapshotIdentifier_
    }

-- | The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the AWS region that contains the source DB snapshot to copy. The @PreSignedUrl@ parameter must be used when copying an encrypted DB snapshot from another AWS region. The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source region that contains the encrypted DB snapshot to be copied. The presigned URL request must contain the following parameter values:     * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot will be copied to. This region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.  For example, if you copy an encrypted DB snapshot from the us-west-2 region to the us-east-1 region, then you will call the @CopyDBSnapshot@ action in the us-east-1 region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 region.     * @KmsKeyId@ - The KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination region. This is the same identifier for both the @CopyDBSnapshot@ action that is called in the destination region, and the action contained in the presigned URL.     * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source region. For example, if you are copying an encrypted DB snapshot from the us-west-2 region, then your @SourceDBSnapshotIdentifier@ would look like Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ . To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
cdsPreSignedURL :: Lens' CopyDBSnapshot (Maybe Text)
cdsPreSignedURL = lens _cdsPreSignedURL (\ s a -> s{_cdsPreSignedURL = a});

-- | True to copy all tags from the source DB snapshot to the target DB snapshot; otherwise false. The default is false.
cdsCopyTags :: Lens' CopyDBSnapshot (Maybe Bool)
cdsCopyTags = lens _cdsCopyTags (\ s a -> s{_cdsCopyTags = a});

-- | The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you copy an unencrypted DB snapshot and specify a value for the @KmsKeyId@ parameter, Amazon RDS encrypts the target DB snapshot using the specified KMS encryption key.  If you copy an encrypted DB snapshot from your AWS account, you can specify a value for @KmsKeyId@ to encrypt the copy with a new KMS encryption key. If you don't specify a value for @KmsKeyId@ , then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.  If you copy an encrypted snapshot to a different AWS region, then you must specify a KMS key for the destination AWS region. If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for @KmsKeyId@ .  To copy an encrypted DB snapshot to another region, you must set @KmsKeyId@ to the KMS key ID used to encrypt the copy of the DB snapshot in the destination region. KMS encryption keys are specific to the region that they are created in, and you cannot use encryption keys from one region in another region.
cdsKMSKeyId :: Lens' CopyDBSnapshot (Maybe Text)
cdsKMSKeyId = lens _cdsKMSKeyId (\ s a -> s{_cdsKMSKeyId = a});

-- | Undocumented member.
cdsTags :: Lens' CopyDBSnapshot [Tag]
cdsTags = lens _cdsTags (\ s a -> s{_cdsTags = a}) . _Default . _Coerce;

-- | The identifier for the source DB snapshot. If you are copying from a shared manual DB snapshot, this must be the ARN of the shared DB snapshot. You cannot copy an encrypted, shared DB snapshot from one AWS region to another. Constraints:     * Must specify a valid system snapshot in the "available" state.     * If the source snapshot is in the same region as the copy, specify a valid DB snapshot identifier.     * If the source snapshot is in a different region than the copy, specify a valid DB snapshot ARN. For more information, go to <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html Copying a DB Snapshot> . Example: @rds:mydb-2012-04-02-00-01@  Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
cdsSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsSourceDBSnapshotIdentifier = lens _cdsSourceDBSnapshotIdentifier (\ s a -> s{_cdsSourceDBSnapshotIdentifier = a});

-- | The identifier for the copied snapshot. Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 alphanumeric characters or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-db-snapshot@
cdsTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsTargetDBSnapshotIdentifier = lens _cdsTargetDBSnapshotIdentifier (\ s a -> s{_cdsTargetDBSnapshotIdentifier = a});

instance AWSRequest CopyDBSnapshot where
        type Rs CopyDBSnapshot = CopyDBSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CopyDBSnapshotResult"
              (\ s h x ->
                 CopyDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance Hashable CopyDBSnapshot

instance NFData CopyDBSnapshot

instance ToHeaders CopyDBSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBSnapshot where
        toPath = const "/"

instance ToQuery CopyDBSnapshot where
        toQuery CopyDBSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "PreSignedUrl" =: _cdsPreSignedURL,
               "CopyTags" =: _cdsCopyTags,
               "KmsKeyId" =: _cdsKMSKeyId,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdsTags),
               "SourceDBSnapshotIdentifier" =:
                 _cdsSourceDBSnapshotIdentifier,
               "TargetDBSnapshotIdentifier" =:
                 _cdsTargetDBSnapshotIdentifier]

-- | /See:/ 'copyDBSnapshotResponse' smart constructor.
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
    { _cdsrsDBSnapshot     :: !(Maybe DBSnapshot)
    , _cdsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsrsDBSnapshot' - Undocumented member.
--
-- * 'cdsrsResponseStatus' - -- | The response status code.
copyDBSnapshotResponse
    :: Int -- ^ 'cdsrsResponseStatus'
    -> CopyDBSnapshotResponse
copyDBSnapshotResponse pResponseStatus_ =
    CopyDBSnapshotResponse'
    { _cdsrsDBSnapshot = Nothing
    , _cdsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cdsrsDBSnapshot :: Lens' CopyDBSnapshotResponse (Maybe DBSnapshot)
cdsrsDBSnapshot = lens _cdsrsDBSnapshot (\ s a -> s{_cdsrsDBSnapshot = a});

-- | -- | The response status code.
cdsrsResponseStatus :: Lens' CopyDBSnapshotResponse Int
cdsrsResponseStatus = lens _cdsrsResponseStatus (\ s a -> s{_cdsrsResponseStatus = a});

instance NFData CopyDBSnapshotResponse
