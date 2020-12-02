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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB snapshot. The source DB snapshot must be in the "available" state.
--
--
-- You can copy a snapshot from one AWS Region to another. In that case, the AWS Region where you call the @CopyDBSnapshot@ action is the destination AWS Region for the DB snapshot copy.
--
-- For more information about copying snapshots, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopyDBSnapshot.html Copying a DB Snapshot> in the Amazon RDS User Guide.
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
    , cdsOptionGroupName
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
-- /See:/ 'copyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
  { _cdsPreSignedURL               :: !(Maybe Text)
  , _cdsCopyTags                   :: !(Maybe Bool)
  , _cdsKMSKeyId                   :: !(Maybe Text)
  , _cdsOptionGroupName            :: !(Maybe Text)
  , _cdsTags                       :: !(Maybe [Tag])
  , _cdsSourceDBSnapshotIdentifier :: !Text
  , _cdsTargetDBSnapshotIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsPreSignedURL' - The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the source AWS Region that contains the source DB snapshot to copy.  You must specify this parameter when you copy an encrypted DB snapshot from another AWS Region by using the Amazon RDS API. You can specify the @--source-region@ option instead of this parameter when you copy an encrypted DB snapshot from another AWS Region by using the AWS CLI.  The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB snapshot to be copied. The presigned URL request must contain the following parameter values:      * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot is copied to. This AWS Region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.  For example, if you copy an encrypted DB snapshot from the us-west-2 AWS Region to the us-east-1 AWS Region, then you call the @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.      * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBSnapshot@ action that is called in the destination AWS Region, and the action contained in the presigned URL.      * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB snapshot from the us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ .  To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
--
-- * 'cdsCopyTags' - True to copy all tags from the source DB snapshot to the target DB snapshot, and otherwise false. The default is false.
--
-- * 'cdsKMSKeyId' - The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you copy an encrypted DB snapshot from your AWS account, you can specify a value for this parameter to encrypt the copy with a new KMS encryption key. If you don't specify a value for this parameter, then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.  If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for this parameter.  If you specify this parameter when you copy an unencrypted snapshot, the copy is encrypted.  If you copy an encrypted snapshot to a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
--
-- * 'cdsOptionGroupName' - The name of an option group to associate with the copy of the snapshot. Specify this option if you are copying a snapshot from one AWS Region to another, and your DB instance uses a nondefault option group. If your source DB instance uses Transparent Data Encryption for Oracle or Microsoft SQL Server, you must specify this option when copying across AWS Regions. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option Group Considerations> .
--
-- * 'cdsTags' - Undocumented member.
--
-- * 'cdsSourceDBSnapshotIdentifier' - The identifier for the source DB snapshot. If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier. For example, you might specify @rds:mysql-instance1-snapshot-20130805@ .  If the source snapshot is in a different AWS Region than the copy, specify a valid DB snapshot ARN. For example, you might specify @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@ .  If you are copying from a shared manual DB snapshot, this parameter must be the Amazon Resource Name (ARN) of the shared DB snapshot.  If you are copying an encrypted snapshot this parameter must be in the ARN format for the source AWS Region, and must match the @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.  Constraints:     * Must specify a valid system snapshot in the "available" state. Example: @rds:mydb-2012-04-02-00-01@  Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
--
-- * 'cdsTargetDBSnapshotIdentifier' - The identifier for the copy of the snapshot.  Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-db-snapshot@
copyDBSnapshot
    :: Text -- ^ 'cdsSourceDBSnapshotIdentifier'
    -> Text -- ^ 'cdsTargetDBSnapshotIdentifier'
    -> CopyDBSnapshot
copyDBSnapshot pSourceDBSnapshotIdentifier_ pTargetDBSnapshotIdentifier_ =
  CopyDBSnapshot'
    { _cdsPreSignedURL = Nothing
    , _cdsCopyTags = Nothing
    , _cdsKMSKeyId = Nothing
    , _cdsOptionGroupName = Nothing
    , _cdsTags = Nothing
    , _cdsSourceDBSnapshotIdentifier = pSourceDBSnapshotIdentifier_
    , _cdsTargetDBSnapshotIdentifier = pTargetDBSnapshotIdentifier_
    }


-- | The URL that contains a Signature Version 4 signed request for the @CopyDBSnapshot@ API action in the source AWS Region that contains the source DB snapshot to copy.  You must specify this parameter when you copy an encrypted DB snapshot from another AWS Region by using the Amazon RDS API. You can specify the @--source-region@ option instead of this parameter when you copy an encrypted DB snapshot from another AWS Region by using the AWS CLI.  The presigned URL must be a valid request for the @CopyDBSnapshot@ API action that can be executed in the source AWS Region that contains the encrypted DB snapshot to be copied. The presigned URL request must contain the following parameter values:      * @DestinationRegion@ - The AWS Region that the encrypted DB snapshot is copied to. This AWS Region is the same one where the @CopyDBSnapshot@ action is called that contains this presigned URL.  For example, if you copy an encrypted DB snapshot from the us-west-2 AWS Region to the us-east-1 AWS Region, then you call the @CopyDBSnapshot@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CopyDBSnapshot@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region.      * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the copy of the DB snapshot in the destination AWS Region. This is the same identifier for both the @CopyDBSnapshot@ action that is called in the destination AWS Region, and the action contained in the presigned URL.      * @SourceDBSnapshotIdentifier@ - The DB snapshot identifier for the encrypted snapshot to be copied. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are copying an encrypted DB snapshot from the us-west-2 AWS Region, then your @SourceDBSnapshotIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20161115@ .  To learn how to generate a Signature Version 4 signed request, see <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> .
cdsPreSignedURL :: Lens' CopyDBSnapshot (Maybe Text)
cdsPreSignedURL = lens _cdsPreSignedURL (\ s a -> s{_cdsPreSignedURL = a})

-- | True to copy all tags from the source DB snapshot to the target DB snapshot, and otherwise false. The default is false.
cdsCopyTags :: Lens' CopyDBSnapshot (Maybe Bool)
cdsCopyTags = lens _cdsCopyTags (\ s a -> s{_cdsCopyTags = a})

-- | The AWS KMS key ID for an encrypted DB snapshot. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.  If you copy an encrypted DB snapshot from your AWS account, you can specify a value for this parameter to encrypt the copy with a new KMS encryption key. If you don't specify a value for this parameter, then the copy of the DB snapshot is encrypted with the same KMS key as the source DB snapshot.  If you copy an encrypted DB snapshot that is shared from another AWS account, then you must specify a value for this parameter.  If you specify this parameter when you copy an unencrypted snapshot, the copy is encrypted.  If you copy an encrypted snapshot to a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
cdsKMSKeyId :: Lens' CopyDBSnapshot (Maybe Text)
cdsKMSKeyId = lens _cdsKMSKeyId (\ s a -> s{_cdsKMSKeyId = a})

-- | The name of an option group to associate with the copy of the snapshot. Specify this option if you are copying a snapshot from one AWS Region to another, and your DB instance uses a nondefault option group. If your source DB instance uses Transparent Data Encryption for Oracle or Microsoft SQL Server, you must specify this option when copying across AWS Regions. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html#USER_CopySnapshot.Options Option Group Considerations> .
cdsOptionGroupName :: Lens' CopyDBSnapshot (Maybe Text)
cdsOptionGroupName = lens _cdsOptionGroupName (\ s a -> s{_cdsOptionGroupName = a})

-- | Undocumented member.
cdsTags :: Lens' CopyDBSnapshot [Tag]
cdsTags = lens _cdsTags (\ s a -> s{_cdsTags = a}) . _Default . _Coerce

-- | The identifier for the source DB snapshot. If the source snapshot is in the same AWS Region as the copy, specify a valid DB snapshot identifier. For example, you might specify @rds:mysql-instance1-snapshot-20130805@ .  If the source snapshot is in a different AWS Region than the copy, specify a valid DB snapshot ARN. For example, you might specify @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@ .  If you are copying from a shared manual DB snapshot, this parameter must be the Amazon Resource Name (ARN) of the shared DB snapshot.  If you are copying an encrypted snapshot this parameter must be in the ARN format for the source AWS Region, and must match the @SourceDBSnapshotIdentifier@ in the @PreSignedUrl@ parameter.  Constraints:     * Must specify a valid system snapshot in the "available" state. Example: @rds:mydb-2012-04-02-00-01@  Example: @arn:aws:rds:us-west-2:123456789012:snapshot:mysql-instance1-snapshot-20130805@
cdsSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsSourceDBSnapshotIdentifier = lens _cdsSourceDBSnapshotIdentifier (\ s a -> s{_cdsSourceDBSnapshotIdentifier = a})

-- | The identifier for the copy of the snapshot.  Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-db-snapshot@
cdsTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsTargetDBSnapshotIdentifier = lens _cdsTargetDBSnapshotIdentifier (\ s a -> s{_cdsTargetDBSnapshotIdentifier = a})

instance AWSRequest CopyDBSnapshot where
        type Rs CopyDBSnapshot = CopyDBSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CopyDBSnapshotResult"
              (\ s h x ->
                 CopyDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance Hashable CopyDBSnapshot where

instance NFData CopyDBSnapshot where

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
               "OptionGroupName" =: _cdsOptionGroupName,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdsTags),
               "SourceDBSnapshotIdentifier" =:
                 _cdsSourceDBSnapshotIdentifier,
               "TargetDBSnapshotIdentifier" =:
                 _cdsTargetDBSnapshotIdentifier]

-- | /See:/ 'copyDBSnapshotResponse' smart constructor.
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
  { _cdsrsDBSnapshot     :: !(Maybe DBSnapshot)
  , _cdsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
    {_cdsrsDBSnapshot = Nothing, _cdsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdsrsDBSnapshot :: Lens' CopyDBSnapshotResponse (Maybe DBSnapshot)
cdsrsDBSnapshot = lens _cdsrsDBSnapshot (\ s a -> s{_cdsrsDBSnapshot = a})

-- | -- | The response status code.
cdsrsResponseStatus :: Lens' CopyDBSnapshotResponse Int
cdsrsResponseStatus = lens _cdsrsResponseStatus (\ s a -> s{_cdsrsResponseStatus = a})

instance NFData CopyDBSnapshotResponse where
