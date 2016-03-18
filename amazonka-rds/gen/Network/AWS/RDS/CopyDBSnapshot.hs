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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB Snapshot. The source DB snapshot must be in the
-- \"available\" state.
--
-- If you are copying from a shared manual DB snapshot, the
-- 'SourceDBSnapshotIdentifier' must be the ARN of the shared DB snapshot.
module Network.AWS.RDS.CopyDBSnapshot
    (
    -- * Creating a Request
      copyDBSnapshot
    , CopyDBSnapshot
    -- * Request Lenses
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
-- /See:/ 'copyDBSnapshot' smart constructor.
data CopyDBSnapshot = CopyDBSnapshot'
    { _cdsCopyTags                   :: !(Maybe Bool)
    , _cdsKMSKeyId                   :: !(Maybe Text)
    , _cdsTags                       :: !(Maybe [Tag])
    , _cdsSourceDBSnapshotIdentifier :: !Text
    , _cdsTargetDBSnapshotIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CopyDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsCopyTags'
--
-- * 'cdsKMSKeyId'
--
-- * 'cdsTags'
--
-- * 'cdsSourceDBSnapshotIdentifier'
--
-- * 'cdsTargetDBSnapshotIdentifier'
copyDBSnapshot
    :: Text -- ^ 'cdsSourceDBSnapshotIdentifier'
    -> Text -- ^ 'cdsTargetDBSnapshotIdentifier'
    -> CopyDBSnapshot
copyDBSnapshot pSourceDBSnapshotIdentifier_ pTargetDBSnapshotIdentifier_ =
    CopyDBSnapshot'
    { _cdsCopyTags = Nothing
    , _cdsKMSKeyId = Nothing
    , _cdsTags = Nothing
    , _cdsSourceDBSnapshotIdentifier = pSourceDBSnapshotIdentifier_
    , _cdsTargetDBSnapshotIdentifier = pTargetDBSnapshotIdentifier_
    }

-- | True to copy all tags from the source DB snapshot to the target DB
-- snapshot; otherwise false. The default is false.
cdsCopyTags :: Lens' CopyDBSnapshot (Maybe Bool)
cdsCopyTags = lens _cdsCopyTags (\ s a -> s{_cdsCopyTags = a});

-- | The AWS Key Management Service (AWS KMS) key identifier for an encrypted
-- DB snapshot. The KMS key identifier is the Amazon Resource Name (ARN) or
-- the KMS key alias for the KMS encryption key.
--
-- If you copy an unencrypted DB snapshot and specify a value for the
-- 'KmsKeyId' parameter, Amazon RDS encrypts the target DB snapshot using
-- the specified KMS encryption key.
--
-- If you copy an encrypted DB snapshot from your AWS account, you can
-- specify a value for 'KmsKeyId' to encrypt the copy with a new KMS
-- encryption key. If you don\'t specify a value for 'KmsKeyId' then the
-- copy of the DB snapshot is encrypted with the same KMS key as the source
-- DB snapshot.
--
-- If you copy an encrypted DB snapshot that is shared from another AWS
-- account, then you must specify a value for 'KmsKeyId'.
cdsKMSKeyId :: Lens' CopyDBSnapshot (Maybe Text)
cdsKMSKeyId = lens _cdsKMSKeyId (\ s a -> s{_cdsKMSKeyId = a});

-- | Undocumented member.
cdsTags :: Lens' CopyDBSnapshot [Tag]
cdsTags = lens _cdsTags (\ s a -> s{_cdsTags = a}) . _Default . _Coerce;

-- | The identifier for the source DB snapshot.
--
-- If you are copying from a shared manual DB snapshot, this must be the
-- ARN of the shared DB snapshot.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
-- -   If the source snapshot is in the same region as the copy, specify a
--     valid DB snapshot identifier.
-- -   If the source snapshot is in a different region than the copy,
--     specify a valid DB snapshot ARN. For more information, go to
--     <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html Copying a DB Snapshot>.
--
-- Example: 'rds:mydb-2012-04-02-00-01'
--
-- Example:
-- 'arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805'
cdsSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsSourceDBSnapshotIdentifier = lens _cdsSourceDBSnapshotIdentifier (\ s a -> s{_cdsSourceDBSnapshotIdentifier = a});

-- | The identifier for the copied snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: 'my-db-snapshot'
cdsTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsTargetDBSnapshotIdentifier = lens _cdsTargetDBSnapshotIdentifier (\ s a -> s{_cdsTargetDBSnapshotIdentifier = a});

instance AWSRequest CopyDBSnapshot where
        type Rs CopyDBSnapshot = CopyDBSnapshotResponse
        request = postQuery rDS
        response
          = receiveXMLWrapper "CopyDBSnapshotResult"
              (\ s h x ->
                 CopyDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders CopyDBSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBSnapshot where
        toPath = const "/"

instance ToQuery CopyDBSnapshot where
        toQuery CopyDBSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
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
-- * 'cdsrsDBSnapshot'
--
-- * 'cdsrsResponseStatus'
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

-- | The response status code.
cdsrsResponseStatus :: Lens' CopyDBSnapshotResponse Int
cdsrsResponseStatus = lens _cdsrsResponseStatus (\ s a -> s{_cdsrsResponseStatus = a});
