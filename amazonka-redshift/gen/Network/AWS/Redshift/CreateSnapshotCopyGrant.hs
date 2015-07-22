{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateSnapshotCopyGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot copy grant that permits Amazon Redshift to use a
-- customer master key (CMK) from AWS Key Management Service (AWS KMS) to
-- encrypt copied snapshots in a destination region.
--
-- For more information about managing snapshot copy grants, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-db-encryption.html Amazon Redshift Database Encryption>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateSnapshotCopyGrant.html>
module Network.AWS.Redshift.CreateSnapshotCopyGrant
    (
    -- * Request
      CreateSnapshotCopyGrant
    -- ** Request constructor
    , createSnapshotCopyGrant
    -- ** Request lenses
    , cscgrqKMSKeyId
    , cscgrqTags
    , cscgrqSnapshotCopyGrantName

    -- * Response
    , CreateSnapshotCopyGrantResponse
    -- ** Response constructor
    , createSnapshotCopyGrantResponse
    -- ** Response lenses
    , cscgrsSnapshotCopyGrant
    , cscgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | The result of the @CreateSnapshotCopyGrant@ action.
--
-- /See:/ 'createSnapshotCopyGrant' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscgrqKMSKeyId'
--
-- * 'cscgrqTags'
--
-- * 'cscgrqSnapshotCopyGrantName'
data CreateSnapshotCopyGrant = CreateSnapshotCopyGrant'
    { _cscgrqKMSKeyId              :: !(Maybe Text)
    , _cscgrqTags                  :: !(Maybe [Tag])
    , _cscgrqSnapshotCopyGrantName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotCopyGrant' smart constructor.
createSnapshotCopyGrant :: Text -> CreateSnapshotCopyGrant
createSnapshotCopyGrant pSnapshotCopyGrantName =
    CreateSnapshotCopyGrant'
    { _cscgrqKMSKeyId = Nothing
    , _cscgrqTags = Nothing
    , _cscgrqSnapshotCopyGrantName = pSnapshotCopyGrantName
    }

-- | The unique identifier of the customer master key (CMK) to which to grant
-- Amazon Redshift permission. If no key is specified, the default key is
-- used.
cscgrqKMSKeyId :: Lens' CreateSnapshotCopyGrant (Maybe Text)
cscgrqKMSKeyId = lens _cscgrqKMSKeyId (\ s a -> s{_cscgrqKMSKeyId = a});

-- | A list of tag instances.
cscgrqTags :: Lens' CreateSnapshotCopyGrant [Tag]
cscgrqTags = lens _cscgrqTags (\ s a -> s{_cscgrqTags = a}) . _Default;

-- | The name of the snapshot copy grant. This name must be unique in the
-- region for the AWS account.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   Alphabetic characters must be lowercase.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   Must be unique for all clusters within an AWS account.
cscgrqSnapshotCopyGrantName :: Lens' CreateSnapshotCopyGrant Text
cscgrqSnapshotCopyGrantName = lens _cscgrqSnapshotCopyGrantName (\ s a -> s{_cscgrqSnapshotCopyGrantName = a});

instance AWSRequest CreateSnapshotCopyGrant where
        type Sv CreateSnapshotCopyGrant = Redshift
        type Rs CreateSnapshotCopyGrant =
             CreateSnapshotCopyGrantResponse
        request = post
        response
          = receiveXMLWrapper "CreateSnapshotCopyGrantResult"
              (\ s h x ->
                 CreateSnapshotCopyGrantResponse' <$>
                   (x .@? "SnapshotCopyGrant") <*> (pure (fromEnum s)))

instance ToHeaders CreateSnapshotCopyGrant where
        toHeaders = const mempty

instance ToPath CreateSnapshotCopyGrant where
        toPath = const "/"

instance ToQuery CreateSnapshotCopyGrant where
        toQuery CreateSnapshotCopyGrant'{..}
          = mconcat
              ["Action" =:
                 ("CreateSnapshotCopyGrant" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "KmsKeyId" =: _cscgrqKMSKeyId,
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cscgrqTags),
               "SnapshotCopyGrantName" =:
                 _cscgrqSnapshotCopyGrantName]

-- | /See:/ 'createSnapshotCopyGrantResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cscgrsSnapshotCopyGrant'
--
-- * 'cscgrsStatus'
data CreateSnapshotCopyGrantResponse = CreateSnapshotCopyGrantResponse'
    { _cscgrsSnapshotCopyGrant :: !(Maybe SnapshotCopyGrant)
    , _cscgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshotCopyGrantResponse' smart constructor.
createSnapshotCopyGrantResponse :: Int -> CreateSnapshotCopyGrantResponse
createSnapshotCopyGrantResponse pStatus =
    CreateSnapshotCopyGrantResponse'
    { _cscgrsSnapshotCopyGrant = Nothing
    , _cscgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cscgrsSnapshotCopyGrant :: Lens' CreateSnapshotCopyGrantResponse (Maybe SnapshotCopyGrant)
cscgrsSnapshotCopyGrant = lens _cscgrsSnapshotCopyGrant (\ s a -> s{_cscgrsSnapshotCopyGrant = a});

-- | FIXME: Undocumented member.
cscgrsStatus :: Lens' CreateSnapshotCopyGrantResponse Int
cscgrsStatus = lens _cscgrsStatus (\ s a -> s{_cscgrsStatus = a});
