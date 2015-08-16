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
-- Module      : Network.AWS.Redshift.CreateSnapshotCopyGrant
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateSnapshotCopyGrant.html AWS API Reference> for CreateSnapshotCopyGrant.
module Network.AWS.Redshift.CreateSnapshotCopyGrant
    (
    -- * Creating a Request
      createSnapshotCopyGrant
    , CreateSnapshotCopyGrant
    -- * Request Lenses
    , cscgKMSKeyId
    , cscgTags
    , cscgSnapshotCopyGrantName

    -- * Destructuring the Response
    , createSnapshotCopyGrantResponse
    , CreateSnapshotCopyGrantResponse
    -- * Response Lenses
    , cscgrsSnapshotCopyGrant
    , cscgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- | The result of the 'CreateSnapshotCopyGrant' action.
--
-- /See:/ 'createSnapshotCopyGrant' smart constructor.
data CreateSnapshotCopyGrant = CreateSnapshotCopyGrant'
    { _cscgKMSKeyId              :: !(Maybe Text)
    , _cscgTags                  :: !(Maybe [Tag])
    , _cscgSnapshotCopyGrantName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSnapshotCopyGrant' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscgKMSKeyId'
--
-- * 'cscgTags'
--
-- * 'cscgSnapshotCopyGrantName'
createSnapshotCopyGrant
    :: Text -- ^ 'cscgSnapshotCopyGrantName'
    -> CreateSnapshotCopyGrant
createSnapshotCopyGrant pSnapshotCopyGrantName_ =
    CreateSnapshotCopyGrant'
    { _cscgKMSKeyId = Nothing
    , _cscgTags = Nothing
    , _cscgSnapshotCopyGrantName = pSnapshotCopyGrantName_
    }

-- | The unique identifier of the customer master key (CMK) to which to grant
-- Amazon Redshift permission. If no key is specified, the default key is
-- used.
cscgKMSKeyId :: Lens' CreateSnapshotCopyGrant (Maybe Text)
cscgKMSKeyId = lens _cscgKMSKeyId (\ s a -> s{_cscgKMSKeyId = a});

-- | A list of tag instances.
cscgTags :: Lens' CreateSnapshotCopyGrant [Tag]
cscgTags = lens _cscgTags (\ s a -> s{_cscgTags = a}) . _Default . _Coerce;

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
cscgSnapshotCopyGrantName :: Lens' CreateSnapshotCopyGrant Text
cscgSnapshotCopyGrantName = lens _cscgSnapshotCopyGrantName (\ s a -> s{_cscgSnapshotCopyGrantName = a});

instance AWSRequest CreateSnapshotCopyGrant where
        type Sv CreateSnapshotCopyGrant = Redshift
        type Rs CreateSnapshotCopyGrant =
             CreateSnapshotCopyGrantResponse
        request = postQuery
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
               "KmsKeyId" =: _cscgKMSKeyId,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cscgTags),
               "SnapshotCopyGrantName" =:
                 _cscgSnapshotCopyGrantName]

-- | /See:/ 'createSnapshotCopyGrantResponse' smart constructor.
data CreateSnapshotCopyGrantResponse = CreateSnapshotCopyGrantResponse'
    { _cscgrsSnapshotCopyGrant :: !(Maybe SnapshotCopyGrant)
    , _cscgrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateSnapshotCopyGrantResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cscgrsSnapshotCopyGrant'
--
-- * 'cscgrsStatus'
createSnapshotCopyGrantResponse
    :: Int -- ^ 'cscgrsStatus'
    -> CreateSnapshotCopyGrantResponse
createSnapshotCopyGrantResponse pStatus_ =
    CreateSnapshotCopyGrantResponse'
    { _cscgrsSnapshotCopyGrant = Nothing
    , _cscgrsStatus = pStatus_
    }

-- | Undocumented member.
cscgrsSnapshotCopyGrant :: Lens' CreateSnapshotCopyGrantResponse (Maybe SnapshotCopyGrant)
cscgrsSnapshotCopyGrant = lens _cscgrsSnapshotCopyGrant (\ s a -> s{_cscgrsSnapshotCopyGrant = a});

-- | The response status code.
cscgrsStatus :: Lens' CreateSnapshotCopyGrantResponse Int
cscgrsStatus = lens _cscgrsStatus (\ s a -> s{_cscgrsStatus = a});
