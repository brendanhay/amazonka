{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB cluster. For more information on Amazon
-- Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBClusterSnapshot.html>
module Network.AWS.RDS.CreateDBClusterSnapshot
    (
    -- * Request
      CreateDBClusterSnapshot
    -- ** Request constructor
    , createDBClusterSnapshot
    -- ** Request lenses
    , cdcsTags
    , cdcsDBClusterSnapshotIdentifier
    , cdcsDBClusterIdentifier

    -- * Response
    , CreateDBClusterSnapshotResponse
    -- ** Response constructor
    , createDBClusterSnapshotResponse
    -- ** Response lenses
    , cdbcsrsDBClusterSnapshot
    , cdbcsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcsTags'
--
-- * 'cdcsDBClusterSnapshotIdentifier'
--
-- * 'cdcsDBClusterIdentifier'
data CreateDBClusterSnapshot = CreateDBClusterSnapshot'
    { _cdcsTags                        :: !(Maybe [Tag])
    , _cdcsDBClusterSnapshotIdentifier :: !Text
    , _cdcsDBClusterIdentifier         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBClusterSnapshot' smart constructor.
createDBClusterSnapshot :: Text -> Text -> CreateDBClusterSnapshot
createDBClusterSnapshot pDBClusterSnapshotIdentifier_ pDBClusterIdentifier_ =
    CreateDBClusterSnapshot'
    { _cdcsTags = Nothing
    , _cdcsDBClusterSnapshotIdentifier = pDBClusterSnapshotIdentifier_
    , _cdcsDBClusterIdentifier = pDBClusterIdentifier_
    }

-- | The tags to be assigned to the DB cluster snapshot.
cdcsTags :: Lens' CreateDBClusterSnapshot [Tag]
cdcsTags = lens _cdcsTags (\ s a -> s{_cdcsTags = a}) . _Default . _Coerce;

-- | The identifier of the DB cluster snapshot. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1-snapshot1@
cdcsDBClusterSnapshotIdentifier :: Lens' CreateDBClusterSnapshot Text
cdcsDBClusterSnapshotIdentifier = lens _cdcsDBClusterSnapshotIdentifier (\ s a -> s{_cdcsDBClusterSnapshotIdentifier = a});

-- | The identifier of the DB cluster to create a snapshot for. This
-- parameter is not case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
cdcsDBClusterIdentifier :: Lens' CreateDBClusterSnapshot Text
cdcsDBClusterIdentifier = lens _cdcsDBClusterIdentifier (\ s a -> s{_cdcsDBClusterIdentifier = a});

instance AWSRequest CreateDBClusterSnapshot where
        type Sv CreateDBClusterSnapshot = RDS
        type Rs CreateDBClusterSnapshot =
             CreateDBClusterSnapshotResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateDBClusterSnapshotResult"
              (\ s h x ->
                 CreateDBClusterSnapshotResponse' <$>
                   (x .@? "DBClusterSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders CreateDBClusterSnapshot where
        toHeaders = const mempty

instance ToPath CreateDBClusterSnapshot where
        toPath = const "/"

instance ToQuery CreateDBClusterSnapshot where
        toQuery CreateDBClusterSnapshot'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBClusterSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdcsTags),
               "DBClusterSnapshotIdentifier" =:
                 _cdcsDBClusterSnapshotIdentifier,
               "DBClusterIdentifier" =: _cdcsDBClusterIdentifier]

-- | /See:/ 'createDBClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbcsrsDBClusterSnapshot'
--
-- * 'cdbcsrsStatus'
data CreateDBClusterSnapshotResponse = CreateDBClusterSnapshotResponse'
    { _cdbcsrsDBClusterSnapshot :: !(Maybe DBClusterSnapshot)
    , _cdbcsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBClusterSnapshotResponse' smart constructor.
createDBClusterSnapshotResponse :: Int -> CreateDBClusterSnapshotResponse
createDBClusterSnapshotResponse pStatus_ =
    CreateDBClusterSnapshotResponse'
    { _cdbcsrsDBClusterSnapshot = Nothing
    , _cdbcsrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cdbcsrsDBClusterSnapshot :: Lens' CreateDBClusterSnapshotResponse (Maybe DBClusterSnapshot)
cdbcsrsDBClusterSnapshot = lens _cdbcsrsDBClusterSnapshot (\ s a -> s{_cdbcsrsDBClusterSnapshot = a});

-- | FIXME: Undocumented member.
cdbcsrsStatus :: Lens' CreateDBClusterSnapshotResponse Int
cdbcsrsStatus = lens _cdbcsrsStatus (\ s a -> s{_cdbcsrsStatus = a});
