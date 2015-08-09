{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB cluster. For more information on Amazon
-- Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyDBClusterSnapshot.html AWS API Reference> for CopyDBClusterSnapshot.
module Network.AWS.RDS.CopyDBClusterSnapshot
    (
    -- * Creating a Request
      CopyDBClusterSnapshot
    , copyDBClusterSnapshot
    -- * Request Lenses
    , cdbcsTags
    , cdbcsSourceDBClusterSnapshotIdentifier
    , cdbcsTargetDBClusterSnapshotIdentifier

    -- * Destructuring the Response
    , CopyDBClusterSnapshotResponse
    , copyDBClusterSnapshotResponse
    -- * Response Lenses
    , cdcsrsDBClusterSnapshot
    , cdcsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'copyDBClusterSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbcsTags'
--
-- * 'cdbcsSourceDBClusterSnapshotIdentifier'
--
-- * 'cdbcsTargetDBClusterSnapshotIdentifier'
data CopyDBClusterSnapshot = CopyDBClusterSnapshot'
    { _cdbcsTags                              :: !(Maybe [Tag])
    , _cdbcsSourceDBClusterSnapshotIdentifier :: !Text
    , _cdbcsTargetDBClusterSnapshotIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBClusterSnapshot' smart constructor.
copyDBClusterSnapshot :: Text -> Text -> CopyDBClusterSnapshot
copyDBClusterSnapshot pSourceDBClusterSnapshotIdentifier_ pTargetDBClusterSnapshotIdentifier_ =
    CopyDBClusterSnapshot'
    { _cdbcsTags = Nothing
    , _cdbcsSourceDBClusterSnapshotIdentifier = pSourceDBClusterSnapshotIdentifier_
    , _cdbcsTargetDBClusterSnapshotIdentifier = pTargetDBClusterSnapshotIdentifier_
    }

-- | Undocumented member.
cdbcsTags :: Lens' CopyDBClusterSnapshot [Tag]
cdbcsTags = lens _cdbcsTags (\ s a -> s{_cdbcsTags = a}) . _Default . _Coerce;

-- | The identifier of the DB cluster snapshot to copy. This parameter is not
-- case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster-snapshot1@
cdbcsSourceDBClusterSnapshotIdentifier :: Lens' CopyDBClusterSnapshot Text
cdbcsSourceDBClusterSnapshotIdentifier = lens _cdbcsSourceDBClusterSnapshotIdentifier (\ s a -> s{_cdbcsSourceDBClusterSnapshotIdentifier = a});

-- | The identifier of the new DB cluster snapshot to create from the source
-- DB cluster snapshot. This parameter is not case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens.
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster-snapshot2@
cdbcsTargetDBClusterSnapshotIdentifier :: Lens' CopyDBClusterSnapshot Text
cdbcsTargetDBClusterSnapshotIdentifier = lens _cdbcsTargetDBClusterSnapshotIdentifier (\ s a -> s{_cdbcsTargetDBClusterSnapshotIdentifier = a});

instance AWSRequest CopyDBClusterSnapshot where
        type Sv CopyDBClusterSnapshot = RDS
        type Rs CopyDBClusterSnapshot =
             CopyDBClusterSnapshotResponse
        request = postQuery
        response
          = receiveXMLWrapper "CopyDBClusterSnapshotResult"
              (\ s h x ->
                 CopyDBClusterSnapshotResponse' <$>
                   (x .@? "DBClusterSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders CopyDBClusterSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBClusterSnapshot where
        toPath = const "/"

instance ToQuery CopyDBClusterSnapshot where
        toQuery CopyDBClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBClusterSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbcsTags),
               "SourceDBClusterSnapshotIdentifier" =:
                 _cdbcsSourceDBClusterSnapshotIdentifier,
               "TargetDBClusterSnapshotIdentifier" =:
                 _cdbcsTargetDBClusterSnapshotIdentifier]

-- | /See:/ 'copyDBClusterSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdcsrsDBClusterSnapshot'
--
-- * 'cdcsrsStatus'
data CopyDBClusterSnapshotResponse = CopyDBClusterSnapshotResponse'
    { _cdcsrsDBClusterSnapshot :: !(Maybe DBClusterSnapshot)
    , _cdcsrsStatus            :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBClusterSnapshotResponse' smart constructor.
copyDBClusterSnapshotResponse :: Int -> CopyDBClusterSnapshotResponse
copyDBClusterSnapshotResponse pStatus_ =
    CopyDBClusterSnapshotResponse'
    { _cdcsrsDBClusterSnapshot = Nothing
    , _cdcsrsStatus = pStatus_
    }

-- | Undocumented member.
cdcsrsDBClusterSnapshot :: Lens' CopyDBClusterSnapshotResponse (Maybe DBClusterSnapshot)
cdcsrsDBClusterSnapshot = lens _cdcsrsDBClusterSnapshot (\ s a -> s{_cdcsrsDBClusterSnapshot = a});

-- | Undocumented member.
cdcsrsStatus :: Lens' CopyDBClusterSnapshotResponse Int
cdcsrsStatus = lens _cdcsrsStatus (\ s a -> s{_cdcsrsStatus = a});
