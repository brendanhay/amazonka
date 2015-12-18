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
-- Module      : Network.AWS.Redshift.CreateClusterSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a manual snapshot of the specified cluster. The cluster must be
-- in the 'available' state.
--
-- For more information about working with snapshots, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSnapshot.html AWS API Reference> for CreateClusterSnapshot.
module Network.AWS.Redshift.CreateClusterSnapshot
    (
    -- * Creating a Request
      createClusterSnapshot
    , CreateClusterSnapshot
    -- * Request Lenses
    , ccsTags
    , ccsSnapshotIdentifier
    , ccsClusterIdentifier

    -- * Destructuring the Response
    , createClusterSnapshotResponse
    , CreateClusterSnapshotResponse
    -- * Response Lenses
    , crersSnapshot
    , crersResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createClusterSnapshot' smart constructor.
data CreateClusterSnapshot = CreateClusterSnapshot'
    { _ccsTags               :: !(Maybe [Tag])
    , _ccsSnapshotIdentifier :: !Text
    , _ccsClusterIdentifier  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateClusterSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsTags'
--
-- * 'ccsSnapshotIdentifier'
--
-- * 'ccsClusterIdentifier'
createClusterSnapshot
    :: Text -- ^ 'ccsSnapshotIdentifier'
    -> Text -- ^ 'ccsClusterIdentifier'
    -> CreateClusterSnapshot
createClusterSnapshot pSnapshotIdentifier_ pClusterIdentifier_ =
    CreateClusterSnapshot'
    { _ccsTags = Nothing
    , _ccsSnapshotIdentifier = pSnapshotIdentifier_
    , _ccsClusterIdentifier = pClusterIdentifier_
    }

-- | A list of tag instances.
ccsTags :: Lens' CreateClusterSnapshot [Tag]
ccsTags = lens _ccsTags (\ s a -> s{_ccsTags = a}) . _Default . _Coerce;

-- | A unique identifier for the snapshot that you are requesting. This
-- identifier must be unique for all snapshots within the AWS account.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: 'my-snapshot-id'
ccsSnapshotIdentifier :: Lens' CreateClusterSnapshot Text
ccsSnapshotIdentifier = lens _ccsSnapshotIdentifier (\ s a -> s{_ccsSnapshotIdentifier = a});

-- | The cluster identifier for which you want a snapshot.
ccsClusterIdentifier :: Lens' CreateClusterSnapshot Text
ccsClusterIdentifier = lens _ccsClusterIdentifier (\ s a -> s{_ccsClusterIdentifier = a});

instance AWSRequest CreateClusterSnapshot where
        type Rs CreateClusterSnapshot =
             CreateClusterSnapshotResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "CreateClusterSnapshotResult"
              (\ s h x ->
                 CreateClusterSnapshotResponse' <$>
                   (x .@? "Snapshot") <*> (pure (fromEnum s)))

instance ToHeaders CreateClusterSnapshot where
        toHeaders = const mempty

instance ToPath CreateClusterSnapshot where
        toPath = const "/"

instance ToQuery CreateClusterSnapshot where
        toQuery CreateClusterSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateClusterSnapshot" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccsTags),
               "SnapshotIdentifier" =: _ccsSnapshotIdentifier,
               "ClusterIdentifier" =: _ccsClusterIdentifier]

-- | /See:/ 'createClusterSnapshotResponse' smart constructor.
data CreateClusterSnapshotResponse = CreateClusterSnapshotResponse'
    { _crersSnapshot       :: !(Maybe Snapshot)
    , _crersResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crersSnapshot'
--
-- * 'crersResponseStatus'
createClusterSnapshotResponse
    :: Int -- ^ 'crersResponseStatus'
    -> CreateClusterSnapshotResponse
createClusterSnapshotResponse pResponseStatus_ =
    CreateClusterSnapshotResponse'
    { _crersSnapshot = Nothing
    , _crersResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
crersSnapshot :: Lens' CreateClusterSnapshotResponse (Maybe Snapshot)
crersSnapshot = lens _crersSnapshot (\ s a -> s{_crersSnapshot = a});

-- | The response status code.
crersResponseStatus :: Lens' CreateClusterSnapshotResponse Int
crersResponseStatus = lens _crersResponseStatus (\ s a -> s{_crersResponseStatus = a});
