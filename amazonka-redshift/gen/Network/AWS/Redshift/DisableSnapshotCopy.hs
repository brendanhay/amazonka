{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Disables the automatic copying of snapshots from one region to another
-- region for a specified cluster.
--
-- If your cluster and its snapshots are encrypted using a customer master
-- key (CMK) from AWS KMS, use DeleteSnapshotCopyGrant to delete the grant
-- that grants Amazon Redshift permission to the CMK in the destination
-- region.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DisableSnapshotCopy.html>
module Network.AWS.Redshift.DisableSnapshotCopy
    (
    -- * Request
      DisableSnapshotCopy
    -- ** Request constructor
    , disableSnapshotCopy
    -- ** Request lenses
    , dscClusterIdentifier

    -- * Response
    , DisableSnapshotCopyResponse
    -- ** Response constructor
    , disableSnapshotCopyResponse
    -- ** Response lenses
    , dscrsCluster
    , dscrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'disableSnapshotCopy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscClusterIdentifier'
newtype DisableSnapshotCopy = DisableSnapshotCopy'
    { _dscClusterIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableSnapshotCopy' smart constructor.
disableSnapshotCopy :: Text -> DisableSnapshotCopy
disableSnapshotCopy pClusterIdentifier_ =
    DisableSnapshotCopy'
    { _dscClusterIdentifier = pClusterIdentifier_
    }

-- | The unique identifier of the source cluster that you want to disable
-- copying of snapshots to a destination region.
--
-- Constraints: Must be the valid name of an existing cluster that has
-- cross-region snapshot copy enabled.
dscClusterIdentifier :: Lens' DisableSnapshotCopy Text
dscClusterIdentifier = lens _dscClusterIdentifier (\ s a -> s{_dscClusterIdentifier = a});

instance AWSRequest DisableSnapshotCopy where
        type Sv DisableSnapshotCopy = Redshift
        type Rs DisableSnapshotCopy =
             DisableSnapshotCopyResponse
        request = post
        response
          = receiveXMLWrapper "DisableSnapshotCopyResult"
              (\ s h x ->
                 DisableSnapshotCopyResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders DisableSnapshotCopy where
        toHeaders = const mempty

instance ToPath DisableSnapshotCopy where
        toPath = const "/"

instance ToQuery DisableSnapshotCopy where
        toQuery DisableSnapshotCopy'{..}
          = mconcat
              ["Action" =: ("DisableSnapshotCopy" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _dscClusterIdentifier]

-- | /See:/ 'disableSnapshotCopyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dscrsCluster'
--
-- * 'dscrsStatus'
data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'
    { _dscrsCluster :: !(Maybe Cluster)
    , _dscrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableSnapshotCopyResponse' smart constructor.
disableSnapshotCopyResponse :: Int -> DisableSnapshotCopyResponse
disableSnapshotCopyResponse pStatus_ =
    DisableSnapshotCopyResponse'
    { _dscrsCluster = Nothing
    , _dscrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
dscrsCluster :: Lens' DisableSnapshotCopyResponse (Maybe Cluster)
dscrsCluster = lens _dscrsCluster (\ s a -> s{_dscrsCluster = a});

-- | FIXME: Undocumented member.
dscrsStatus :: Lens' DisableSnapshotCopyResponse Int
dscrsStatus = lens _dscrsStatus (\ s a -> s{_dscrsStatus = a});
