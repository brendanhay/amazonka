{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables the automatic copy of snapshots from one region to another
-- region for a specified cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_EnableSnapshotCopy.html>
module Network.AWS.Redshift.EnableSnapshotCopy
    (
    -- * Request
      EnableSnapshotCopy
    -- ** Request constructor
    , enableSnapshotCopy
    -- ** Request lenses
    , escrqRetentionPeriod
    , escrqSnapshotCopyGrantName
    , escrqClusterIdentifier
    , escrqDestinationRegion

    -- * Response
    , EnableSnapshotCopyResponse
    -- ** Response constructor
    , enableSnapshotCopyResponse
    -- ** Response lenses
    , escrsCluster
    , escrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'enableSnapshotCopy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'escrqRetentionPeriod'
--
-- * 'escrqSnapshotCopyGrantName'
--
-- * 'escrqClusterIdentifier'
--
-- * 'escrqDestinationRegion'
data EnableSnapshotCopy = EnableSnapshotCopy'
    { _escrqRetentionPeriod       :: !(Maybe Int)
    , _escrqSnapshotCopyGrantName :: !(Maybe Text)
    , _escrqClusterIdentifier     :: !Text
    , _escrqDestinationRegion     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableSnapshotCopy' smart constructor.
enableSnapshotCopy :: Text -> Text -> EnableSnapshotCopy
enableSnapshotCopy pClusterIdentifier_ pDestinationRegion_ =
    EnableSnapshotCopy'
    { _escrqRetentionPeriod = Nothing
    , _escrqSnapshotCopyGrantName = Nothing
    , _escrqClusterIdentifier = pClusterIdentifier_
    , _escrqDestinationRegion = pDestinationRegion_
    }

-- | The number of days to retain automated snapshots in the destination
-- region after they are copied from the source region.
--
-- Default: 7.
--
-- Constraints: Must be at least 1 and no more than 35.
escrqRetentionPeriod :: Lens' EnableSnapshotCopy (Maybe Int)
escrqRetentionPeriod = lens _escrqRetentionPeriod (\ s a -> s{_escrqRetentionPeriod = a});

-- | The name of the snapshot copy grant to use when snapshots of an AWS
-- KMS-encrypted cluster are copied to the destination region.
escrqSnapshotCopyGrantName :: Lens' EnableSnapshotCopy (Maybe Text)
escrqSnapshotCopyGrantName = lens _escrqSnapshotCopyGrantName (\ s a -> s{_escrqSnapshotCopyGrantName = a});

-- | The unique identifier of the source cluster to copy snapshots from.
--
-- Constraints: Must be the valid name of an existing cluster that does not
-- already have cross-region snapshot copy enabled.
escrqClusterIdentifier :: Lens' EnableSnapshotCopy Text
escrqClusterIdentifier = lens _escrqClusterIdentifier (\ s a -> s{_escrqClusterIdentifier = a});

-- | The destination region that you want to copy snapshots to.
--
-- Constraints: Must be the name of a valid region. For more information,
-- see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints>
-- in the Amazon Web Services General Reference.
escrqDestinationRegion :: Lens' EnableSnapshotCopy Text
escrqDestinationRegion = lens _escrqDestinationRegion (\ s a -> s{_escrqDestinationRegion = a});

instance AWSRequest EnableSnapshotCopy where
        type Sv EnableSnapshotCopy = Redshift
        type Rs EnableSnapshotCopy =
             EnableSnapshotCopyResponse
        request = post
        response
          = receiveXMLWrapper "EnableSnapshotCopyResult"
              (\ s h x ->
                 EnableSnapshotCopyResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance ToHeaders EnableSnapshotCopy where
        toHeaders = const mempty

instance ToPath EnableSnapshotCopy where
        toPath = const "/"

instance ToQuery EnableSnapshotCopy where
        toQuery EnableSnapshotCopy'{..}
          = mconcat
              ["Action" =: ("EnableSnapshotCopy" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "RetentionPeriod" =: _escrqRetentionPeriod,
               "SnapshotCopyGrantName" =:
                 _escrqSnapshotCopyGrantName,
               "ClusterIdentifier" =: _escrqClusterIdentifier,
               "DestinationRegion" =: _escrqDestinationRegion]

-- | /See:/ 'enableSnapshotCopyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'escrsCluster'
--
-- * 'escrsStatus'
data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse'
    { _escrsCluster :: !(Maybe Cluster)
    , _escrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableSnapshotCopyResponse' smart constructor.
enableSnapshotCopyResponse :: Int -> EnableSnapshotCopyResponse
enableSnapshotCopyResponse pStatus_ =
    EnableSnapshotCopyResponse'
    { _escrsCluster = Nothing
    , _escrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
escrsCluster :: Lens' EnableSnapshotCopyResponse (Maybe Cluster)
escrsCluster = lens _escrsCluster (\ s a -> s{_escrsCluster = a});

-- | FIXME: Undocumented member.
escrsStatus :: Lens' EnableSnapshotCopyResponse Int
escrsStatus = lens _escrsStatus (\ s a -> s{_escrsStatus = a});
