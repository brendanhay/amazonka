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
-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the automatic copy of snapshots from one region to another region for a specified cluster.
--
--
module Network.AWS.Redshift.EnableSnapshotCopy
    (
    -- * Creating a Request
      enableSnapshotCopy
    , EnableSnapshotCopy
    -- * Request Lenses
    , escRetentionPeriod
    , escSnapshotCopyGrantName
    , escClusterIdentifier
    , escDestinationRegion

    -- * Destructuring the Response
    , enableSnapshotCopyResponse
    , EnableSnapshotCopyResponse
    -- * Response Lenses
    , escrsCluster
    , escrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'enableSnapshotCopy' smart constructor.
data EnableSnapshotCopy = EnableSnapshotCopy'
  { _escRetentionPeriod       :: !(Maybe Int)
  , _escSnapshotCopyGrantName :: !(Maybe Text)
  , _escClusterIdentifier     :: !Text
  , _escDestinationRegion     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableSnapshotCopy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'escRetentionPeriod' - The number of days to retain automated snapshots in the destination region after they are copied from the source region. Default: 7. Constraints: Must be at least 1 and no more than 35.
--
-- * 'escSnapshotCopyGrantName' - The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
--
-- * 'escClusterIdentifier' - The unique identifier of the source cluster to copy snapshots from. Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
--
-- * 'escDestinationRegion' - The destination region that you want to copy snapshots to. Constraints: Must be the name of a valid region. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
enableSnapshotCopy
    :: Text -- ^ 'escClusterIdentifier'
    -> Text -- ^ 'escDestinationRegion'
    -> EnableSnapshotCopy
enableSnapshotCopy pClusterIdentifier_ pDestinationRegion_ =
  EnableSnapshotCopy'
    { _escRetentionPeriod = Nothing
    , _escSnapshotCopyGrantName = Nothing
    , _escClusterIdentifier = pClusterIdentifier_
    , _escDestinationRegion = pDestinationRegion_
    }


-- | The number of days to retain automated snapshots in the destination region after they are copied from the source region. Default: 7. Constraints: Must be at least 1 and no more than 35.
escRetentionPeriod :: Lens' EnableSnapshotCopy (Maybe Int)
escRetentionPeriod = lens _escRetentionPeriod (\ s a -> s{_escRetentionPeriod = a})

-- | The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
escSnapshotCopyGrantName :: Lens' EnableSnapshotCopy (Maybe Text)
escSnapshotCopyGrantName = lens _escSnapshotCopyGrantName (\ s a -> s{_escSnapshotCopyGrantName = a})

-- | The unique identifier of the source cluster to copy snapshots from. Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
escClusterIdentifier :: Lens' EnableSnapshotCopy Text
escClusterIdentifier = lens _escClusterIdentifier (\ s a -> s{_escClusterIdentifier = a})

-- | The destination region that you want to copy snapshots to. Constraints: Must be the name of a valid region. For more information, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
escDestinationRegion :: Lens' EnableSnapshotCopy Text
escDestinationRegion = lens _escDestinationRegion (\ s a -> s{_escDestinationRegion = a})

instance AWSRequest EnableSnapshotCopy where
        type Rs EnableSnapshotCopy =
             EnableSnapshotCopyResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "EnableSnapshotCopyResult"
              (\ s h x ->
                 EnableSnapshotCopyResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable EnableSnapshotCopy where

instance NFData EnableSnapshotCopy where

instance ToHeaders EnableSnapshotCopy where
        toHeaders = const mempty

instance ToPath EnableSnapshotCopy where
        toPath = const "/"

instance ToQuery EnableSnapshotCopy where
        toQuery EnableSnapshotCopy'{..}
          = mconcat
              ["Action" =: ("EnableSnapshotCopy" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "RetentionPeriod" =: _escRetentionPeriod,
               "SnapshotCopyGrantName" =: _escSnapshotCopyGrantName,
               "ClusterIdentifier" =: _escClusterIdentifier,
               "DestinationRegion" =: _escDestinationRegion]

-- | /See:/ 'enableSnapshotCopyResponse' smart constructor.
data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse'
  { _escrsCluster        :: !(Maybe Cluster)
  , _escrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableSnapshotCopyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'escrsCluster' - Undocumented member.
--
-- * 'escrsResponseStatus' - -- | The response status code.
enableSnapshotCopyResponse
    :: Int -- ^ 'escrsResponseStatus'
    -> EnableSnapshotCopyResponse
enableSnapshotCopyResponse pResponseStatus_ =
  EnableSnapshotCopyResponse'
    {_escrsCluster = Nothing, _escrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
escrsCluster :: Lens' EnableSnapshotCopyResponse (Maybe Cluster)
escrsCluster = lens _escrsCluster (\ s a -> s{_escrsCluster = a})

-- | -- | The response status code.
escrsResponseStatus :: Lens' EnableSnapshotCopyResponse Int
escrsResponseStatus = lens _escrsResponseStatus (\ s a -> s{_escrsResponseStatus = a})

instance NFData EnableSnapshotCopyResponse where
