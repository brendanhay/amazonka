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
-- Module      : Network.AWS.Redshift.DisableSnapshotCopy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the automatic copying of snapshots from one region to another region for a specified cluster.
--
--
-- If your cluster and its snapshots are encrypted using a customer master key (CMK) from AWS KMS, use 'DeleteSnapshotCopyGrant' to delete the grant that grants Amazon Redshift permission to the CMK in the destination region.
--
module Network.AWS.Redshift.DisableSnapshotCopy
    (
    -- * Creating a Request
      disableSnapshotCopy
    , DisableSnapshotCopy
    -- * Request Lenses
    , dscClusterIdentifier

    -- * Destructuring the Response
    , disableSnapshotCopyResponse
    , DisableSnapshotCopyResponse
    -- * Response Lenses
    , dscrsCluster
    , dscrsResponseStatus
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
-- /See:/ 'disableSnapshotCopy' smart constructor.
newtype DisableSnapshotCopy = DisableSnapshotCopy'
  { _dscClusterIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableSnapshotCopy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscClusterIdentifier' - The unique identifier of the source cluster that you want to disable copying of snapshots to a destination region. Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
disableSnapshotCopy
    :: Text -- ^ 'dscClusterIdentifier'
    -> DisableSnapshotCopy
disableSnapshotCopy pClusterIdentifier_ =
  DisableSnapshotCopy' {_dscClusterIdentifier = pClusterIdentifier_}


-- | The unique identifier of the source cluster that you want to disable copying of snapshots to a destination region. Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
dscClusterIdentifier :: Lens' DisableSnapshotCopy Text
dscClusterIdentifier = lens _dscClusterIdentifier (\ s a -> s{_dscClusterIdentifier = a})

instance AWSRequest DisableSnapshotCopy where
        type Rs DisableSnapshotCopy =
             DisableSnapshotCopyResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DisableSnapshotCopyResult"
              (\ s h x ->
                 DisableSnapshotCopyResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable DisableSnapshotCopy where

instance NFData DisableSnapshotCopy where

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
data DisableSnapshotCopyResponse = DisableSnapshotCopyResponse'
  { _dscrsCluster        :: !(Maybe Cluster)
  , _dscrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableSnapshotCopyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsCluster' - Undocumented member.
--
-- * 'dscrsResponseStatus' - -- | The response status code.
disableSnapshotCopyResponse
    :: Int -- ^ 'dscrsResponseStatus'
    -> DisableSnapshotCopyResponse
disableSnapshotCopyResponse pResponseStatus_ =
  DisableSnapshotCopyResponse'
    {_dscrsCluster = Nothing, _dscrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
dscrsCluster :: Lens' DisableSnapshotCopyResponse (Maybe Cluster)
dscrsCluster = lens _dscrsCluster (\ s a -> s{_dscrsCluster = a})

-- | -- | The response status code.
dscrsResponseStatus :: Lens' DisableSnapshotCopyResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\ s a -> s{_dscrsResponseStatus = a})

instance NFData DisableSnapshotCopyResponse where
