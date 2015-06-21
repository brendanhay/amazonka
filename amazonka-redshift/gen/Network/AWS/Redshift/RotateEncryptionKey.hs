{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Rotates the encryption keys for a cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_RotateEncryptionKey.html>
module Network.AWS.Redshift.RotateEncryptionKey
    (
    -- * Request
      RotateEncryptionKey
    -- ** Request constructor
    , rotateEncryptionKey
    -- ** Request lenses
    , rekClusterIdentifier

    -- * Response
    , RotateEncryptionKeyResponse
    -- ** Response constructor
    , rotateEncryptionKeyResponse
    -- ** Response lenses
    , rekrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rotateEncryptionKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rekClusterIdentifier'
newtype RotateEncryptionKey = RotateEncryptionKey'{_rekClusterIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'RotateEncryptionKey' smart constructor.
rotateEncryptionKey :: Text -> RotateEncryptionKey
rotateEncryptionKey pClusterIdentifier = RotateEncryptionKey'{_rekClusterIdentifier = pClusterIdentifier};

-- | The unique identifier of the cluster that you want to rotate the
-- encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption
-- enabled.
rekClusterIdentifier :: Lens' RotateEncryptionKey Text
rekClusterIdentifier = lens _rekClusterIdentifier (\ s a -> s{_rekClusterIdentifier = a});

instance AWSRequest RotateEncryptionKey where
        type Sv RotateEncryptionKey = Redshift
        type Rs RotateEncryptionKey =
             RotateEncryptionKeyResponse
        request = post
        response
          = receiveXMLWrapper "RotateEncryptionKeyResult"
              (\ s h x ->
                 RotateEncryptionKeyResponse' <$> (x .@? "Cluster"))

instance ToHeaders RotateEncryptionKey where
        toHeaders = const mempty

instance ToPath RotateEncryptionKey where
        toPath = const "/"

instance ToQuery RotateEncryptionKey where
        toQuery RotateEncryptionKey'{..}
          = mconcat
              ["Action" =: ("RotateEncryptionKey" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterIdentifier" =: _rekClusterIdentifier]

-- | /See:/ 'rotateEncryptionKeyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rekrCluster'
newtype RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'{_rekrCluster :: Maybe Cluster} deriving (Eq, Read, Show)

-- | 'RotateEncryptionKeyResponse' smart constructor.
rotateEncryptionKeyResponse :: RotateEncryptionKeyResponse
rotateEncryptionKeyResponse = RotateEncryptionKeyResponse'{_rekrCluster = Nothing};

-- | FIXME: Undocumented member.
rekrCluster :: Lens' RotateEncryptionKeyResponse (Maybe Cluster)
rekrCluster = lens _rekrCluster (\ s a -> s{_rekrCluster = a});
