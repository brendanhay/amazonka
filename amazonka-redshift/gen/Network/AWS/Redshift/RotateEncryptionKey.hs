{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
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
    , rekrsCluster
    , rekrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'rotateEncryptionKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rekClusterIdentifier'
newtype RotateEncryptionKey = RotateEncryptionKey'
    { _rekClusterIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RotateEncryptionKey' smart constructor.
rotateEncryptionKey :: Text -> RotateEncryptionKey
rotateEncryptionKey pClusterIdentifier_ =
    RotateEncryptionKey'
    { _rekClusterIdentifier = pClusterIdentifier_
    }

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
        request = post "RotateEncryptionKey"
        response
          = receiveXMLWrapper "RotateEncryptionKeyResult"
              (\ s h x ->
                 RotateEncryptionKeyResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

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
-- * 'rekrsCluster'
--
-- * 'rekrsStatus'
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
    { _rekrsCluster :: !(Maybe Cluster)
    , _rekrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RotateEncryptionKeyResponse' smart constructor.
rotateEncryptionKeyResponse :: Int -> RotateEncryptionKeyResponse
rotateEncryptionKeyResponse pStatus_ =
    RotateEncryptionKeyResponse'
    { _rekrsCluster = Nothing
    , _rekrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rekrsCluster :: Lens' RotateEncryptionKeyResponse (Maybe Cluster)
rekrsCluster = lens _rekrsCluster (\ s a -> s{_rekrsCluster = a});

-- | FIXME: Undocumented member.
rekrsStatus :: Lens' RotateEncryptionKeyResponse Int
rekrsStatus = lens _rekrsStatus (\ s a -> s{_rekrsStatus = a});
