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
-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.RotateEncryptionKey
    (
    -- * Creating a Request
      rotateEncryptionKey
    , RotateEncryptionKey
    -- * Request Lenses
    , rekClusterIdentifier

    -- * Destructuring the Response
    , rotateEncryptionKeyResponse
    , RotateEncryptionKeyResponse
    -- * Response Lenses
    , rekrsCluster
    , rekrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'rotateEncryptionKey' smart constructor.
newtype RotateEncryptionKey = RotateEncryptionKey'
    { _rekClusterIdentifier :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RotateEncryptionKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rekClusterIdentifier'
rotateEncryptionKey
    :: Text -- ^ 'rekClusterIdentifier'
    -> RotateEncryptionKey
rotateEncryptionKey pClusterIdentifier_ =
    RotateEncryptionKey'
    { _rekClusterIdentifier = pClusterIdentifier_
    }

-- | The unique identifier of the cluster that you want to rotate the encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption enabled.
rekClusterIdentifier :: Lens' RotateEncryptionKey Text
rekClusterIdentifier = lens _rekClusterIdentifier (\ s a -> s{_rekClusterIdentifier = a});

instance AWSRequest RotateEncryptionKey where
        type Rs RotateEncryptionKey =
             RotateEncryptionKeyResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "RotateEncryptionKeyResult"
              (\ s h x ->
                 RotateEncryptionKeyResponse' <$>
                   (x .@? "Cluster") <*> (pure (fromEnum s)))

instance Hashable RotateEncryptionKey

instance NFData RotateEncryptionKey

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
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
    { _rekrsCluster        :: !(Maybe Cluster)
    , _rekrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RotateEncryptionKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rekrsCluster'
--
-- * 'rekrsResponseStatus'
rotateEncryptionKeyResponse
    :: Int -- ^ 'rekrsResponseStatus'
    -> RotateEncryptionKeyResponse
rotateEncryptionKeyResponse pResponseStatus_ =
    RotateEncryptionKeyResponse'
    { _rekrsCluster = Nothing
    , _rekrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rekrsCluster :: Lens' RotateEncryptionKeyResponse (Maybe Cluster)
rekrsCluster = lens _rekrsCluster (\ s a -> s{_rekrsCluster = a});

-- | The response status code.
rekrsResponseStatus :: Lens' RotateEncryptionKeyResponse Int
rekrsResponseStatus = lens _rekrsResponseStatus (\ s a -> s{_rekrsResponseStatus = a});

instance NFData RotateEncryptionKeyResponse
