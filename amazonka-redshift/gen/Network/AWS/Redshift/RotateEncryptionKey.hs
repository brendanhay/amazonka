{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Rotates the encryption keys for a cluster.
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

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
newtype RotateEncryptionKey = RotateEncryptionKey
    { _rekClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RotateEncryptionKey' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Text@
--
rotateEncryptionKey :: Text -- ^ 'rekClusterIdentifier'
                    -> RotateEncryptionKey
rotateEncryptionKey p1 = RotateEncryptionKey
    { _rekClusterIdentifier = p1
    }

-- | The unique identifier of the cluster that you want to rotate the encryption
-- keys for. Constraints: Must be the name of valid cluster that has
-- encryption enabled.
rekClusterIdentifier :: Lens' RotateEncryptionKey Text
rekClusterIdentifier =
    lens _rekClusterIdentifier (\s a -> s { _rekClusterIdentifier = a })

instance ToQuery RotateEncryptionKey where
    toQuery = genericQuery def

newtype RotateEncryptionKeyResponse = RotateEncryptionKeyResponse
    { _rekrCluster :: Maybe Cluster
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RotateEncryptionKeyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Cluster ::@ @Maybe Cluster@
--
rotateEncryptionKeyResponse :: RotateEncryptionKeyResponse
rotateEncryptionKeyResponse = RotateEncryptionKeyResponse
    { _rekrCluster = Nothing
    }

-- | Describes a cluster.
rekrCluster :: Lens' RotateEncryptionKeyResponse (Maybe Cluster)
rekrCluster = lens _rekrCluster (\s a -> s { _rekrCluster = a })

instance FromXML RotateEncryptionKeyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RotateEncryptionKey where
    type Sv RotateEncryptionKey = Redshift
    type Rs RotateEncryptionKey = RotateEncryptionKeyResponse

    request = post "RotateEncryptionKey"
    response _ = xmlResponse
