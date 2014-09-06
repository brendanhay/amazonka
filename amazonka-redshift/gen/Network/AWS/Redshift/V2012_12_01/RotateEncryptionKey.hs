{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.RotateEncryptionKey
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.V2012_12_01.RotateEncryptionKey
    (
    -- * Request
      RotateEncryptionKey
    -- ** Request constructor
    , mkRotateEncryptionKey
    -- ** Request lenses
    , rekClusterIdentifier

    -- * Response
    , RotateEncryptionKeyResponse
    -- ** Response lenses
    , rekrsCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
newtype RotateEncryptionKey = RotateEncryptionKey
    { _rekClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RotateEncryptionKey' request.
mkRotateEncryptionKey :: Text -- ^ 'rekClusterIdentifier'
                      -> RotateEncryptionKey
mkRotateEncryptionKey p1 = RotateEncryptionKey
    { _rekClusterIdentifier = p1
    }
{-# INLINE mkRotateEncryptionKey #-}

-- | The unique identifier of the cluster that you want to rotate the encryption
-- keys for. Constraints: Must be the name of valid cluster that has
-- encryption enabled.
rekClusterIdentifier :: Lens' RotateEncryptionKey Text
rekClusterIdentifier =
    lens _rekClusterIdentifier (\s a -> s { _rekClusterIdentifier = a })
{-# INLINE rekClusterIdentifier #-}

instance ToQuery RotateEncryptionKey where
    toQuery = genericQuery def

newtype RotateEncryptionKeyResponse = RotateEncryptionKeyResponse
    { _rekrsCluster :: Maybe Cluster
    } deriving (Show, Generic)

-- | Describes a cluster.
rekrsCluster :: Lens' RotateEncryptionKeyResponse (Maybe Cluster)
rekrsCluster = lens _rekrsCluster (\s a -> s { _rekrsCluster = a })
{-# INLINE rekrsCluster #-}

instance FromXML RotateEncryptionKeyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RotateEncryptionKey where
    type Sv RotateEncryptionKey = Redshift
    type Rs RotateEncryptionKey = RotateEncryptionKeyResponse

    request = post "RotateEncryptionKey"
    response _ = xmlResponse
