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
    , mkRotateEncryptionKeyMessage
    -- ** Request lenses
    , rekmClusterIdentifier

    -- * Response
    , RotateEncryptionKeyResponse
    -- ** Response lenses
    , cccrCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RotateEncryptionKey' request.
mkRotateEncryptionKeyMessage :: Text -- ^ 'rekmClusterIdentifier'
                             -> RotateEncryptionKey
mkRotateEncryptionKeyMessage p1 = RotateEncryptionKey
    { _rekmClusterIdentifier = p1
    }
{-# INLINE mkRotateEncryptionKeyMessage #-}

newtype RotateEncryptionKey = RotateEncryptionKey
    { _rekmClusterIdentifier :: Text
      -- ^ The unique identifier of the cluster that you want to rotate the
      -- encryption keys for. Constraints: Must be the name of valid
      -- cluster that has encryption enabled.
    } deriving (Show, Generic)

-- | The unique identifier of the cluster that you want to rotate the encryption
-- keys for. Constraints: Must be the name of valid cluster that has
-- encryption enabled.
rekmClusterIdentifier :: Lens' RotateEncryptionKey (Text)
rekmClusterIdentifier = lens _rekmClusterIdentifier (\s a -> s { _rekmClusterIdentifier = a })
{-# INLINE rekmClusterIdentifier #-}

instance ToQuery RotateEncryptionKey where
    toQuery = genericQuery def

newtype RotateEncryptionKeyResponse = RotateEncryptionKeyResponse
    { _cccrCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Show, Generic)

-- | Describes a cluster.
cccrCluster :: Lens' RotateEncryptionKeyResponse (Maybe Cluster)
cccrCluster = lens _cccrCluster (\s a -> s { _cccrCluster = a })
{-# INLINE cccrCluster #-}

instance FromXML RotateEncryptionKeyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RotateEncryptionKey where
    type Sv RotateEncryptionKey = Redshift
    type Rs RotateEncryptionKey = RotateEncryptionKeyResponse

    request = post "RotateEncryptionKey"
    response _ = xmlResponse
