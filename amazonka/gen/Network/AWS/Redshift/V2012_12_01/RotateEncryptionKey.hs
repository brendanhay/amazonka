{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Redshift.V2012_12_01.RotateEncryptionKey where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data RotateEncryptionKey = RotateEncryptionKey
    { _rekmClusterIdentifier :: Text
      -- ^ The unique identifier of the cluster that you want to rotate the
      -- encryption keys for. Constraints: Must be the name of valid
      -- cluster that has encryption enabled.
    } deriving (Generic)

makeLenses ''RotateEncryptionKey

instance ToQuery RotateEncryptionKey where
    toQuery = genericToQuery def

data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse
    { _cczCluster :: Maybe Cluster
      -- ^ Describes a cluster.
    } deriving (Generic)

makeLenses ''RotateEncryptionKeyResponse

instance FromXML RotateEncryptionKeyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RotateEncryptionKey where
    type Sv RotateEncryptionKey = Redshift
    type Rs RotateEncryptionKey = RotateEncryptionKeyResponse

    request = post "RotateEncryptionKey"
    response _ = xmlResponse
