{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
      RotateEncryptionKeyMessage
    -- ** Request constructor
    , rotateEncryptionKey
    -- ** Request lenses
    , rekmClusterIdentifier

    -- * Response
    , RotateEncryptionKeyResult
    -- ** Response constructor
    , rotateEncryptionKeyResponse
    -- ** Response lenses
    , rekrCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype RotateEncryptionKeyMessage = RotateEncryptionKeyMessage
    { _rekmClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RotateEncryptionKeyMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rekmClusterIdentifier' @::@ 'Text'
--
rotateEncryptionKey :: Text -- ^ 'rekmClusterIdentifier'
                    -> RotateEncryptionKeyMessage
rotateEncryptionKey p1 = RotateEncryptionKeyMessage
    { _rekmClusterIdentifier = p1
    }

-- | The unique identifier of the cluster that you want to rotate the
-- encryption keys for. Constraints: Must be the name of valid cluster that
-- has encryption enabled.
rekmClusterIdentifier :: Lens' RotateEncryptionKeyMessage Text
rekmClusterIdentifier =
    lens _rekmClusterIdentifier (\s a -> s { _rekmClusterIdentifier = a })

instance ToPath RotateEncryptionKeyMessage where
    toPath = const "/"

instance ToQuery RotateEncryptionKeyMessage

newtype RotateEncryptionKeyResult = RotateEncryptionKeyResult
    { _rekrCluster :: Maybe Cluster
    } deriving (Eq, Show, Generic)

-- | 'RotateEncryptionKeyResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rekrCluster' @::@ 'Maybe' 'Cluster'
--
rotateEncryptionKeyResponse :: RotateEncryptionKeyResult
rotateEncryptionKeyResponse = RotateEncryptionKeyResult
    { _rekrCluster = Nothing
    }

rekrCluster :: Lens' RotateEncryptionKeyResult (Maybe Cluster)
rekrCluster = lens _rekrCluster (\s a -> s { _rekrCluster = a })

instance AWSRequest RotateEncryptionKeyMessage where
    type Sv RotateEncryptionKeyMessage = Redshift
    type Rs RotateEncryptionKeyMessage = RotateEncryptionKeyResult

    request  = post "RotateEncryptionKey"
    response = xmlResponse $ \h x -> RotateEncryptionKeyResult
        <$> x %| "Cluster"
