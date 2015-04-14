{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation configures one or more gateway local disks as cache for a
-- cached-volume gateway. This operation is supported only for the
-- gateway-cached volume architecture (see <http://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html Storage Gateway Concepts>).
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to which
-- you want to add cache, and one or more disk IDs that you want to configure as
-- cache.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_AddCache.html>
module Network.AWS.StorageGateway.AddCache
    (
    -- * Request
      AddCache
    -- ** Request constructor
    , addCache
    -- ** Request lenses
    , acDiskIds
    , acGatewayARN

    -- * Response
    , AddCacheResponse
    -- ** Response constructor
    , addCacheResponse
    -- ** Response lenses
    , acrGatewayARN
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

data AddCache = AddCache
    { _acDiskIds    :: List "DiskIds" Text
    , _acGatewayARN :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AddCache' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acDiskIds' @::@ ['Text']
--
-- * 'acGatewayARN' @::@ 'Text'
--
addCache :: Text -- ^ 'acGatewayARN'
         -> AddCache
addCache p1 = AddCache
    { _acGatewayARN = p1
    , _acDiskIds    = mempty
    }

acDiskIds :: Lens' AddCache [Text]
acDiskIds = lens _acDiskIds (\s a -> s { _acDiskIds = a }) . _List

acGatewayARN :: Lens' AddCache Text
acGatewayARN = lens _acGatewayARN (\s a -> s { _acGatewayARN = a })

newtype AddCacheResponse = AddCacheResponse
    { _acrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'AddCacheResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'acrGatewayARN' @::@ 'Maybe' 'Text'
--
addCacheResponse :: AddCacheResponse
addCacheResponse = AddCacheResponse
    { _acrGatewayARN = Nothing
    }

acrGatewayARN :: Lens' AddCacheResponse (Maybe Text)
acrGatewayARN = lens _acrGatewayARN (\s a -> s { _acrGatewayARN = a })

instance ToPath AddCache where
    toPath = const "/"

instance ToQuery AddCache where
    toQuery = const mempty

instance ToHeaders AddCache

instance ToJSON AddCache where
    toJSON AddCache{..} = object
        [ "GatewayARN" .= _acGatewayARN
        , "DiskIds"    .= _acDiskIds
        ]

instance AWSRequest AddCache where
    type Sv AddCache = StorageGateway
    type Rs AddCache = AddCacheResponse

    request  = post "AddCache"
    response = jsonResponse

instance FromJSON AddCacheResponse where
    parseJSON = withObject "AddCacheResponse" $ \o -> AddCacheResponse
        <$> o .:? "GatewayARN"
