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

-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation configures one or more gateway local disks as cache for a
-- cached-volume gateway. This operation is supported only for the
-- gateway-cached volume architecture (see Storage Gateway Concepts). In the
-- request, you specify the gateway Amazon Resource Name (ARN) to which you
-- want to add cache, and one or more disk IDs that you want to configure as
-- cache.
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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data AddCache = AddCache
    { _acDiskIds    :: [Text]
    , _acGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic)

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
acDiskIds = lens _acDiskIds (\s a -> s { _acDiskIds = a })

acGatewayARN :: Lens' AddCache Text
acGatewayARN = lens _acGatewayARN (\s a -> s { _acGatewayARN = a })

instance ToPath AddCache where
    toPath = const "/"

instance ToQuery AddCache where
    toQuery = const mempty

instance ToHeaders AddCache

instance ToBody AddCache where
    toBody = toBody . encode . _acGatewayARN

newtype AddCacheResponse = AddCacheResponse
    { _acrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

-- FromJSON

instance AWSRequest AddCache where
    type Sv AddCache = StorageGateway
    type Rs AddCache = AddCacheResponse

    request  = post'
    response = jsonResponse $ \h o -> AddCacheResponse
        <$> o .: "GatewayARN"
