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

-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation configures one or more gateway local disks as upload buffer
-- for a specified gateway. This operation is supported for both the
-- gateway-stored and gateway-cached volume architectures. In the request, you
-- specify the gateway Amazon Resource Name (ARN) to which you want to add
-- upload buffer, and one or more disk IDs that you want to configure as
-- upload buffer.
module Network.AWS.StorageGateway.AddUploadBuffer
    (
    -- * Request
      AddUploadBuffer
    -- ** Request constructor
    , addUploadBuffer
    -- ** Request lenses
    , aubDiskIds
    , aubGatewayARN

    -- * Response
    , AddUploadBufferResponse
    -- ** Response constructor
    , addUploadBufferResponse
    -- ** Response lenses
    , aubrGatewayARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

data AddUploadBuffer = AddUploadBuffer
    { _aubDiskIds    :: [Text]
    , _aubGatewayARN :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddUploadBuffer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aubDiskIds' @::@ ['Text']
--
-- * 'aubGatewayARN' @::@ 'Text'
--
addUploadBuffer :: Text -- ^ 'aubGatewayARN'
                -> AddUploadBuffer
addUploadBuffer p1 = AddUploadBuffer
    { _aubGatewayARN = p1
    , _aubDiskIds    = mempty
    }

aubDiskIds :: Lens' AddUploadBuffer [Text]
aubDiskIds = lens _aubDiskIds (\s a -> s { _aubDiskIds = a })

aubGatewayARN :: Lens' AddUploadBuffer Text
aubGatewayARN = lens _aubGatewayARN (\s a -> s { _aubGatewayARN = a })

instance ToPath AddUploadBuffer where
    toPath = const "/"

instance ToQuery AddUploadBuffer where
    toQuery = const mempty

instance ToHeaders AddUploadBuffer

instance ToBody AddUploadBuffer where
    toBody = toBody . encode . _aubGatewayARN

newtype AddUploadBufferResponse = AddUploadBufferResponse
    { _aubrGatewayARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AddUploadBufferResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aubrGatewayARN' @::@ 'Maybe' 'Text'
--
addUploadBufferResponse :: AddUploadBufferResponse
addUploadBufferResponse = AddUploadBufferResponse
    { _aubrGatewayARN = Nothing
    }

aubrGatewayARN :: Lens' AddUploadBufferResponse (Maybe Text)
aubrGatewayARN = lens _aubrGatewayARN (\s a -> s { _aubrGatewayARN = a })

-- FromJSON

instance AWSRequest AddUploadBuffer where
    type Sv AddUploadBuffer = StorageGateway
    type Rs AddUploadBuffer = AddUploadBufferResponse

    request  = post'
    response = jsonResponse $ \h o -> AddUploadBufferResponse
        <$> o .: "GatewayARN"
