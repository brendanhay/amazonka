{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.AddUploadBuffer
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
module Network.AWS.StorageGateway.V2013_06_30.AddUploadBuffer
    (
    -- * Request
      AddUploadBuffer
    -- ** Request constructor
    , mkAddUploadBuffer
    -- ** Request lenses
    , aubGatewayARN
    , aubDiskIds

    -- * Response
    , AddUploadBufferResponse
    -- ** Response lenses
    , aubrsGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data AddUploadBuffer = AddUploadBuffer
    { _aubGatewayARN :: Text
    , _aubDiskIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddUploadBuffer' request.
mkAddUploadBuffer :: Text -- ^ 'aubGatewayARN'
                  -> [Text] -- ^ 'aubDiskIds'
                  -> AddUploadBuffer
mkAddUploadBuffer p1 p2 = AddUploadBuffer
    { _aubGatewayARN = p1
    , _aubDiskIds = p2
    }
{-# INLINE mkAddUploadBuffer #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
aubGatewayARN :: Lens' AddUploadBuffer Text
aubGatewayARN = lens _aubGatewayARN (\s a -> s { _aubGatewayARN = a })
{-# INLINE aubGatewayARN #-}

aubDiskIds :: Lens' AddUploadBuffer [Text]
aubDiskIds = lens _aubDiskIds (\s a -> s { _aubDiskIds = a })
{-# INLINE aubDiskIds #-}

instance ToPath AddUploadBuffer

instance ToQuery AddUploadBuffer

instance ToHeaders AddUploadBuffer

instance ToJSON AddUploadBuffer

newtype AddUploadBufferResponse = AddUploadBufferResponse
    { _aubrsGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
aubrsGatewayARN :: Lens' AddUploadBufferResponse (Maybe Text)
aubrsGatewayARN = lens _aubrsGatewayARN (\s a -> s { _aubrsGatewayARN = a })
{-# INLINE aubrsGatewayARN #-}

instance FromJSON AddUploadBufferResponse

instance AWSRequest AddUploadBuffer where
    type Sv AddUploadBuffer = StorageGateway
    type Rs AddUploadBuffer = AddUploadBufferResponse

    request = get
    response _ = jsonResponse
