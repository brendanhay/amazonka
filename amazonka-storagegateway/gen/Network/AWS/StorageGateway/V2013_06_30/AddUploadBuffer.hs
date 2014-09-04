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
    , mkAddUploadBufferInput
    -- ** Request lenses
    , aubiGatewayARN
    , aubiDiskIds

    -- * Response
    , AddUploadBufferResponse
    -- ** Response lenses
    , auboGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddUploadBuffer' request.
mkAddUploadBufferInput :: Text -- ^ 'aubiGatewayARN'
                       -> [Text] -- ^ 'aubiDiskIds'
                       -> AddUploadBuffer
mkAddUploadBufferInput p1 p2 = AddUploadBuffer
    { _aubiGatewayARN = p1
    , _aubiDiskIds = p2
    }
{-# INLINE mkAddUploadBufferInput #-}

data AddUploadBuffer = AddUploadBuffer
    { _aubiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _aubiDiskIds :: [Text]
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
aubiGatewayARN :: Lens' AddUploadBuffer (Text)
aubiGatewayARN = lens _aubiGatewayARN (\s a -> s { _aubiGatewayARN = a })
{-# INLINE aubiGatewayARN #-}

aubiDiskIds :: Lens' AddUploadBuffer ([Text])
aubiDiskIds = lens _aubiDiskIds (\s a -> s { _aubiDiskIds = a })
{-# INLINE aubiDiskIds #-}

instance ToPath AddUploadBuffer

instance ToQuery AddUploadBuffer

instance ToHeaders AddUploadBuffer

instance ToJSON AddUploadBuffer

newtype AddUploadBufferResponse = AddUploadBufferResponse
    { _auboGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
auboGatewayARN :: Lens' AddUploadBufferResponse (Maybe Text)
auboGatewayARN = lens _auboGatewayARN (\s a -> s { _auboGatewayARN = a })
{-# INLINE auboGatewayARN #-}

instance FromJSON AddUploadBufferResponse

instance AWSRequest AddUploadBuffer where
    type Sv AddUploadBuffer = StorageGateway
    type Rs AddUploadBuffer = AddUploadBufferResponse

    request = get
    response _ = jsonResponse
