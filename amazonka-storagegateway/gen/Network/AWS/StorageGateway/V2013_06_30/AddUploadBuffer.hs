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
    , addUploadBuffer
    -- ** Request lenses
    , aubiDiskIds
    , aubiGatewayARN

    -- * Response
    , AddUploadBufferResponse
    -- ** Response lenses
    , auboGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AddUploadBuffer' request.
addUploadBuffer :: [Text] -- ^ 'aubiDiskIds'
                -> Text -- ^ 'aubiGatewayARN'
                -> AddUploadBuffer
addUploadBuffer p1 p2 = AddUploadBuffer
    { _aubiDiskIds = p1
    , _aubiGatewayARN = p2
    }

data AddUploadBuffer = AddUploadBuffer
    { _aubiDiskIds :: [Text]
    , _aubiGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

aubiDiskIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> AddUploadBuffer
    -> f AddUploadBuffer
aubiDiskIds f x =
    (\y -> x { _aubiDiskIds = y })
       <$> f (_aubiDiskIds x)
{-# INLINE aubiDiskIds #-}

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
aubiGatewayARN
    :: Functor f
    => (Text
    -> f (Text))
    -> AddUploadBuffer
    -> f AddUploadBuffer
aubiGatewayARN f x =
    (\y -> x { _aubiGatewayARN = y })
       <$> f (_aubiGatewayARN x)
{-# INLINE aubiGatewayARN #-}

instance ToPath AddUploadBuffer

instance ToQuery AddUploadBuffer

instance ToHeaders AddUploadBuffer

instance ToJSON AddUploadBuffer

data AddUploadBufferResponse = AddUploadBufferResponse
    { _auboGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
auboGatewayARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AddUploadBufferResponse
    -> f AddUploadBufferResponse
auboGatewayARN f x =
    (\y -> x { _auboGatewayARN = y })
       <$> f (_auboGatewayARN x)
{-# INLINE auboGatewayARN #-}

instance FromJSON AddUploadBufferResponse

instance AWSRequest AddUploadBuffer where
    type Sv AddUploadBuffer = StorageGateway
    type Rs AddUploadBuffer = AddUploadBufferResponse

    request = get
    response _ = jsonResponse
