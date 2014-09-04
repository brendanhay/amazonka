{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.AddCache
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
-- cache. Example Request The following example shows a request that activates
-- a gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- Content-Type: application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120425T120000Z x-amz-target: StorageGateway_20120630.AddCache
-- { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:03:00.0-scsi-0:0:1:0" ] } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 85 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }.
module Network.AWS.StorageGateway.V2013_06_30.AddCache
    (
    -- * Request
      AddCache
    -- ** Request constructor
    , mkAddCacheInput
    -- ** Request lenses
    , aciGatewayARN
    , aciDiskIds

    -- * Response
    , AddCacheResponse
    -- ** Response lenses
    , acoGatewayARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddCache' request.
mkAddCacheInput :: Text -- ^ 'aciGatewayARN'
                -> [Text] -- ^ 'aciDiskIds'
                -> AddCache
mkAddCacheInput p1 p2 = AddCache
    { _aciGatewayARN = p1
    , _aciDiskIds = p2
    }
{-# INLINE mkAddCacheInput #-}

data AddCache = AddCache
    { _aciGatewayARN :: Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    , _aciDiskIds :: [Text]
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
aciGatewayARN :: Lens' AddCache (Text)
aciGatewayARN = lens _aciGatewayARN (\s a -> s { _aciGatewayARN = a })
{-# INLINE aciGatewayARN #-}

aciDiskIds :: Lens' AddCache ([Text])
aciDiskIds = lens _aciDiskIds (\s a -> s { _aciDiskIds = a })
{-# INLINE aciDiskIds #-}

instance ToPath AddCache

instance ToQuery AddCache

instance ToHeaders AddCache

instance ToJSON AddCache

newtype AddCacheResponse = AddCacheResponse
    { _acoGatewayARN :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the gateway. Use the
      -- ListGateways operation to return a list of gateways for your
      -- account and region.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
acoGatewayARN :: Lens' AddCacheResponse (Maybe Text)
acoGatewayARN = lens _acoGatewayARN (\s a -> s { _acoGatewayARN = a })
{-# INLINE acoGatewayARN #-}

instance FromJSON AddCacheResponse

instance AWSRequest AddCache where
    type Sv AddCache = StorageGateway
    type Rs AddCache = AddCacheResponse

    request = get
    response _ = jsonResponse
