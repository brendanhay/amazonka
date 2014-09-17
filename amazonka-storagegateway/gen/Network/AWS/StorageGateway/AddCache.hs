{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.StorageGateway.AddCache
    (
    -- * Request
      AddCache
    -- ** Request constructor
    , mkAddCache
    -- ** Request lenses
    , acGatewayARN
    , acDiskIds

    -- * Response
    , AddCacheResponse
    -- ** Response constructor
    , mkAddCacheResponse
    -- ** Response lenses
    , acrGatewayARN
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data AddCache = AddCache
    { _acGatewayARN :: Text
    , _acDiskIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddCache' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
-- * @DiskIds ::@ @[Text]@
--
mkAddCache :: Text -- ^ 'acGatewayARN'
           -> [Text] -- ^ 'acDiskIds'
           -> AddCache
mkAddCache p1 p2 = AddCache
    { _acGatewayARN = p1
    , _acDiskIds = p2
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
acGatewayARN :: Lens' AddCache Text
acGatewayARN = lens _acGatewayARN (\s a -> s { _acGatewayARN = a })

acDiskIds :: Lens' AddCache [Text]
acDiskIds = lens _acDiskIds (\s a -> s { _acDiskIds = a })

instance ToPath AddCache

instance ToQuery AddCache

instance ToHeaders AddCache

instance ToJSON AddCache

newtype AddCacheResponse = AddCacheResponse
    { _acrGatewayARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AddCacheResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
mkAddCacheResponse :: AddCacheResponse
mkAddCacheResponse = AddCacheResponse
    { _acrGatewayARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
acrGatewayARN :: Lens' AddCacheResponse (Maybe Text)
acrGatewayARN = lens _acrGatewayARN (\s a -> s { _acrGatewayARN = a })

instance FromJSON AddCacheResponse

instance AWSRequest AddCache where
    type Sv AddCache = StorageGateway
    type Rs AddCache = AddCacheResponse

    request = get
    response _ = jsonResponse
