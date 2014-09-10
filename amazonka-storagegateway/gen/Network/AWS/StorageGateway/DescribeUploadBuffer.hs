{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns information about the upload buffer of a gateway.
-- This operation is supported for both the gateway-stored and gateway-cached
-- volume architectures. The response includes disk IDs that are configured as
-- upload buffer space, and it includes the amount of upload buffer space
-- allocated and used. Example Request The following example shows a request
-- to obtain a description of a gateway's working storage. POST / HTTP/1.1
-- Host: storagegateway.us-east-1.amazonaws.com Content-Type:
-- application/x-amz-json-1.1 Authorization: AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20120425/us-east-1/storagegateway/aws4_request,
-- SignedHeaders=content-type;host;x-amz-date;x-amz-target,
-- Signature=9cd5a3584d1d67d57e61f120f35102d6b3649066abdd4bf4bbcf05bd9f2f8fe2
-- x-amz-date: 20120912T120000Z x-amz-target:
-- StorageGateway_20120630.DescribeUploadBuffer {
-- "GatewayARN":"arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway"
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- gur28r2rqlgb8vvs0mq17hlgij1q8glle1qeu3kpgg6f0kstauu0 Date: Wed, 12 Sep 2012
-- 12:00:02 GMT Content-Type: application/x-amz-json-1.1 Content-length: 271 {
-- "DiskIds": [ "pci-0000:03:00.0-scsi-0:0:0:0",
-- "pci-0000:04:00.0-scsi-0:1:0:0" ], "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "UploadBufferAllocatedInBytes": 161061273600, "UploadBufferUsedInBytes": 0
-- }.
module Network.AWS.StorageGateway
    (
    -- * Request
      DescribeUploadBuffer
    -- ** Request constructor
    , mkDescribeUploadBuffer
    -- ** Request lenses
    , dubGatewayARN

    -- * Response
    , DescribeUploadBufferResponse
    -- ** Response constructor
    , mkDescribeUploadBufferResponse
    -- ** Response lenses
    , dubrGatewayARN
    , dubrDiskIds
    , dubrUploadBufferUsedInBytes
    , dubrUploadBufferAllocatedInBytes
    ) where

import Network.AWS.StorageGateway.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DescribeUploadBuffer = DescribeUploadBuffer
    { _dubGatewayARN :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeUploadBuffer' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Text@
--
mkDescribeUploadBuffer :: Text -- ^ 'dubGatewayARN'
                       -> DescribeUploadBuffer
mkDescribeUploadBuffer p1 = DescribeUploadBuffer
    { _dubGatewayARN = p1
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dubGatewayARN :: Lens' DescribeUploadBuffer Text
dubGatewayARN = lens _dubGatewayARN (\s a -> s { _dubGatewayARN = a })

instance ToPath DescribeUploadBuffer

instance ToQuery DescribeUploadBuffer

instance ToHeaders DescribeUploadBuffer

instance ToJSON DescribeUploadBuffer

data DescribeUploadBufferResponse = DescribeUploadBufferResponse
    { _dubrGatewayARN :: !(Maybe Text)
    , _dubrDiskIds :: [Text]
    , _dubrUploadBufferUsedInBytes :: !(Maybe Integer)
    , _dubrUploadBufferAllocatedInBytes :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeUploadBufferResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GatewayARN ::@ @Maybe Text@
--
-- * @DiskIds ::@ @[Text]@
--
-- * @UploadBufferUsedInBytes ::@ @Maybe Integer@
--
-- * @UploadBufferAllocatedInBytes ::@ @Maybe Integer@
--
mkDescribeUploadBufferResponse :: DescribeUploadBufferResponse
mkDescribeUploadBufferResponse = DescribeUploadBufferResponse
    { _dubrGatewayARN = Nothing
    , _dubrDiskIds = mempty
    , _dubrUploadBufferUsedInBytes = Nothing
    , _dubrUploadBufferAllocatedInBytes = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
dubrGatewayARN :: Lens' DescribeUploadBufferResponse (Maybe Text)
dubrGatewayARN = lens _dubrGatewayARN (\s a -> s { _dubrGatewayARN = a })

dubrDiskIds :: Lens' DescribeUploadBufferResponse [Text]
dubrDiskIds = lens _dubrDiskIds (\s a -> s { _dubrDiskIds = a })

dubrUploadBufferUsedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrUploadBufferUsedInBytes =
    lens _dubrUploadBufferUsedInBytes
         (\s a -> s { _dubrUploadBufferUsedInBytes = a })

dubrUploadBufferAllocatedInBytes :: Lens' DescribeUploadBufferResponse (Maybe Integer)
dubrUploadBufferAllocatedInBytes =
    lens _dubrUploadBufferAllocatedInBytes
         (\s a -> s { _dubrUploadBufferAllocatedInBytes = a })

instance FromJSON DescribeUploadBufferResponse

instance AWSRequest DescribeUploadBuffer where
    type Sv DescribeUploadBuffer = StorageGateway
    type Rs DescribeUploadBuffer = DescribeUploadBufferResponse

    request = get
    response _ = jsonResponse
