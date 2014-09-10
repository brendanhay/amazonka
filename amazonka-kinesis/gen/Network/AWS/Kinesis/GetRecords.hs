{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Kinesis.GetRecords
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns one or more data records from a shard. A GetRecords
-- operation request can retrieve up to 10 MB of data. You specify a shard
-- iterator for the shard that you want to read data from in the ShardIterator
-- parameter. The shard iterator specifies the position in the shard from
-- which you want to start reading data records sequentially. A shard iterator
-- specifies this position using the sequence number of a data record in the
-- shard. For more information about the shard iterator, see GetShardIterator.
-- GetRecords may return a partial result if the response size limit is
-- exceeded. You will get an error, but not a partial result if the shard's
-- provisioned throughput is exceeded, the shard iterator has expired, or an
-- internal processing failure has occurred. Clients can request a smaller
-- amount of data by specifying a maximum number of returned records using the
-- Limit parameter. The Limit parameter can be set to an integer value of up
-- to 10,000. If you set the value to an integer greater than 10,000, you will
-- receive InvalidArgumentException. A new shard iterator is returned by every
-- GetRecords request in NextShardIterator, which you use in the ShardIterator
-- parameter of the next GetRecords request. When you repeatedly read from an
-- Amazon Kinesis stream use a GetShardIterator request to get the first shard
-- iterator to use in your first GetRecords request and then use the shard
-- iterator returned in NextShardIterator for subsequent reads. GetRecords can
-- return null for the NextShardIterator to reflect that the shard has been
-- closed and that the requested shard iterator would never have returned more
-- data. If no items can be processed because of insufficient provisioned
-- throughput on the shard involved in the request, GetRecords throws
-- ProvisionedThroughputExceededException. Get Data from the Shards in a
-- Stream The following is an example of an Amazon Kinesis GetRecords request
-- and response. POST / HTTP/1.1 Host: kinesis.. x-amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.GetRecords { "ShardIterator":
-- "AAAAAAAAAAETYyAYzd665+8e0X7JTsASDM/Hr2rSwc0X2qz93iuA3udrjTH+ikQvpQk/1ZcMMLzRdAesqwBGPnsthzU0/CBlM/U8/8oEqGwX3pKw0XyeDNRAAZyXBo3MqkQtCpXhr942BRTjvWKhFz7OmCb2Ncfr8Tl2cBktooi6kJhr+djN5WYkB38Rr3akRgCl9qaU4dY=",
-- "Limit": 2 } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "NextShardIterator":
-- "AAAAAAAAAAHsW8zCWf9164uy8Epue6WS3w6wmj4a4USt+CNvMd6uXQ+HL5vAJMznqqC0DLKsIjuoiTi1BpT6nW0LN2M2D56zM5H8anHm30Gbri9ua+qaGgj+3XTyvbhpERfrezgLHbPB/rIcVpykJbaSj5tmcXYRmFnqZBEyHwtZYFmh6hvWVFkIwLuMZLMrpWhG5r5hzkE=",
-- "Records": [ { "Data": "XzxkYXRhPl8w", "PartitionKey": "partitionKey",
-- "SequenceNumber": "21269319989652663814458848515492872193" } ] }.
module Network.AWS.Kinesis.GetRecords
    (
    -- * Request
      GetRecords
    -- ** Request constructor
    , mkGetRecords
    -- ** Request lenses
    , grShardIterator
    , grLimit

    -- * Response
    , GetRecordsResponse
    -- ** Response constructor
    , mkGetRecordsResponse
    -- ** Response lenses
    , grrRecords
    , grrNextShardIterator
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input of a GetRecords operation.
data GetRecords = GetRecords
    { _grShardIterator :: !Text
    , _grLimit :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetRecords' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ShardIterator ::@ @Text@
--
-- * @Limit ::@ @Maybe Integer@
--
mkGetRecords :: Text -- ^ 'grShardIterator'
             -> GetRecords
mkGetRecords p1 = GetRecords
    { _grShardIterator = p1
    , _grLimit = Nothing
    }

-- | The position in the shard from which you want to start sequentially reading
-- data records.
grShardIterator :: Lens' GetRecords Text
grShardIterator = lens _grShardIterator (\s a -> s { _grShardIterator = a })

-- | The maximum number of records to return, which can be set to a value of up
-- to 10,000.
grLimit :: Lens' GetRecords (Maybe Integer)
grLimit = lens _grLimit (\s a -> s { _grLimit = a })

instance ToPath GetRecords

instance ToQuery GetRecords

instance ToHeaders GetRecords

instance ToJSON GetRecords

-- | Represents the output of a GetRecords operation.
data GetRecordsResponse = GetRecordsResponse
    { _grrRecords :: [Record]
    , _grrNextShardIterator :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetRecordsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Records ::@ @[Record]@
--
-- * @NextShardIterator ::@ @Maybe Text@
--
mkGetRecordsResponse :: [Record] -- ^ 'grrRecords'
                     -> GetRecordsResponse
mkGetRecordsResponse p1 = GetRecordsResponse
    { _grrRecords = p1
    , _grrNextShardIterator = Nothing
    }

-- | The data records retrieved from the shard.
grrRecords :: Lens' GetRecordsResponse [Record]
grrRecords = lens _grrRecords (\s a -> s { _grrRecords = a })

-- | The next position in the shard from which to start sequentially reading
-- data records. If set to null, the shard has been closed and the requested
-- iterator will not return any more data.
grrNextShardIterator :: Lens' GetRecordsResponse (Maybe Text)
grrNextShardIterator =
    lens _grrNextShardIterator (\s a -> s { _grrNextShardIterator = a })

instance FromJSON GetRecordsResponse

instance AWSRequest GetRecords where
    type Sv GetRecords = Kinesis
    type Rs GetRecords = GetRecordsResponse

    request = get
    response _ = jsonResponse
