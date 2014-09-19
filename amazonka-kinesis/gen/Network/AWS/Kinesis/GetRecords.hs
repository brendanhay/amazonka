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

-- | Gets data records from a shard. You specify a shard iterator for the shard
-- using the ShardIterator parameter. The shard iterator specifies the
-- position in the shard from which you want to start reading data records
-- sequentially. GetRecords returns a new shard iterator in NextShardIterator.
-- If the shard has been closed, the shard iterator can't return more data and
-- GetRecords returns null in NextShardIterator. To read from an Amazon
-- Kinesis stream continually, use GetShardIterator to get the shard iterator
-- to specify in the first GetRecords call, and then specify the shard
-- iterator returned in NextShardIterator in subsequent calls. If there are no
-- records available, GetRecords returns an empty list. The size of the data
-- returned by GetRecords will vary, but the maximum size is 10 MB. Each data
-- record can be up to 50 KB in size, and each shard can read up to 2 MB per
-- second. You can ensure that your calls don't exceed the maximum size or
-- throughput by using the Limit parameter to specify the maximum number of
-- records that GetRecords can return. Consider your average record size when
-- specifying a limit. For example, if your average record size is 40 KB, you
-- can limit the data returned to about 1 MB per call using a limit of 25. If
-- there is insufficient provisioned throughput on the shard involved in the
-- request, subsequent calls to GetRecords made within the next one second
-- throw ProvisionedThroughputExceededException. GetRecords won't return data
-- when it throws an exception, so wait one second before making another call
-- or the application won't get any records. Also, you can scale by
-- provisioning multiple shards. To detect whether you are falling behind in
-- processing, add a timestamp to your records and note how long it takes to
-- process them. To get data from the shards in a stream The following example
-- gets data from the shards in a stream. POST / HTTP/1.1 Host: kinesis..
-- x-amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Content-Type: application/x-amz-json-1.1
-- Content-Length: Connection: Keep-Alive]]> X-Amz-Target:
-- Kinesis_20131202.GetRecords { "ShardIterator":
-- "AAAAAAAAAAETYyAYzd665+8e0X7JTsASDM/Hr2rSwc0X2qz93iuA3udrjTH+ikQvpQk/1ZcMMLzRdAesqwBGPnsthzU0/CBlM/U8/8oEqGwX3pKw0XyeDNRAAZyXBo3MqkQtCpXhr942BRTjvWKhFz7OmCb2Ncfr8Tl2cBktooi6kJhr+djN5WYkB38Rr3akRgCl9qaU4dY=",
-- "Limit": 25 } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]> { "NextShardIterator":
-- "AAAAAAAAAAHsW8zCWf9164uy8Epue6WS3w6wmj4a4USt+CNvMd6uXQ+HL5vAJMznqqC0DLKsIjuoiTi1BpT6nW0LN2M2D56zM5H8anHm30Gbri9ua+qaGgj+3XTyvbhpERfrezgLHbPB/rIcVpykJbaSj5tmcXYRmFnqZBEyHwtZYFmh6hvWVFkIwLuMZLMrpWhG5r5hzkE=",
-- "Records": [ { "Data": "XzxkYXRhPl8w", "PartitionKey": "partitionKey",
-- "SequenceNumber": "21269319989652663814458848515492872193" } ] }.
module Network.AWS.Kinesis.GetRecords
    (
    -- * Request
      GetRecords
    -- ** Request constructor
    , getRecords
    -- ** Request lenses
    , grShardIterator
    , grLimit

    -- * Response
    , GetRecordsResponse
    -- ** Response constructor
    , getRecordsResponse
    -- ** Response lenses
    , grrRecords
    , grrNextShardIterator
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Represents the input for GetRecords.
data GetRecords = GetRecords
    { _grShardIterator :: Text
    , _grLimit :: Maybe Integer
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
getRecords :: Text -- ^ 'grShardIterator'
             -> GetRecords
getRecords p1 = GetRecords
    { _grShardIterator = p1
    , _grLimit = Nothing
    }

-- | The position in the shard from which you want to start sequentially reading
-- data records. A shard iterator specifies this position using the sequence
-- number of a data record in the shard.
grShardIterator :: Lens' GetRecords Text
grShardIterator = lens _grShardIterator (\s a -> s { _grShardIterator = a })

-- | The maximum size of data to return, in MB. Specify a value of up to 10,000.
-- If you specify a value that is greater than 10,000, GetRecords throws
-- InvalidArgumentException.
grLimit :: Lens' GetRecords (Maybe Integer)
grLimit = lens _grLimit (\s a -> s { _grLimit = a })

instance ToPath GetRecords

instance ToQuery GetRecords

instance ToHeaders GetRecords

instance ToJSON GetRecords

-- | Represents the output for GetRecords.
data GetRecordsResponse = GetRecordsResponse
    { _grrRecords :: [Record]
    , _grrNextShardIterator :: Maybe Text
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
getRecordsResponse :: [Record] -- ^ 'grrRecords'
                     -> GetRecordsResponse
getRecordsResponse p1 = GetRecordsResponse
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
