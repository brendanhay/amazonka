{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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

-- | Gets data records from a shard. Specify a shard iterator using the
-- ShardIterator parameter. The shard iterator specifies the position in the
-- shard from which you want to start reading data records sequentially. If
-- there are no records available in the portion of the shard that the
-- iterator points to, GetRecords returns an empty list. Note that it might
-- take multiple calls to get to a portion of the shard that contains records.
-- You can scale by provisioning multiple shards. Your application should have
-- one thread per shard, each reading continuously from its stream. To read
-- from a stream continually, call GetRecords in a loop. Use GetShardIterator
-- to get the shard iterator to specify in the first GetRecords call.
-- GetRecords returns a new shard iterator in NextShardIterator. Specify the
-- shard iterator returned in NextShardIterator in subsequent calls to
-- GetRecords. Note that if the shard has been closed, the shard iterator
-- can't return more data and GetRecords returns null in NextShardIterator.
-- You can terminate the loop when the shard is closed, or when the shard
-- iterator reaches the record with the sequence number or other attribute
-- that marks it as the last record to process. Each data record can be up to
-- 50 KB in size, and each shard can read up to 2 MB per second. You can
-- ensure that your calls don't exceed the maximum supported size or
-- throughput by specifying the maximum number of records that GetRecords can
-- return in the Limit parameter. Consider your average record size when
-- determining this limit. For example, if your average record size is 40 KB,
-- you can limit the data returned to about 1 MB per call by specifying 25 as
-- the limit. The size of the data returned by GetRecords will vary depending
-- on the utilization of the shard. The maximum size of data that GetRecords
-- can return is 10 MB. If a call returns 10 MB of data, subsequent calls made
-- within the next 5 seconds throw ProvisionedThroughputExceededException. If
-- there is insufficient provisioned throughput on the shard, subsequent calls
-- made within the next 1 second throw ProvisionedThroughputExceededException.
-- Note that GetRecords won't return any data when it throws an exception. For
-- this reason, we recommend that you wait one second between calls to
-- GetRecords; however, it's possible that the application will get exceptions
-- for longer than 1 second. To detect whether the application is falling
-- behind in processing, add a timestamp to your records and note how long it
-- takes to process them. You can also monitor how much data is in a stream
-- using the CloudWatch metrics for PutRecord. For more information, see
-- Monitoring Amazon Kinesis with Amazon CloudWatch in the Amazon Kinesis
-- Developer Guide.
--
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetRecords.html>
module Network.AWS.Kinesis.GetRecords
    (
    -- * Request
      GetRecords
    -- ** Request constructor
    , getRecords
    -- ** Request lenses
    , grLimit
    , grShardIterator

    -- * Response
    , GetRecordsResponse
    -- ** Response constructor
    , getRecordsResponse
    -- ** Response lenses
    , grrNextShardIterator
    , grrRecords
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Kinesis.Types
import qualified GHC.Exts

data GetRecords = GetRecords
    { _grLimit         :: Maybe Nat
    , _grShardIterator :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetRecords' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grLimit' @::@ 'Maybe' 'Natural'
--
-- * 'grShardIterator' @::@ 'Text'
--
getRecords :: Text -- ^ 'grShardIterator'
           -> GetRecords
getRecords p1 = GetRecords
    { _grShardIterator = p1
    , _grLimit         = Nothing
    }

-- | The maximum number of records to return. Specify a value of up to 10,000.
-- If you specify a value that is greater than 10,000, GetRecords throws
-- InvalidArgumentException.
grLimit :: Lens' GetRecords (Maybe Natural)
grLimit = lens _grLimit (\s a -> s { _grLimit = a })
    . mapping _Nat

-- | The position in the shard from which you want to start sequentially
-- reading data records. A shard iterator specifies this position using the
-- sequence number of a data record in the shard.
grShardIterator :: Lens' GetRecords Text
grShardIterator = lens _grShardIterator (\s a -> s { _grShardIterator = a })

data GetRecordsResponse = GetRecordsResponse
    { _grrNextShardIterator :: Maybe Text
    , _grrRecords           :: [Record]
    } deriving (Eq, Show, Generic)

-- | 'GetRecordsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrNextShardIterator' @::@ 'Maybe' 'Text'
--
-- * 'grrRecords' @::@ ['Record']
--
getRecordsResponse :: GetRecordsResponse
getRecordsResponse = GetRecordsResponse
    { _grrRecords           = mempty
    , _grrNextShardIterator = Nothing
    }

-- | The next position in the shard from which to start sequentially reading
-- data records. If set to null, the shard has been closed and the requested
-- iterator will not return any more data.
grrNextShardIterator :: Lens' GetRecordsResponse (Maybe Text)
grrNextShardIterator =
    lens _grrNextShardIterator (\s a -> s { _grrNextShardIterator = a })

-- | The data records retrieved from the shard.
grrRecords :: Lens' GetRecordsResponse [Record]
grrRecords = lens _grrRecords (\s a -> s { _grrRecords = a })

instance ToPath GetRecords where
    toPath = const "/"

instance ToQuery GetRecords where
    toQuery = const mempty

instance ToHeaders GetRecords

instance ToJSON GetRecords where
    toJSON GetRecords{..} = object
        [ "ShardIterator" .= _grShardIterator
        , "Limit"         .= _grLimit
        ]

instance AWSRequest GetRecords where
    type Sv GetRecords = Kinesis
    type Rs GetRecords = GetRecordsResponse

    request  = post "GetRecords"
    response = jsonResponse

instance FromJSON GetRecordsResponse where
    parseJSON = withObject "GetRecordsResponse" $ \o -> GetRecordsResponse
        <$> o .: "NextShardIterator"
        <*> o .: "Records"
