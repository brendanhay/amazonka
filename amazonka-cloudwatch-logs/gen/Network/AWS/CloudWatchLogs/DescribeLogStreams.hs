{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.DescribeLogStreams
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all the log streams that are associated with the specified log
-- group. The list returned in the response is ASCII-sorted by log stream
-- name. By default, this operation returns up to 50 log streams. If there are
-- more log streams to list, the response would contain a nextToken value in
-- the response body. You can also limit the number of log streams returned in
-- the response by specifying the limit parameter in the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogStreams.html>
module Network.AWS.CloudWatchLogs.DescribeLogStreams
    (
    -- * Request
      DescribeLogStreams
    -- ** Request constructor
    , describeLogStreams
    -- ** Request lenses
    , dls1Limit
    , dls1LogGroupName
    , dls1LogStreamNamePrefix
    , dls1NextToken

    -- * Response
    , DescribeLogStreamsResponse
    -- ** Response constructor
    , describeLogStreamsResponse
    -- ** Response lenses
    , dlsrLogStreams
    , dlsrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data DescribeLogStreams = DescribeLogStreams
    { _dls1Limit               :: Maybe Nat
    , _dls1LogGroupName        :: Text
    , _dls1LogStreamNamePrefix :: Maybe Text
    , _dls1NextToken           :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLogStreams' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dls1Limit' @::@ 'Maybe' 'Natural'
--
-- * 'dls1LogGroupName' @::@ 'Text'
--
-- * 'dls1LogStreamNamePrefix' @::@ 'Maybe' 'Text'
--
-- * 'dls1NextToken' @::@ 'Maybe' 'Text'
--
describeLogStreams :: Text -- ^ 'dls1LogGroupName'
                   -> DescribeLogStreams
describeLogStreams p1 = DescribeLogStreams
    { _dls1LogGroupName        = p1
    , _dls1LogStreamNamePrefix = Nothing
    , _dls1NextToken           = Nothing
    , _dls1Limit               = Nothing
    }

-- | The maximum number of items returned in the response. If you don't
-- specify a value, the request would return up to 50 items.
dls1Limit :: Lens' DescribeLogStreams (Maybe Natural)
dls1Limit = lens _dls1Limit (\s a -> s { _dls1Limit = a })
    . mapping _Nat

dls1LogGroupName :: Lens' DescribeLogStreams Text
dls1LogGroupName = lens _dls1LogGroupName (\s a -> s { _dls1LogGroupName = a })

dls1LogStreamNamePrefix :: Lens' DescribeLogStreams (Maybe Text)
dls1LogStreamNamePrefix =
    lens _dls1LogStreamNamePrefix (\s a -> s { _dls1LogStreamNamePrefix = a })

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- DescribeLogStreams request.
dls1NextToken :: Lens' DescribeLogStreams (Maybe Text)
dls1NextToken = lens _dls1NextToken (\s a -> s { _dls1NextToken = a })

data DescribeLogStreamsResponse = DescribeLogStreamsResponse
    { _dlsrLogStreams :: [LogStream]
    , _dlsrNextToken  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeLogStreamsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsrLogStreams' @::@ ['LogStream']
--
-- * 'dlsrNextToken' @::@ 'Maybe' 'Text'
--
describeLogStreamsResponse :: DescribeLogStreamsResponse
describeLogStreamsResponse = DescribeLogStreamsResponse
    { _dlsrLogStreams = mempty
    , _dlsrNextToken  = Nothing
    }

dlsrLogStreams :: Lens' DescribeLogStreamsResponse [LogStream]
dlsrLogStreams = lens _dlsrLogStreams (\s a -> s { _dlsrLogStreams = a })

dlsrNextToken :: Lens' DescribeLogStreamsResponse (Maybe Text)
dlsrNextToken = lens _dlsrNextToken (\s a -> s { _dlsrNextToken = a })

instance ToPath DescribeLogStreams where
    toPath = const "/"

instance ToQuery DescribeLogStreams where
    toQuery = const mempty

instance ToHeaders DescribeLogStreams
instance ToJSON DescribeLogStreams where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DescribeLogStreams where
    type Sv DescribeLogStreams = CloudWatchLogs
    type Rs DescribeLogStreams = DescribeLogStreamsResponse

    request  = post "DescribeLogStreams"
    response = jsonResponse

instance FromJSON DescribeLogStreamsResponse where
    parseJSON = genericParseJSON jsonOptions
