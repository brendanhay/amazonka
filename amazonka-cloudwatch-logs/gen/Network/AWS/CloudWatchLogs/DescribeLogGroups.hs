{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all the log groups that are associated with the AWS account making
-- the request. The list returned in the response is ASCII-sorted by log group
-- name. By default, this operation returns up to 50 log groups. If there are
-- more log groups to list, the response would contain a nextToken value in
-- the response body. You can also limit the number of log groups returned in
-- the response by specifying the limit parameter in the request.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeLogGroups.html>
module Network.AWS.CloudWatchLogs.DescribeLogGroups
    (
    -- * Request
      DescribeLogGroups
    -- ** Request constructor
    , describeLogGroups
    -- ** Request lenses
    , dlgLimit
    , dlgLogGroupNamePrefix
    , dlgNextToken

    -- * Response
    , DescribeLogGroupsResponse
    -- ** Response constructor
    , describeLogGroupsResponse
    -- ** Response lenses
    , dlgrLogGroups
    , dlgrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudWatchLogs.Types
import qualified GHC.Exts

data DescribeLogGroups = DescribeLogGroups
    { _dlgLimit              :: Maybe Nat
    , _dlgLogGroupNamePrefix :: Maybe Text
    , _dlgNextToken          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLogGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlgLimit' @::@ 'Maybe' 'Natural'
--
-- * 'dlgLogGroupNamePrefix' @::@ 'Maybe' 'Text'
--
-- * 'dlgNextToken' @::@ 'Maybe' 'Text'
--
describeLogGroups :: DescribeLogGroups
describeLogGroups = DescribeLogGroups
    { _dlgLogGroupNamePrefix = Nothing
    , _dlgNextToken          = Nothing
    , _dlgLimit              = Nothing
    }

-- | The maximum number of items returned in the response. If you don't
-- specify a value, the request would return up to 50 items.
dlgLimit :: Lens' DescribeLogGroups (Maybe Natural)
dlgLimit = lens _dlgLimit (\s a -> s { _dlgLimit = a })
    . mapping _Nat

dlgLogGroupNamePrefix :: Lens' DescribeLogGroups (Maybe Text)
dlgLogGroupNamePrefix =
    lens _dlgLogGroupNamePrefix (\s a -> s { _dlgLogGroupNamePrefix = a })

-- | A string token used for pagination that points to the next page of
-- results. It must be a value obtained from the response of the previous
-- DescribeLogGroups request.
dlgNextToken :: Lens' DescribeLogGroups (Maybe Text)
dlgNextToken = lens _dlgNextToken (\s a -> s { _dlgNextToken = a })

data DescribeLogGroupsResponse = DescribeLogGroupsResponse
    { _dlgrLogGroups :: [LogGroup]
    , _dlgrNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeLogGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlgrLogGroups' @::@ ['LogGroup']
--
-- * 'dlgrNextToken' @::@ 'Maybe' 'Text'
--
describeLogGroupsResponse :: DescribeLogGroupsResponse
describeLogGroupsResponse = DescribeLogGroupsResponse
    { _dlgrLogGroups = mempty
    , _dlgrNextToken = Nothing
    }

dlgrLogGroups :: Lens' DescribeLogGroupsResponse [LogGroup]
dlgrLogGroups = lens _dlgrLogGroups (\s a -> s { _dlgrLogGroups = a })

dlgrNextToken :: Lens' DescribeLogGroupsResponse (Maybe Text)
dlgrNextToken = lens _dlgrNextToken (\s a -> s { _dlgrNextToken = a })

instance ToPath DescribeLogGroups where
    toPath = const "/"

instance ToQuery DescribeLogGroups where
    toQuery = const mempty

instance ToHeaders DescribeLogGroups

instance ToJSON DescribeLogGroups where
    toJSON DescribeLogGroups{..} = object
        [ "logGroupNamePrefix" .= _dlgLogGroupNamePrefix
        , "nextToken"          .= _dlgNextToken
        , "limit"              .= _dlgLimit
        ]

instance AWSRequest DescribeLogGroups where
    type Sv DescribeLogGroups = CloudWatchLogs
    type Rs DescribeLogGroups = DescribeLogGroupsResponse

    request  = post "DescribeLogGroups"
    response = jsonResponse

instance FromJSON DescribeLogGroupsResponse where
    parseJSON = withObject "DescribeLogGroupsResponse" $ \o -> DescribeLogGroupsResponse
        <$> o .: "logGroups"
        <*> o .: "nextToken"
