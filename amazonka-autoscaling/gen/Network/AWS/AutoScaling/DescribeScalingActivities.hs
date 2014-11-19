{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeScalingActivities
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the scaling activities for the specified Auto Scaling group. If the
-- specified ActivityIds list is empty, all the activities from the past six
-- weeks are returned. Activities are sorted by the start time. Activities
-- still in progress appear first on the list. This action supports
-- pagination. If the response includes a token, there are more records
-- available. To get the additional records, repeat the request with the
-- response token as the NextToken parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScalingActivities.html>
module Network.AWS.AutoScaling.DescribeScalingActivities
    (
    -- * Request
      DescribeScalingActivities
    -- ** Request constructor
    , describeScalingActivities
    -- ** Request lenses
    , dsa2ActivityIds
    , dsa2AutoScalingGroupName
    , dsa2MaxRecords
    , dsa2NextToken

    -- * Response
    , DescribeScalingActivitiesResponse
    -- ** Response constructor
    , describeScalingActivitiesResponse
    -- ** Response lenses
    , dsar1Activities
    , dsar1NextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeScalingActivities = DescribeScalingActivities
    { _dsa2ActivityIds          :: List "ActivityIds" Text
    , _dsa2AutoScalingGroupName :: Maybe Text
    , _dsa2MaxRecords           :: Maybe Int
    , _dsa2NextToken            :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DescribeScalingActivities' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsa2ActivityIds' @::@ ['Text']
--
-- * 'dsa2AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dsa2MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dsa2NextToken' @::@ 'Maybe' 'Text'
--
describeScalingActivities :: DescribeScalingActivities
describeScalingActivities = DescribeScalingActivities
    { _dsa2ActivityIds          = mempty
    , _dsa2AutoScalingGroupName = Nothing
    , _dsa2MaxRecords           = Nothing
    , _dsa2NextToken            = Nothing
    }

-- | A list containing the activity IDs of the desired scaling activities. If
-- this list is omitted, all activities are described. If an
-- AutoScalingGroupName is provided, the results are limited to that group.
-- The list of requested activities cannot contain more than 50 items. If
-- unknown activities are requested, they are ignored with no error.
dsa2ActivityIds :: Lens' DescribeScalingActivities [Text]
dsa2ActivityIds = lens _dsa2ActivityIds (\s a -> s { _dsa2ActivityIds = a }) . _List

-- | The name of the AutoScalingGroup.
dsa2AutoScalingGroupName :: Lens' DescribeScalingActivities (Maybe Text)
dsa2AutoScalingGroupName =
    lens _dsa2AutoScalingGroupName
        (\s a -> s { _dsa2AutoScalingGroupName = a })

-- | The maximum number of scaling activities to return.
dsa2MaxRecords :: Lens' DescribeScalingActivities (Maybe Int)
dsa2MaxRecords = lens _dsa2MaxRecords (\s a -> s { _dsa2MaxRecords = a })

-- | A string that marks the start of the next batch of returned results for
-- pagination.
dsa2NextToken :: Lens' DescribeScalingActivities (Maybe Text)
dsa2NextToken = lens _dsa2NextToken (\s a -> s { _dsa2NextToken = a })

data DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { _dsar1Activities :: List "Activities" Activity
    , _dsar1NextToken  :: Maybe Text
    } deriving (Eq, Show)

-- | 'DescribeScalingActivitiesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsar1Activities' @::@ ['Activity']
--
-- * 'dsar1NextToken' @::@ 'Maybe' 'Text'
--
describeScalingActivitiesResponse :: DescribeScalingActivitiesResponse
describeScalingActivitiesResponse = DescribeScalingActivitiesResponse
    { _dsar1Activities = mempty
    , _dsar1NextToken  = Nothing
    }

-- | A list of the requested scaling activities.
dsar1Activities :: Lens' DescribeScalingActivitiesResponse [Activity]
dsar1Activities = lens _dsar1Activities (\s a -> s { _dsar1Activities = a }) . _List

-- | Acts as a paging mechanism for large result sets. Set to a non-empty
-- string if there are additional results waiting to be returned. Pass this
-- in to subsequent calls to return additional results.
dsar1NextToken :: Lens' DescribeScalingActivitiesResponse (Maybe Text)
dsar1NextToken = lens _dsar1NextToken (\s a -> s { _dsar1NextToken = a })

instance ToPath DescribeScalingActivities where
    toPath = const "/"

instance ToQuery DescribeScalingActivities where
    toQuery DescribeScalingActivities{..} = mconcat
        [ "ActivityIds"          =? _dsa2ActivityIds
        , "AutoScalingGroupName" =? _dsa2AutoScalingGroupName
        , "MaxRecords"           =? _dsa2MaxRecords
        , "NextToken"            =? _dsa2NextToken
        ]

instance ToHeaders DescribeScalingActivities

instance AWSRequest DescribeScalingActivities where
    type Sv DescribeScalingActivities = AutoScaling
    type Rs DescribeScalingActivities = DescribeScalingActivitiesResponse

    request  = post "DescribeScalingActivities"
    response = xmlResponse

instance FromXML DescribeScalingActivitiesResponse where
    parseXML = withElement "DescribeScalingActivitiesResult" $ \x -> DescribeScalingActivitiesResponse
        <$> x .@  "Activities"
        <*> x .@? "NextToken"

instance AWSPager DescribeScalingActivities where
    next rq rs = (\x -> rq & dsa2NextToken ?~ x)
        <$> (rs ^. dsar1NextToken)
