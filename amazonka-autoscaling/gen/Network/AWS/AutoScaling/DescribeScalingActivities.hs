{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.AutoScaling.DescribeScalingActivities
    (
    -- * Request
      DescribeScalingActivitiesType
    -- ** Request constructor
    , describeScalingActivities
    -- ** Request lenses
    , dsatActivityIds
    , dsatAutoScalingGroupName
    , dsatMaxRecords
    , dsatNextToken

    -- * Response
    , ActivitiesType
    -- ** Response constructor
    , describeScalingActivitiesResponse
    -- ** Response lenses
    , atActivities
    , atNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeScalingActivitiesType = DescribeScalingActivitiesType
    { _dsatActivityIds          :: [Text]
    , _dsatAutoScalingGroupName :: Maybe Text
    , _dsatMaxRecords           :: Maybe Int
    , _dsatNextToken            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeScalingActivitiesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsatActivityIds' @::@ ['Text']
--
-- * 'dsatAutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dsatMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dsatNextToken' @::@ 'Maybe' 'Text'
--
describeScalingActivities :: DescribeScalingActivitiesType
describeScalingActivities = DescribeScalingActivitiesType
    { _dsatActivityIds          = mempty
    , _dsatAutoScalingGroupName = Nothing
    , _dsatMaxRecords           = Nothing
    , _dsatNextToken            = Nothing
    }

-- | A list containing the activity IDs of the desired scaling activities. If
-- this list is omitted, all activities are described. If an
-- AutoScalingGroupName is provided, the results are limited to that group.
-- The list of requested activities cannot contain more than 50 items. If
-- unknown activities are requested, they are ignored with no error.
dsatActivityIds :: Lens' DescribeScalingActivitiesType [Text]
dsatActivityIds = lens _dsatActivityIds (\s a -> s { _dsatActivityIds = a })

-- | The name of the AutoScalingGroup.
dsatAutoScalingGroupName :: Lens' DescribeScalingActivitiesType (Maybe Text)
dsatAutoScalingGroupName =
    lens _dsatAutoScalingGroupName
        (\s a -> s { _dsatAutoScalingGroupName = a })

-- | The maximum number of scaling activities to return.
dsatMaxRecords :: Lens' DescribeScalingActivitiesType (Maybe Int)
dsatMaxRecords = lens _dsatMaxRecords (\s a -> s { _dsatMaxRecords = a })

-- | A string that marks the start of the next batch of returned results for
-- pagination.
dsatNextToken :: Lens' DescribeScalingActivitiesType (Maybe Text)
dsatNextToken = lens _dsatNextToken (\s a -> s { _dsatNextToken = a })

instance ToQuery DescribeScalingActivitiesType

instance ToPath DescribeScalingActivitiesType where
    toPath = const "/"

data ActivitiesType = ActivitiesType
    { _atActivities :: [Activity]
    , _atNextToken  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ActivitiesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atActivities' @::@ ['Activity']
--
-- * 'atNextToken' @::@ 'Maybe' 'Text'
--
describeScalingActivitiesResponse :: ActivitiesType
describeScalingActivitiesResponse = ActivitiesType
    { _atActivities = mempty
    , _atNextToken  = Nothing
    }

-- | A list of the requested scaling activities.
atActivities :: Lens' ActivitiesType [Activity]
atActivities = lens _atActivities (\s a -> s { _atActivities = a })

-- | Acts as a paging mechanism for large result sets. Set to a non-empty
-- string if there are additional results waiting to be returned. Pass this
-- in to subsequent calls to return additional results.
atNextToken :: Lens' ActivitiesType (Maybe Text)
atNextToken = lens _atNextToken (\s a -> s { _atNextToken = a })

instance FromXML ActivitiesType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ActivitiesType"

instance AWSRequest DescribeScalingActivitiesType where
    type Sv DescribeScalingActivitiesType = AutoScaling
    type Rs DescribeScalingActivitiesType = ActivitiesType

    request  = post "DescribeScalingActivities"
    response = xmlResponse $ \h x -> ActivitiesType
        <$> x %| "Activities"
        <*> x %| "NextToken"
