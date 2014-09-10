{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeScheduledActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all the actions scheduled for your Auto Scaling group that haven't
-- been executed. To see a list of actions already executed, see the activity
-- record returned in DescribeScalingActivities.
module Network.AWS.AutoScaling.DescribeScheduledActions
    (
    -- * Request
      DescribeScheduledActions
    -- ** Request constructor
    , mkDescribeScheduledActions
    -- ** Request lenses
    , dsa2AutoScalingGroupName
    , dsa2ScheduledActionNames
    , dsa2StartTime
    , dsa2EndTime
    , dsa2NextToken
    , dsa2MaxRecords

    -- * Response
    , DescribeScheduledActionsResponse
    -- ** Response constructor
    , mkDescribeScheduledActionsResponse
    -- ** Response lenses
    , dsarrScheduledUpdateGroupActions
    , dsarrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DescribeScheduledActions = DescribeScheduledActions
    { _dsa2AutoScalingGroupName :: !(Maybe Text)
    , _dsa2ScheduledActionNames :: [Text]
    , _dsa2StartTime :: !(Maybe ISO8601)
    , _dsa2EndTime :: !(Maybe ISO8601)
    , _dsa2NextToken :: !(Maybe Text)
    , _dsa2MaxRecords :: !(Maybe Integer)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScheduledActions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Maybe Text@
--
-- * @ScheduledActionNames ::@ @[Text]@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @NextToken ::@ @Maybe Text@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
mkDescribeScheduledActions :: DescribeScheduledActions
mkDescribeScheduledActions = DescribeScheduledActions
    { _dsa2AutoScalingGroupName = Nothing
    , _dsa2ScheduledActionNames = mempty
    , _dsa2StartTime = Nothing
    , _dsa2EndTime = Nothing
    , _dsa2NextToken = Nothing
    , _dsa2MaxRecords = Nothing
    }

-- | The name of the Auto Scaling group.
dsa2AutoScalingGroupName :: Lens' DescribeScheduledActions (Maybe Text)
dsa2AutoScalingGroupName =
    lens _dsa2AutoScalingGroupName
         (\s a -> s { _dsa2AutoScalingGroupName = a })

-- | A list of scheduled actions to be described. If this list is omitted, all
-- scheduled actions are described. The list of requested scheduled actions
-- cannot contain more than 50 items. If an auto scaling group name is
-- provided, the results are limited to that group. If unknown scheduled
-- actions are requested, they are ignored with no error.
dsa2ScheduledActionNames :: Lens' DescribeScheduledActions [Text]
dsa2ScheduledActionNames =
    lens _dsa2ScheduledActionNames
         (\s a -> s { _dsa2ScheduledActionNames = a })

-- | The earliest scheduled start time to return. If scheduled action names are
-- provided, this field will be ignored.
dsa2StartTime :: Lens' DescribeScheduledActions (Maybe ISO8601)
dsa2StartTime = lens _dsa2StartTime (\s a -> s { _dsa2StartTime = a })

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this field is ignored.
dsa2EndTime :: Lens' DescribeScheduledActions (Maybe ISO8601)
dsa2EndTime = lens _dsa2EndTime (\s a -> s { _dsa2EndTime = a })

-- | A string that marks the start of the next batch of returned results.
dsa2NextToken :: Lens' DescribeScheduledActions (Maybe Text)
dsa2NextToken = lens _dsa2NextToken (\s a -> s { _dsa2NextToken = a })

-- | The maximum number of scheduled actions to return.
dsa2MaxRecords :: Lens' DescribeScheduledActions (Maybe Integer)
dsa2MaxRecords = lens _dsa2MaxRecords (\s a -> s { _dsa2MaxRecords = a })

instance ToQuery DescribeScheduledActions where
    toQuery = genericQuery def

-- | A scaling action that is scheduled for a future time and date. An action
-- can be scheduled up to thirty days in advance. Starting with API version
-- 2011-01-01, you can use recurrence to specify that a scaling action occurs
-- regularly on a schedule.
data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _dsarrScheduledUpdateGroupActions :: [ScheduledUpdateGroupAction]
    , _dsarrNextToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScheduledActionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ScheduledUpdateGroupActions ::@ @[ScheduledUpdateGroupAction]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeScheduledActionsResponse :: DescribeScheduledActionsResponse
mkDescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _dsarrScheduledUpdateGroupActions = mempty
    , _dsarrNextToken = Nothing
    }

-- | A list of scheduled actions designed to update an Auto Scaling group.
dsarrScheduledUpdateGroupActions :: Lens' DescribeScheduledActionsResponse [ScheduledUpdateGroupAction]
dsarrScheduledUpdateGroupActions =
    lens _dsarrScheduledUpdateGroupActions
         (\s a -> s { _dsarrScheduledUpdateGroupActions = a })

-- | A string that marks the start of the next batch of returned results.
dsarrNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarrNextToken = lens _dsarrNextToken (\s a -> s { _dsarrNextToken = a })

instance FromXML DescribeScheduledActionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScheduledActions where
    type Sv DescribeScheduledActions = AutoScaling
    type Rs DescribeScheduledActions = DescribeScheduledActionsResponse

    request = post "DescribeScheduledActions"
    response _ = xmlResponse

instance AWSPager DescribeScheduledActions where
    next rq rs = (\x -> rq & dsa2NextToken ?~ x)
        <$> (rs ^. dsarrNextToken)
