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
      DescribeScheduledActionsType
    -- ** Request constructor
    , describeScheduledActionsType
    -- ** Request lenses
    , dsat2AutoScalingGroupName
    , dsat2EndTime
    , dsat2MaxRecords
    , dsat2NextToken
    , dsat2ScheduledActionNames
    , dsat2StartTime

    -- * Response
    , ScheduledActionsType
    -- ** Response constructor
    , scheduledActionsType
    -- ** Response lenses
    , satNextToken
    , satScheduledUpdateGroupActions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeScheduledActionsType = DescribeScheduledActionsType
    { _dsat2AutoScalingGroupName :: Maybe Text
    , _dsat2EndTime              :: Maybe RFC822
    , _dsat2MaxRecords           :: Maybe Int
    , _dsat2NextToken            :: Maybe Text
    , _dsat2ScheduledActionNames :: [Text]
    , _dsat2StartTime            :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeScheduledActionsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsat2AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dsat2EndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dsat2MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dsat2NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsat2ScheduledActionNames' @::@ ['Text']
--
-- * 'dsat2StartTime' @::@ 'Maybe' 'UTCTime'
--
describeScheduledActionsType :: DescribeScheduledActionsType
describeScheduledActionsType = DescribeScheduledActionsType
    { _dsat2AutoScalingGroupName = Nothing
    , _dsat2ScheduledActionNames = mempty
    , _dsat2StartTime            = Nothing
    , _dsat2EndTime              = Nothing
    , _dsat2NextToken            = Nothing
    , _dsat2MaxRecords           = Nothing
    }

-- | The name of the Auto Scaling group.
dsat2AutoScalingGroupName :: Lens' DescribeScheduledActionsType (Maybe Text)
dsat2AutoScalingGroupName =
    lens _dsat2AutoScalingGroupName
        (\s a -> s { _dsat2AutoScalingGroupName = a })

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this field is ignored.
dsat2EndTime :: Lens' DescribeScheduledActionsType (Maybe UTCTime)
dsat2EndTime = lens _dsat2EndTime (\s a -> s { _dsat2EndTime = a })
    . mapping _Time

-- | The maximum number of scheduled actions to return.
dsat2MaxRecords :: Lens' DescribeScheduledActionsType (Maybe Int)
dsat2MaxRecords = lens _dsat2MaxRecords (\s a -> s { _dsat2MaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
dsat2NextToken :: Lens' DescribeScheduledActionsType (Maybe Text)
dsat2NextToken = lens _dsat2NextToken (\s a -> s { _dsat2NextToken = a })

-- | A list of scheduled actions to be described. If this list is omitted, all
-- scheduled actions are described. The list of requested scheduled actions
-- cannot contain more than 50 items. If an auto scaling group name is
-- provided, the results are limited to that group. If unknown scheduled
-- actions are requested, they are ignored with no error.
dsat2ScheduledActionNames :: Lens' DescribeScheduledActionsType [Text]
dsat2ScheduledActionNames =
    lens _dsat2ScheduledActionNames
        (\s a -> s { _dsat2ScheduledActionNames = a })

-- | The earliest scheduled start time to return. If scheduled action names
-- are provided, this field will be ignored.
dsat2StartTime :: Lens' DescribeScheduledActionsType (Maybe UTCTime)
dsat2StartTime = lens _dsat2StartTime (\s a -> s { _dsat2StartTime = a })
    . mapping _Time
instance ToQuery DescribeScheduledActionsType

instance ToPath DescribeScheduledActionsType where
    toPath = const "/"

data ScheduledActionsType = ScheduledActionsType
    { _satNextToken                   :: Maybe Text
    , _satScheduledUpdateGroupActions :: [ScheduledUpdateGroupAction]
    } deriving (Eq, Show, Generic)

-- | 'ScheduledActionsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'satNextToken' @::@ 'Maybe' 'Text'
--
-- * 'satScheduledUpdateGroupActions' @::@ ['ScheduledUpdateGroupAction']
--
scheduledActionsType :: ScheduledActionsType
scheduledActionsType = ScheduledActionsType
    { _satScheduledUpdateGroupActions = mempty
    , _satNextToken                   = Nothing
    }

-- | A string that marks the start of the next batch of returned results.
satNextToken :: Lens' ScheduledActionsType (Maybe Text)
satNextToken = lens _satNextToken (\s a -> s { _satNextToken = a })

-- | A list of scheduled actions designed to update an Auto Scaling group.
satScheduledUpdateGroupActions :: Lens' ScheduledActionsType [ScheduledUpdateGroupAction]
satScheduledUpdateGroupActions =
    lens _satScheduledUpdateGroupActions
        (\s a -> s { _satScheduledUpdateGroupActions = a })
instance FromXML ScheduledActionsType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ScheduledActionsType"

instance AWSRequest DescribeScheduledActionsType where
    type Sv DescribeScheduledActionsType = AutoScaling
    type Rs DescribeScheduledActionsType = ScheduledActionsType

    request  = post "DescribeScheduledActions"
    response = xmlResponse $ \h x -> ScheduledActionsType
        <$> x %| "NextToken"
        <*> x %| "ScheduledUpdateGroupActions"
