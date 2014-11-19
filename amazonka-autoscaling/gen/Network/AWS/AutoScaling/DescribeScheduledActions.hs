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
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeScheduledActions.html>
module Network.AWS.AutoScaling.DescribeScheduledActions
    (
    -- * Request
      DescribeScheduledActions
    -- ** Request constructor
    , describeScheduledActions
    -- ** Request lenses
    , dsa1AutoScalingGroupName
    , dsa1EndTime
    , dsa1MaxRecords
    , dsa1NextToken
    , dsa1ScheduledActionNames
    , dsa1StartTime

    -- * Response
    , DescribeScheduledActionsResponse
    -- ** Response constructor
    , describeScheduledActionsResponse
    -- ** Response lenses
    , dsarNextToken
    , dsarScheduledUpdateGroupActions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeScheduledActions = DescribeScheduledActions
    { _dsa1AutoScalingGroupName :: Maybe Text
    , _dsa1EndTime              :: Maybe RFC822
    , _dsa1MaxRecords           :: Maybe Int
    , _dsa1NextToken            :: Maybe Text
    , _dsa1ScheduledActionNames :: List "ScheduledActionNames" Text
    , _dsa1StartTime            :: Maybe RFC822
    } deriving (Eq, Ord, Show)

-- | 'DescribeScheduledActions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsa1AutoScalingGroupName' @::@ 'Maybe' 'Text'
--
-- * 'dsa1EndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dsa1MaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dsa1NextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsa1ScheduledActionNames' @::@ ['Text']
--
-- * 'dsa1StartTime' @::@ 'Maybe' 'UTCTime'
--
describeScheduledActions :: DescribeScheduledActions
describeScheduledActions = DescribeScheduledActions
    { _dsa1AutoScalingGroupName = Nothing
    , _dsa1ScheduledActionNames = mempty
    , _dsa1StartTime            = Nothing
    , _dsa1EndTime              = Nothing
    , _dsa1NextToken            = Nothing
    , _dsa1MaxRecords           = Nothing
    }

-- | The name of the Auto Scaling group.
dsa1AutoScalingGroupName :: Lens' DescribeScheduledActions (Maybe Text)
dsa1AutoScalingGroupName =
    lens _dsa1AutoScalingGroupName
        (\s a -> s { _dsa1AutoScalingGroupName = a })

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this field is ignored.
dsa1EndTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsa1EndTime = lens _dsa1EndTime (\s a -> s { _dsa1EndTime = a }) . mapping _Time

-- | The maximum number of scheduled actions to return.
dsa1MaxRecords :: Lens' DescribeScheduledActions (Maybe Int)
dsa1MaxRecords = lens _dsa1MaxRecords (\s a -> s { _dsa1MaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
dsa1NextToken :: Lens' DescribeScheduledActions (Maybe Text)
dsa1NextToken = lens _dsa1NextToken (\s a -> s { _dsa1NextToken = a })

-- | A list of scheduled actions to be described. If this list is omitted, all
-- scheduled actions are described. The list of requested scheduled actions
-- cannot contain more than 50 items. If an auto scaling group name is
-- provided, the results are limited to that group. If unknown scheduled
-- actions are requested, they are ignored with no error.
dsa1ScheduledActionNames :: Lens' DescribeScheduledActions [Text]
dsa1ScheduledActionNames =
    lens _dsa1ScheduledActionNames
        (\s a -> s { _dsa1ScheduledActionNames = a })
            . _List

-- | The earliest scheduled start time to return. If scheduled action names
-- are provided, this field will be ignored.
dsa1StartTime :: Lens' DescribeScheduledActions (Maybe UTCTime)
dsa1StartTime = lens _dsa1StartTime (\s a -> s { _dsa1StartTime = a }) . mapping _Time

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _dsarNextToken                   :: Maybe Text
    , _dsarScheduledUpdateGroupActions :: List "ScheduledUpdateGroupActions" ScheduledUpdateGroupAction
    } deriving (Eq, Show)

-- | 'DescribeScheduledActionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsarNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dsarScheduledUpdateGroupActions' @::@ ['ScheduledUpdateGroupAction']
--
describeScheduledActionsResponse :: DescribeScheduledActionsResponse
describeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _dsarScheduledUpdateGroupActions = mempty
    , _dsarNextToken                   = Nothing
    }

-- | A string that marks the start of the next batch of returned results.
dsarNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
dsarNextToken = lens _dsarNextToken (\s a -> s { _dsarNextToken = a })

-- | A list of scheduled actions designed to update an Auto Scaling group.
dsarScheduledUpdateGroupActions :: Lens' DescribeScheduledActionsResponse [ScheduledUpdateGroupAction]
dsarScheduledUpdateGroupActions =
    lens _dsarScheduledUpdateGroupActions
        (\s a -> s { _dsarScheduledUpdateGroupActions = a })
            . _List

instance ToPath DescribeScheduledActions where
    toPath = const "/"

instance ToQuery DescribeScheduledActions where
    toQuery DescribeScheduledActions{..} = mconcat
        [ "AutoScalingGroupName" =? _dsa1AutoScalingGroupName
        , "EndTime"              =? _dsa1EndTime
        , "MaxRecords"           =? _dsa1MaxRecords
        , "NextToken"            =? _dsa1NextToken
        , "ScheduledActionNames" =? _dsa1ScheduledActionNames
        , "StartTime"            =? _dsa1StartTime
        ]

instance ToHeaders DescribeScheduledActions

instance AWSRequest DescribeScheduledActions where
    type Sv DescribeScheduledActions = AutoScaling
    type Rs DescribeScheduledActions = DescribeScheduledActionsResponse

    request  = post "DescribeScheduledActions"
    response = xmlResponse

instance FromXML DescribeScheduledActionsResponse where
    parseXML = withElement "DescribeScheduledActionsResult" $ \x -> DescribeScheduledActionsResponse
        <$> x .@? "NextToken"
        <*> x .@  "ScheduledUpdateGroupActions"

instance AWSPager DescribeScheduledActions where
    next rq rs = (\x -> rq & dsa1NextToken ?~ x)
        <$> (rs ^. dsarNextToken)
