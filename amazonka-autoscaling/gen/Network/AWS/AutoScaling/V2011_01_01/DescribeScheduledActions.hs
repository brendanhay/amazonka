{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions
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
module Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions
    (
    -- * Request
      DescribeScheduledActions
    -- ** Request constructor
    , mkDescribeScheduledActionsType
    -- ** Request lenses
    , dsavAutoScalingGroupName
    , dsavScheduledActionNames
    , dsavStartTime
    , dsavEndTime
    , dsavNextToken
    , dsavMaxRecords

    -- * Response
    , DescribeScheduledActionsResponse
    -- ** Response lenses
    , satScheduledUpdateGroupActions
    , satNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeScheduledActions' request.
mkDescribeScheduledActionsType :: DescribeScheduledActions
mkDescribeScheduledActionsType = DescribeScheduledActions
    { _dsavAutoScalingGroupName = Nothing
    , _dsavScheduledActionNames = mempty
    , _dsavStartTime = Nothing
    , _dsavEndTime = Nothing
    , _dsavNextToken = Nothing
    , _dsavMaxRecords = Nothing
    }
{-# INLINE mkDescribeScheduledActionsType #-}

data DescribeScheduledActions = DescribeScheduledActions
    { _dsavAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dsavScheduledActionNames :: [Text]
      -- ^ A list of scheduled actions to be described. If this list is
      -- omitted, all scheduled actions are described. The list of
      -- requested scheduled actions cannot contain more than 50 items. If
      -- an auto scaling group name is provided, the results are limited
      -- to that group. If unknown scheduled actions are requested, they
      -- are ignored with no error.
    , _dsavStartTime :: Maybe ISO8601
      -- ^ The earliest scheduled start time to return. If scheduled action
      -- names are provided, this field will be ignored.
    , _dsavEndTime :: Maybe ISO8601
      -- ^ The latest scheduled start time to return. If scheduled action
      -- names are provided, this field is ignored.
    , _dsavNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    , _dsavMaxRecords :: Maybe Integer
      -- ^ The maximum number of scheduled actions to return.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
dsavAutoScalingGroupName :: Lens' DescribeScheduledActions (Maybe Text)
dsavAutoScalingGroupName = lens _dsavAutoScalingGroupName (\s a -> s { _dsavAutoScalingGroupName = a })
{-# INLINE dsavAutoScalingGroupName #-}

-- | A list of scheduled actions to be described. If this list is omitted, all
-- scheduled actions are described. The list of requested scheduled actions
-- cannot contain more than 50 items. If an auto scaling group name is
-- provided, the results are limited to that group. If unknown scheduled
-- actions are requested, they are ignored with no error.
dsavScheduledActionNames :: Lens' DescribeScheduledActions ([Text])
dsavScheduledActionNames = lens _dsavScheduledActionNames (\s a -> s { _dsavScheduledActionNames = a })
{-# INLINE dsavScheduledActionNames #-}

-- | The earliest scheduled start time to return. If scheduled action names are
-- provided, this field will be ignored.
dsavStartTime :: Lens' DescribeScheduledActions (Maybe ISO8601)
dsavStartTime = lens _dsavStartTime (\s a -> s { _dsavStartTime = a })
{-# INLINE dsavStartTime #-}

-- | The latest scheduled start time to return. If scheduled action names are
-- provided, this field is ignored.
dsavEndTime :: Lens' DescribeScheduledActions (Maybe ISO8601)
dsavEndTime = lens _dsavEndTime (\s a -> s { _dsavEndTime = a })
{-# INLINE dsavEndTime #-}

-- | A string that marks the start of the next batch of returned results.
dsavNextToken :: Lens' DescribeScheduledActions (Maybe Text)
dsavNextToken = lens _dsavNextToken (\s a -> s { _dsavNextToken = a })
{-# INLINE dsavNextToken #-}

-- | The maximum number of scheduled actions to return.
dsavMaxRecords :: Lens' DescribeScheduledActions (Maybe Integer)
dsavMaxRecords = lens _dsavMaxRecords (\s a -> s { _dsavMaxRecords = a })
{-# INLINE dsavMaxRecords #-}

instance ToQuery DescribeScheduledActions where
    toQuery = genericQuery def

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _satScheduledUpdateGroupActions :: [ScheduledUpdateGroupAction]
      -- ^ A list of scheduled actions designed to update an Auto Scaling
      -- group.
    , _satNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Show, Generic)

-- | A list of scheduled actions designed to update an Auto Scaling group.
satScheduledUpdateGroupActions :: Lens' DescribeScheduledActionsResponse ([ScheduledUpdateGroupAction])
satScheduledUpdateGroupActions = lens _satScheduledUpdateGroupActions (\s a -> s { _satScheduledUpdateGroupActions = a })
{-# INLINE satScheduledUpdateGroupActions #-}

-- | A string that marks the start of the next batch of returned results.
satNextToken :: Lens' DescribeScheduledActionsResponse (Maybe Text)
satNextToken = lens _satNextToken (\s a -> s { _satNextToken = a })
{-# INLINE satNextToken #-}

instance FromXML DescribeScheduledActionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeScheduledActions where
    type Sv DescribeScheduledActions = AutoScaling
    type Rs DescribeScheduledActions = DescribeScheduledActionsResponse

    request = post "DescribeScheduledActions"
    response _ = xmlResponse

instance AWSPager DescribeScheduledActions where
    next rq rs = (\x -> rq { _dsavNextToken = Just x })
        <$> (_satNextToken rs)
