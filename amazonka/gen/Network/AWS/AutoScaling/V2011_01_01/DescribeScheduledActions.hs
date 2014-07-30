{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.AutoScaling.V2011_01_01.DescribeScheduledActions where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.AutoScaling.V2011_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeScheduledActions' request.
describeScheduledActions :: DescribeScheduledActions
describeScheduledActions = DescribeScheduledActions
    { _dsauMaxRecords = Nothing
    , _dsauAutoScalingGroupName = Nothing
    , _dsauScheduledActionNames = mempty
    , _dsauStartTime = Nothing
    , _dsauEndTime = Nothing
    , _dsauNextToken = Nothing
    }

data DescribeScheduledActions = DescribeScheduledActions
    { _dsauMaxRecords :: Maybe Integer
      -- ^ The maximum number of scheduled actions to return.
    , _dsauAutoScalingGroupName :: Maybe Text
      -- ^ The name of the Auto Scaling group.
    , _dsauScheduledActionNames :: [Text]
      -- ^ A list of scheduled actions to be described. If this list is
      -- omitted, all scheduled actions are described. The list of
      -- requested scheduled actions cannot contain more than 50 items. If
      -- an auto scaling group name is provided, the results are limited
      -- to that group. If unknown scheduled actions are requested, they
      -- are ignored with no error.
    , _dsauStartTime :: Maybe ISO8601
      -- ^ The earliest scheduled start time to return. If scheduled action
      -- names are provided, this field will be ignored.
    , _dsauEndTime :: Maybe ISO8601
      -- ^ The latest scheduled start time to return. If scheduled action
      -- names are provided, this field is ignored.
    , _dsauNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Generic)

instance ToQuery DescribeScheduledActions where
    toQuery = genericToQuery def

instance AWSRequest DescribeScheduledActions where
    type Sv DescribeScheduledActions = AutoScaling
    type Rs DescribeScheduledActions = DescribeScheduledActionsResponse

    request = post "DescribeScheduledActions"
    response _ = xmlResponse

instance AWSPager DescribeScheduledActions where
    next rq rs = (\x -> rq { _dsauNextToken = Just x })
        <$> _satNextToken rs

data DescribeScheduledActionsResponse = DescribeScheduledActionsResponse
    { _satScheduledUpdateGroupActions :: [ScheduledUpdateGroupAction]
      -- ^ A list of scheduled actions designed to update an Auto Scaling
      -- group.
    , _satNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Generic)

instance FromXML DescribeScheduledActionsResponse where
    fromXMLOptions = xmlOptions
