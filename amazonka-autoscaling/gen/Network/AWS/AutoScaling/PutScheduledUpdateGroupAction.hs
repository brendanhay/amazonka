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

-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a scheduled scaling action for an Auto Scaling group.
-- When updating a scheduled scaling action, if you leave a parameter
-- unspecified, the corresponding value remains unchanged in the affected Auto
-- Scaling group. For information on creating or updating a scheduled action
-- for your Auto Scaling group, see Scale Based on a Schedule.
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
    (
    -- * Request
      PutScheduledUpdateGroupActionType
    -- ** Request constructor
    , putScheduledUpdateGroupActionType
    -- ** Request lenses
    , psugatAutoScalingGroupName
    , psugatDesiredCapacity
    , psugatEndTime
    , psugatMaxSize
    , psugatMinSize
    , psugatRecurrence
    , psugatScheduledActionName
    , psugatStartTime
    , psugatTime

    -- * Response
    , PutScheduledUpdateGroupActionResponse
    -- ** Response constructor
    , putScheduledUpdateGroupActionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data PutScheduledUpdateGroupActionType = PutScheduledUpdateGroupActionType
    { _psugatAutoScalingGroupName :: Text
    , _psugatDesiredCapacity      :: Maybe Int
    , _psugatEndTime              :: Maybe RFC822
    , _psugatMaxSize              :: Maybe Int
    , _psugatMinSize              :: Maybe Int
    , _psugatRecurrence           :: Maybe Text
    , _psugatScheduledActionName  :: Text
    , _psugatStartTime            :: Maybe RFC822
    , _psugatTime                 :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'PutScheduledUpdateGroupActionType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'psugatAutoScalingGroupName' @::@ 'Text'
--
-- * 'psugatDesiredCapacity' @::@ 'Maybe' 'Int'
--
-- * 'psugatEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'psugatMaxSize' @::@ 'Maybe' 'Int'
--
-- * 'psugatMinSize' @::@ 'Maybe' 'Int'
--
-- * 'psugatRecurrence' @::@ 'Maybe' 'Text'
--
-- * 'psugatScheduledActionName' @::@ 'Text'
--
-- * 'psugatStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'psugatTime' @::@ 'Maybe' 'UTCTime'
--
putScheduledUpdateGroupActionType :: Text -- ^ 'psugatAutoScalingGroupName'
                                  -> Text -- ^ 'psugatScheduledActionName'
                                  -> PutScheduledUpdateGroupActionType
putScheduledUpdateGroupActionType p1 p2 = PutScheduledUpdateGroupActionType
    { _psugatAutoScalingGroupName = p1
    , _psugatScheduledActionName  = p2
    , _psugatTime                 = Nothing
    , _psugatStartTime            = Nothing
    , _psugatEndTime              = Nothing
    , _psugatRecurrence           = Nothing
    , _psugatMinSize              = Nothing
    , _psugatMaxSize              = Nothing
    , _psugatDesiredCapacity      = Nothing
    }

-- | The name or ARN of the Auto Scaling group.
psugatAutoScalingGroupName :: Lens' PutScheduledUpdateGroupActionType Text
psugatAutoScalingGroupName =
    lens _psugatAutoScalingGroupName
        (\s a -> s { _psugatAutoScalingGroupName = a })

-- | The number of Amazon EC2 instances that should be running in the group.
psugatDesiredCapacity :: Lens' PutScheduledUpdateGroupActionType (Maybe Int)
psugatDesiredCapacity =
    lens _psugatDesiredCapacity (\s a -> s { _psugatDesiredCapacity = a })

-- | The time for this action to end.
psugatEndTime :: Lens' PutScheduledUpdateGroupActionType (Maybe UTCTime)
psugatEndTime = lens _psugatEndTime (\s a -> s { _psugatEndTime = a })
    . mapping _Time

-- | The maximum size for the Auto Scaling group.
psugatMaxSize :: Lens' PutScheduledUpdateGroupActionType (Maybe Int)
psugatMaxSize = lens _psugatMaxSize (\s a -> s { _psugatMaxSize = a })

-- | The minimum size for the new Auto Scaling group.
psugatMinSize :: Lens' PutScheduledUpdateGroupActionType (Maybe Int)
psugatMinSize = lens _psugatMinSize (\s a -> s { _psugatMinSize = a })

-- | The time when recurring future actions will start. Start time is
-- specified by the user following the Unix cron syntax format. For
-- information about cron syntax, go to Wikipedia, The Free Encyclopedia.
-- When StartTime and EndTime are specified with Recurrence, they form the
-- boundaries of when the recurring action will start and stop.
psugatRecurrence :: Lens' PutScheduledUpdateGroupActionType (Maybe Text)
psugatRecurrence = lens _psugatRecurrence (\s a -> s { _psugatRecurrence = a })

-- | The name of this scaling action.
psugatScheduledActionName :: Lens' PutScheduledUpdateGroupActionType Text
psugatScheduledActionName =
    lens _psugatScheduledActionName
        (\s a -> s { _psugatScheduledActionName = a })

-- | The time for this action to start, as in --start-time
-- 2010-06-01T00:00:00Z. If you try to schedule your action in the past,
-- Auto Scaling returns an error message. When StartTime and EndTime are
-- specified with Recurrence, they form the boundaries of when the recurring
-- action will start and stop.
psugatStartTime :: Lens' PutScheduledUpdateGroupActionType (Maybe UTCTime)
psugatStartTime = lens _psugatStartTime (\s a -> s { _psugatStartTime = a })
    . mapping _Time

-- | Time is deprecated. The time for this action to start. Time is an alias
-- for StartTime and can be specified instead of StartTime, or vice versa.
-- If both Time and StartTime are specified, their values should be
-- identical. Otherwise, PutScheduledUpdateGroupAction will return an error.
psugatTime :: Lens' PutScheduledUpdateGroupActionType (Maybe UTCTime)
psugatTime = lens _psugatTime (\s a -> s { _psugatTime = a })
    . mapping _Time

instance ToQuery PutScheduledUpdateGroupActionType

instance ToPath PutScheduledUpdateGroupActionType where
    toPath = const "/"

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutScheduledUpdateGroupActionResponse' constructor.
putScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse
putScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse

instance FromXML PutScheduledUpdateGroupActionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutScheduledUpdateGroupActionResponse"

instance AWSRequest PutScheduledUpdateGroupActionType where
    type Sv PutScheduledUpdateGroupActionType = AutoScaling
    type Rs PutScheduledUpdateGroupActionType = PutScheduledUpdateGroupActionResponse

    request  = post "PutScheduledUpdateGroupAction"
    response = nullaryResponse PutScheduledUpdateGroupActionResponse
