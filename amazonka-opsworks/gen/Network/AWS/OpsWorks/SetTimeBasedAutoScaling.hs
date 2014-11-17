{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.SetTimeBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specify the time-based auto scaling configuration for a specified instance.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. Required Permissions: To use this action, an IAM user must have
-- a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.SetTimeBasedAutoScaling
    (
    -- * Request
      SetTimeBasedAutoScaling
    -- ** Request constructor
    , setTimeBasedAutoScaling
    -- ** Request lenses
    , stbasAutoScalingSchedule
    , stbasInstanceId

    -- * Response
    , SetTimeBasedAutoScalingResponse
    -- ** Response constructor
    , setTimeBasedAutoScalingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling
    { _stbasAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
    , _stbasInstanceId          :: Text
    } deriving (Eq, Show, Generic)

-- | 'SetTimeBasedAutoScaling' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stbasAutoScalingSchedule' @::@ 'Maybe' 'WeeklyAutoScalingSchedule'
--
-- * 'stbasInstanceId' @::@ 'Text'
--
setTimeBasedAutoScaling :: Text -- ^ 'stbasInstanceId'
                        -> SetTimeBasedAutoScaling
setTimeBasedAutoScaling p1 = SetTimeBasedAutoScaling
    { _stbasInstanceId          = p1
    , _stbasAutoScalingSchedule = Nothing
    }

-- | An AutoScalingSchedule with the instance schedule.
stbasAutoScalingSchedule :: Lens' SetTimeBasedAutoScaling (Maybe WeeklyAutoScalingSchedule)
stbasAutoScalingSchedule =
    lens _stbasAutoScalingSchedule
        (\s a -> s { _stbasAutoScalingSchedule = a })

-- | The instance ID.
stbasInstanceId :: Lens' SetTimeBasedAutoScaling Text
stbasInstanceId = lens _stbasInstanceId (\s a -> s { _stbasInstanceId = a })

data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetTimeBasedAutoScalingResponse' constructor.
setTimeBasedAutoScalingResponse :: SetTimeBasedAutoScalingResponse
setTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse

instance AWSRequest SetTimeBasedAutoScaling where
    type Sv SetTimeBasedAutoScaling = OpsWorks
    type Rs SetTimeBasedAutoScaling = SetTimeBasedAutoScalingResponse

    request  = post
    response = nullResponse SetTimeBasedAutoScalingResponse

instance ToPath SetTimeBasedAutoScaling where
    toPath = const "/"

instance ToHeaders SetTimeBasedAutoScaling

instance ToQuery SetTimeBasedAutoScaling where
    toQuery = const mempty

instance ToJSON SetTimeBasedAutoScaling where
    toJSON = genericToJSON jsonOptions
