{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.SetTimeBasedAutoScaling
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
module Network.AWS.OpsWorks.V2013_02_18.SetTimeBasedAutoScaling
    (
    -- * Request
      SetTimeBasedAutoScaling
    -- ** Request constructor
    , mkSetTimeBasedAutoScaling
    -- ** Request lenses
    , stbasInstanceId
    , stbasAutoScalingSchedule

    -- * Response
    , SetTimeBasedAutoScalingResponse
    -- ** Response constructor
    , mkSetTimeBasedAutoScalingResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling
    { _stbasInstanceId :: Text
    , _stbasAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetTimeBasedAutoScaling' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
-- * @AutoScalingSchedule ::@ @Maybe WeeklyAutoScalingSchedule@
--
mkSetTimeBasedAutoScaling :: Text -- ^ 'stbasInstanceId'
                          -> SetTimeBasedAutoScaling
mkSetTimeBasedAutoScaling p1 = SetTimeBasedAutoScaling
    { _stbasInstanceId = p1
    , _stbasAutoScalingSchedule = Nothing
    }

-- | The instance ID.
stbasInstanceId :: Lens' SetTimeBasedAutoScaling Text
stbasInstanceId = lens _stbasInstanceId (\s a -> s { _stbasInstanceId = a })

-- | An AutoScalingSchedule with the instance schedule.
stbasAutoScalingSchedule :: Lens' SetTimeBasedAutoScaling (Maybe WeeklyAutoScalingSchedule)
stbasAutoScalingSchedule =
    lens _stbasAutoScalingSchedule
         (\s a -> s { _stbasAutoScalingSchedule = a })

instance ToPath SetTimeBasedAutoScaling

instance ToQuery SetTimeBasedAutoScaling

instance ToHeaders SetTimeBasedAutoScaling

instance ToJSON SetTimeBasedAutoScaling

data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetTimeBasedAutoScalingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetTimeBasedAutoScalingResponse :: SetTimeBasedAutoScalingResponse
mkSetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse

instance AWSRequest SetTimeBasedAutoScaling where
    type Sv SetTimeBasedAutoScaling = OpsWorks
    type Rs SetTimeBasedAutoScaling = SetTimeBasedAutoScalingResponse

    request = get
    response _ = nullaryResponse SetTimeBasedAutoScalingResponse
