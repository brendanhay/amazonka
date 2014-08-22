{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.OpsWorks.V2013_02_18.SetTimeBasedAutoScaling where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'SetTimeBasedAutoScaling' request.
setTimeBasedAutoScaling :: Text -- ^ '_stbasrInstanceId'
                        -> SetTimeBasedAutoScaling
setTimeBasedAutoScaling p1 = SetTimeBasedAutoScaling
    { _stbasrInstanceId = p1
    , _stbasrAutoScalingSchedule = Nothing
    }

data SetTimeBasedAutoScaling = SetTimeBasedAutoScaling
    { _stbasrInstanceId :: Text
      -- ^ The instance ID.
    , _stbasrAutoScalingSchedule :: Maybe WeeklyAutoScalingSchedule
      -- ^ An AutoScalingSchedule with the instance schedule.
    } deriving (Show, Generic)

makeLenses ''SetTimeBasedAutoScaling

instance ToPath SetTimeBasedAutoScaling

instance ToQuery SetTimeBasedAutoScaling

instance ToHeaders SetTimeBasedAutoScaling

instance ToJSON SetTimeBasedAutoScaling

data SetTimeBasedAutoScalingResponse = SetTimeBasedAutoScalingResponse
    deriving (Eq, Show, Generic)

makeLenses ''SetTimeBasedAutoScalingResponse

instance AWSRequest SetTimeBasedAutoScaling where
    type Sv SetTimeBasedAutoScaling = OpsWorks
    type Rs SetTimeBasedAutoScaling = SetTimeBasedAutoScalingResponse

    request = get
    response _ = nullaryResponse SetTimeBasedAutoScalingResponse
