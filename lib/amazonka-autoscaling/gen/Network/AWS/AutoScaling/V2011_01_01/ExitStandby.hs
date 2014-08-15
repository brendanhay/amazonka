{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.ExitStandby
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Move an instance out of Standby mode. To learn more about how to put
-- instances that are in a Standby mode back into service, see Auto Scaling
-- InService State.
-- https://autoscaling.amazonaws.com/?InstanceIds.member.1=i-5b73d709&AutoScalingGroupName=my-asg&Version=2011-01-01&Action=ExitStandby&SignatureVersion=2&SignatureMet
-- hod=HmacSHA256&Timestamp=2014-06-13T22%3A43%3A53.182Z&AUTHPARAMS
-- dca4efcf-eea6-4844-8064-cab1fecd1aa2 30 PreInService
-- 2014-06-13T22:43:53.523Z At 2014-06-13T22:43:53Z instance i-5b73d709 was
-- moved out of standby in response to a user request, increasing the capacity
-- from 3 to 4. my-asg {"Availability Zone":"us-east-1a"} Moving EC2 instance
-- out of Standby: i-5b73d709 321a11c8-f34c-11e3-a434-7f10009d5849.
module Network.AWS.AutoScaling.V2011_01_01.ExitStandby where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ExitStandby' request.
exitStandby :: Text -- ^ '_esrAutoScalingGroupName'
            -> ExitStandby
exitStandby p1 = ExitStandby
    { _esrAutoScalingGroupName = p1
    , _esrInstanceIds = mempty
    }

data ExitStandby = ExitStandby
    { _esrAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group from which to move instances
      -- out of Standby mode.
    , _esrInstanceIds :: [Text]
      -- ^ A list of instances to move out of Standby mode. You must specify
      -- at least one instance ID.
    } deriving (Show, Generic)

makeLenses ''ExitStandby

instance ToQuery ExitStandby where
    toQuery = genericQuery def

data ExitStandbyResponse = ExitStandbyResponse
    { _esbActivities :: [Activity]
      -- ^ A list describing the activities related to moving instances out
      -- of Standby mode.
    } deriving (Show, Generic)

makeLenses ''ExitStandbyResponse

instance FromXML ExitStandbyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ExitStandby where
    type Sv ExitStandby = AutoScaling
    type Rs ExitStandby = ExitStandbyResponse

    request = post "ExitStandby"
    response _ = xmlResponse
