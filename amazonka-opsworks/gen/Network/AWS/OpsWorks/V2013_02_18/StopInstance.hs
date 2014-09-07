{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.StopInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops a specified instance. When you stop a standard instance, the data
-- disappears and must be reinstalled when you restart the instance. You can
-- stop an Amazon EBS-backed instance without losing data. For more
-- information, see Starting, Stopping, and Rebooting Instances. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.StopInstance
    (
    -- * Request
      StopInstance
    -- ** Request constructor
    , mkStopInstance
    -- ** Request lenses
    , si1InstanceId

    -- * Response
    , StopInstanceResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype StopInstance = StopInstance
    { _si1InstanceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopInstance' request.
mkStopInstance :: Text -- ^ 'si1InstanceId'
               -> StopInstance
mkStopInstance p1 = StopInstance
    { _si1InstanceId = p1
    }

-- | The instance ID.
si1InstanceId :: Lens' StopInstance Text
si1InstanceId = lens _si1InstanceId (\s a -> s { _si1InstanceId = a })

instance ToPath StopInstance

instance ToQuery StopInstance

instance ToHeaders StopInstance

instance ToJSON StopInstance

data StopInstanceResponse = StopInstanceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest StopInstance where
    type Sv StopInstance = OpsWorks
    type Rs StopInstance = StopInstanceResponse

    request = get
    response _ = nullaryResponse StopInstanceResponse
