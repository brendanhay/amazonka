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
    , mkStopInstanceRequest
    -- ** Request lenses
    , sisInstanceId

    -- * Response
    , StopInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopInstance' request.
mkStopInstanceRequest :: Text -- ^ 'sisInstanceId'
                      -> StopInstance
mkStopInstanceRequest p1 = StopInstance
    { _sisInstanceId = p1
    }
{-# INLINE mkStopInstanceRequest #-}

newtype StopInstance = StopInstance
    { _sisInstanceId :: Text
      -- ^ The instance ID.
    } deriving (Show, Generic)

-- | The instance ID.
sisInstanceId :: Lens' StopInstance (Text)
sisInstanceId = lens _sisInstanceId (\s a -> s { _sisInstanceId = a })
{-# INLINE sisInstanceId #-}

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
