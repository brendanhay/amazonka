{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.StopInstance
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
module Network.AWS.OpsWorks.StopInstance
    (
    -- * Request
      StopInstance
    -- ** Request constructor
    , stopInstance
    -- ** Request lenses
    , si1InstanceId

    -- * Response
    , StopInstanceResponse
    -- ** Response constructor
    , stopInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype StopInstance = StopInstance
    { _si1InstanceId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceId ::@ @Text@
--
stopInstance :: Text -- ^ 'si1InstanceId'
             -> StopInstance
stopInstance p1 = StopInstance
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
stopInstanceResponse :: StopInstanceResponse
stopInstanceResponse = StopInstanceResponse

instance AWSRequest StopInstance where
    type Sv StopInstance = OpsWorks
    type Rs StopInstance = StopInstanceResponse

    request = get
    response _ = nullaryResponse StopInstanceResponse
