{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified instance. Optionally, the desired group size can
-- be adjusted. This call simply registers a termination request. The
-- termination of the instance cannot happen immediately.
module Network.AWS.AutoScaling.V2011_01_01.TerminateInstanceInAutoScalingGroup
    (
    -- * Request
      TerminateInstanceInAutoScalingGroup
    -- ** Request constructor
    , mkTerminateInstanceInAutoScalingGroup
    -- ** Request lenses
    , tiiasgInstanceId
    , tiiasgShouldDecrementDesiredCapacity

    -- * Response
    , TerminateInstanceInAutoScalingGroupResponse
    -- ** Response lenses
    , tiiasgrsActivity
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | 
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { _tiiasgInstanceId :: Text
    , _tiiasgShouldDecrementDesiredCapacity :: Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateInstanceInAutoScalingGroup' request.
mkTerminateInstanceInAutoScalingGroup :: Text -- ^ 'tiiasgInstanceId'
                                      -> Bool -- ^ 'tiiasgShouldDecrementDesiredCapacity'
                                      -> TerminateInstanceInAutoScalingGroup
mkTerminateInstanceInAutoScalingGroup p1 p2 = TerminateInstanceInAutoScalingGroup
    { _tiiasgInstanceId = p1
    , _tiiasgShouldDecrementDesiredCapacity = p2
    }

-- | The ID of the Amazon EC2 instance to be terminated.
tiiasgInstanceId :: Lens' TerminateInstanceInAutoScalingGroup Text
tiiasgInstanceId =
    lens _tiiasgInstanceId (\s a -> s { _tiiasgInstanceId = a })

-- | Specifies whether (true) or not (false) terminating this instance should
-- also decrement the size of the AutoScalingGroup.
tiiasgShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroup Bool
tiiasgShouldDecrementDesiredCapacity =
    lens _tiiasgShouldDecrementDesiredCapacity
         (\s a -> s { _tiiasgShouldDecrementDesiredCapacity = a })

instance ToQuery TerminateInstanceInAutoScalingGroup where
    toQuery = genericQuery def

-- | The output for the TerminateInstanceInAutoScalingGroup action.
newtype TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { _tiiasgrsActivity :: Maybe Activity
    } deriving (Show, Generic)

-- | A scaling Activity.
tiiasgrsActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
tiiasgrsActivity =
    lens _tiiasgrsActivity (\s a -> s { _tiiasgrsActivity = a })

instance FromXML TerminateInstanceInAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateInstanceInAutoScalingGroup where
    type Sv TerminateInstanceInAutoScalingGroup = AutoScaling
    type Rs TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroupResponse

    request = post "TerminateInstanceInAutoScalingGroup"
    response _ = xmlResponse
