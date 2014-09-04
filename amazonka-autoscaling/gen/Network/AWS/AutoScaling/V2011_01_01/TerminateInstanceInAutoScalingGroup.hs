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
    , terminateInstanceInAutoScalingGroup
    -- ** Request lenses
    , tiiasgtShouldDecrementDesiredCapacity
    , tiiasgtInstanceId

    -- * Response
    , TerminateInstanceInAutoScalingGroupResponse
    -- ** Response lenses
    , aaeActivity
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'TerminateInstanceInAutoScalingGroup' request.
terminateInstanceInAutoScalingGroup :: Bool -- ^ 'tiiasgtShouldDecrementDesiredCapacity'
                                    -> Text -- ^ 'tiiasgtInstanceId'
                                    -> TerminateInstanceInAutoScalingGroup
terminateInstanceInAutoScalingGroup p1 p2 = TerminateInstanceInAutoScalingGroup
    { _tiiasgtShouldDecrementDesiredCapacity = p1
    , _tiiasgtInstanceId = p2
    }
{-# INLINE terminateInstanceInAutoScalingGroup #-}

data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { _tiiasgtShouldDecrementDesiredCapacity :: Bool
      -- ^ Specifies whether (true) or not (false) terminating this instance
      -- should also decrement the size of the AutoScalingGroup.
    , _tiiasgtInstanceId :: Text
      -- ^ The ID of the Amazon EC2 instance to be terminated.
    } deriving (Show, Generic)

-- | Specifies whether (true) or not (false) terminating this instance should
-- also decrement the size of the AutoScalingGroup.
tiiasgtShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroup (Bool)
tiiasgtShouldDecrementDesiredCapacity f x =
    f (_tiiasgtShouldDecrementDesiredCapacity x)
        <&> \y -> x { _tiiasgtShouldDecrementDesiredCapacity = y }
{-# INLINE tiiasgtShouldDecrementDesiredCapacity #-}

-- | The ID of the Amazon EC2 instance to be terminated.
tiiasgtInstanceId :: Lens' TerminateInstanceInAutoScalingGroup (Text)
tiiasgtInstanceId f x =
    f (_tiiasgtInstanceId x)
        <&> \y -> x { _tiiasgtInstanceId = y }
{-# INLINE tiiasgtInstanceId #-}

instance ToQuery TerminateInstanceInAutoScalingGroup where
    toQuery = genericQuery def

data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { _aaeActivity :: Maybe Activity
      -- ^ A scaling Activity.
    } deriving (Show, Generic)

-- | A scaling Activity.
aaeActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
aaeActivity f x =
    f (_aaeActivity x)
        <&> \y -> x { _aaeActivity = y }
{-# INLINE aaeActivity #-}

instance FromXML TerminateInstanceInAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateInstanceInAutoScalingGroup where
    type Sv TerminateInstanceInAutoScalingGroup = AutoScaling
    type Rs TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroupResponse

    request = post "TerminateInstanceInAutoScalingGroup"
    response _ = xmlResponse
