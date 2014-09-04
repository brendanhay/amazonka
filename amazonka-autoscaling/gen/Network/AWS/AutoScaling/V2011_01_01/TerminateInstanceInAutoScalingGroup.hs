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
    , mkTerminateInstanceInAutoScalingGroupType
    -- ** Request lenses
    , tiiasgtInstanceId
    , tiiasgtShouldDecrementDesiredCapacity

    -- * Response
    , TerminateInstanceInAutoScalingGroupResponse
    -- ** Response lenses
    , aaeActivity
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TerminateInstanceInAutoScalingGroup' request.
mkTerminateInstanceInAutoScalingGroupType :: Text -- ^ 'tiiasgtInstanceId'
                                          -> Bool -- ^ 'tiiasgtShouldDecrementDesiredCapacity'
                                          -> TerminateInstanceInAutoScalingGroup
mkTerminateInstanceInAutoScalingGroupType p1 p2 = TerminateInstanceInAutoScalingGroup
    { _tiiasgtInstanceId = p1
    , _tiiasgtShouldDecrementDesiredCapacity = p2
    }
{-# INLINE mkTerminateInstanceInAutoScalingGroupType #-}

data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup
    { _tiiasgtInstanceId :: Text
      -- ^ The ID of the Amazon EC2 instance to be terminated.
    , _tiiasgtShouldDecrementDesiredCapacity :: Bool
      -- ^ Specifies whether (true) or not (false) terminating this instance
      -- should also decrement the size of the AutoScalingGroup.
    } deriving (Show, Generic)

-- | The ID of the Amazon EC2 instance to be terminated.
tiiasgtInstanceId :: Lens' TerminateInstanceInAutoScalingGroup (Text)
tiiasgtInstanceId = lens _tiiasgtInstanceId (\s a -> s { _tiiasgtInstanceId = a })
{-# INLINE tiiasgtInstanceId #-}

-- | Specifies whether (true) or not (false) terminating this instance should
-- also decrement the size of the AutoScalingGroup.
tiiasgtShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroup (Bool)
tiiasgtShouldDecrementDesiredCapacity = lens _tiiasgtShouldDecrementDesiredCapacity (\s a -> s { _tiiasgtShouldDecrementDesiredCapacity = a })
{-# INLINE tiiasgtShouldDecrementDesiredCapacity #-}

instance ToQuery TerminateInstanceInAutoScalingGroup where
    toQuery = genericQuery def

newtype TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse
    { _aaeActivity :: Maybe Activity
      -- ^ A scaling Activity.
    } deriving (Show, Generic)

-- | A scaling Activity.
aaeActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
aaeActivity = lens _aaeActivity (\s a -> s { _aaeActivity = a })
{-# INLINE aaeActivity #-}

instance FromXML TerminateInstanceInAutoScalingGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest TerminateInstanceInAutoScalingGroup where
    type Sv TerminateInstanceInAutoScalingGroup = AutoScaling
    type Rs TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroupResponse

    request = post "TerminateInstanceInAutoScalingGroup"
    response _ = xmlResponse
