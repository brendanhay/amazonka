{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the desired size of the specified AutoScalingGroup.
module Network.AWS.AutoScaling.SetDesiredCapacity
    (
    -- * Request
      SetDesiredCapacityType
    -- ** Request constructor
    , setDesiredCapacity
    -- ** Request lenses
    , sdctAutoScalingGroupName
    , sdctDesiredCapacity
    , sdctHonorCooldown

    -- * Response
    , SetDesiredCapacityResponse
    -- ** Response constructor
    , setDesiredCapacityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data SetDesiredCapacityType = SetDesiredCapacityType
    { _sdctAutoScalingGroupName :: Text
    , _sdctDesiredCapacity      :: Int
    , _sdctHonorCooldown        :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetDesiredCapacityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdctAutoScalingGroupName' @::@ 'Text'
--
-- * 'sdctDesiredCapacity' @::@ 'Int'
--
-- * 'sdctHonorCooldown' @::@ 'Maybe' 'Bool'
--
setDesiredCapacity :: Text -- ^ 'sdctAutoScalingGroupName'
                   -> Int -- ^ 'sdctDesiredCapacity'
                   -> SetDesiredCapacityType
setDesiredCapacity p1 p2 = SetDesiredCapacityType
    { _sdctAutoScalingGroupName = p1
    , _sdctDesiredCapacity      = p2
    , _sdctHonorCooldown        = Nothing
    }

-- | The name of the Auto Scaling group.
sdctAutoScalingGroupName :: Lens' SetDesiredCapacityType Text
sdctAutoScalingGroupName =
    lens _sdctAutoScalingGroupName
        (\s a -> s { _sdctAutoScalingGroupName = a })

-- | The new capacity setting for the Auto Scaling group.
sdctDesiredCapacity :: Lens' SetDesiredCapacityType Int
sdctDesiredCapacity =
    lens _sdctDesiredCapacity (\s a -> s { _sdctDesiredCapacity = a })

-- | By default, SetDesiredCapacity overrides any cooldown period associated
-- with the Auto Scaling group. Set to True if you want Auto Scaling to wait
-- for the cooldown period associated with the Auto Scaling group to
-- complete before initiating a scaling activity to set your Auto Scaling
-- group to the new capacity setting.
sdctHonorCooldown :: Lens' SetDesiredCapacityType (Maybe Bool)
sdctHonorCooldown =
    lens _sdctHonorCooldown (\s a -> s { _sdctHonorCooldown = a })

instance ToPath SetDesiredCapacityType where
    toPath = const "/"

instance ToQuery SetDesiredCapacityType

data SetDesiredCapacityResponse = SetDesiredCapacityResponse

-- | 'SetDesiredCapacityResponse' constructor.
setDesiredCapacityResponse :: SetDesiredCapacityResponse
setDesiredCapacityResponse = SetDesiredCapacityResponse

instance AWSRequest SetDesiredCapacityType where
    type Sv SetDesiredCapacityType = AutoScaling
    type Rs SetDesiredCapacityType = SetDesiredCapacityResponse

    request  = post "SetDesiredCapacity"
    response = const (nullaryResponse SetDesiredCapacityResponse)
