{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetDesiredCapacity.html>
module Network.AWS.AutoScaling.SetDesiredCapacity
    (
    -- * Request
      SetDesiredCapacity
    -- ** Request constructor
    , setDesiredCapacity
    -- ** Request lenses
    , sdcAutoScalingGroupName
    , sdcDesiredCapacity
    , sdcHonorCooldown

    -- * Response
    , SetDesiredCapacityResponse
    -- ** Response constructor
    , setDesiredCapacityResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data SetDesiredCapacity = SetDesiredCapacity
    { _sdcAutoScalingGroupName :: Text
    , _sdcDesiredCapacity      :: Int
    , _sdcHonorCooldown        :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'SetDesiredCapacity' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcAutoScalingGroupName' @::@ 'Text'
--
-- * 'sdcDesiredCapacity' @::@ 'Int'
--
-- * 'sdcHonorCooldown' @::@ 'Maybe' 'Bool'
--
setDesiredCapacity :: Text -- ^ 'sdcAutoScalingGroupName'
                   -> Int -- ^ 'sdcDesiredCapacity'
                   -> SetDesiredCapacity
setDesiredCapacity p1 p2 = SetDesiredCapacity
    { _sdcAutoScalingGroupName = p1
    , _sdcDesiredCapacity      = p2
    , _sdcHonorCooldown        = Nothing
    }

-- | The name of the Auto Scaling group.
sdcAutoScalingGroupName :: Lens' SetDesiredCapacity Text
sdcAutoScalingGroupName =
    lens _sdcAutoScalingGroupName (\s a -> s { _sdcAutoScalingGroupName = a })

-- | The new capacity setting for the Auto Scaling group.
sdcDesiredCapacity :: Lens' SetDesiredCapacity Int
sdcDesiredCapacity =
    lens _sdcDesiredCapacity (\s a -> s { _sdcDesiredCapacity = a })

-- | By default, SetDesiredCapacity overrides any cooldown period associated
-- with the Auto Scaling group. Set to True if you want Auto Scaling to wait
-- for the cooldown period associated with the Auto Scaling group to
-- complete before initiating a scaling activity to set your Auto Scaling
-- group to the new capacity setting.
sdcHonorCooldown :: Lens' SetDesiredCapacity (Maybe Bool)
sdcHonorCooldown = lens _sdcHonorCooldown (\s a -> s { _sdcHonorCooldown = a })

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetDesiredCapacityResponse' constructor.
setDesiredCapacityResponse :: SetDesiredCapacityResponse
setDesiredCapacityResponse = SetDesiredCapacityResponse

instance ToPath SetDesiredCapacity where
    toPath = const "/"

instance ToQuery SetDesiredCapacity where
    toQuery SetDesiredCapacity{..} = mconcat
        [ "AutoScalingGroupName" =? _sdcAutoScalingGroupName
        , "DesiredCapacity"      =? _sdcDesiredCapacity
        , "HonorCooldown"        =? _sdcHonorCooldown
        ]

instance ToHeaders SetDesiredCapacity

instance AWSRequest SetDesiredCapacity where
    type Sv SetDesiredCapacity = AutoScaling
    type Rs SetDesiredCapacity = SetDesiredCapacityResponse

    request  = post "SetDesiredCapacity"
    response = nullResponse SetDesiredCapacityResponse


Some kind of operator / class to check the types whether to continue?
