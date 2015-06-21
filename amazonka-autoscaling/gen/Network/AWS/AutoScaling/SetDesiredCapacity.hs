{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Sets the size of the specified Auto Scaling group.
--
-- For more information about desired capacity, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/WhatIsAutoScaling.html What Is Auto Scaling?>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_SetDesiredCapacity.html>
module Network.AWS.AutoScaling.SetDesiredCapacity
    (
    -- * Request
      SetDesiredCapacity
    -- ** Request constructor
    , setDesiredCapacity
    -- ** Request lenses
    , sdcHonorCooldown
    , sdcAutoScalingGroupName
    , sdcDesiredCapacity

    -- * Response
    , SetDesiredCapacityResponse
    -- ** Response constructor
    , setDesiredCapacityResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setDesiredCapacity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcHonorCooldown'
--
-- * 'sdcAutoScalingGroupName'
--
-- * 'sdcDesiredCapacity'
data SetDesiredCapacity = SetDesiredCapacity'{_sdcHonorCooldown :: Maybe Bool, _sdcAutoScalingGroupName :: Text, _sdcDesiredCapacity :: Int} deriving (Eq, Read, Show)

-- | 'SetDesiredCapacity' smart constructor.
setDesiredCapacity :: Text -> Int -> SetDesiredCapacity
setDesiredCapacity pAutoScalingGroupName pDesiredCapacity = SetDesiredCapacity'{_sdcHonorCooldown = Nothing, _sdcAutoScalingGroupName = pAutoScalingGroupName, _sdcDesiredCapacity = pDesiredCapacity};

-- | By default, @SetDesiredCapacity@ overrides any cooldown period
-- associated with the Auto Scaling group. Specify @True@ to make Auto
-- Scaling to wait for the cool-down period associated with the Auto
-- Scaling group to complete before initiating a scaling activity to set
-- your Auto Scaling group to its new capacity.
sdcHonorCooldown :: Lens' SetDesiredCapacity (Maybe Bool)
sdcHonorCooldown = lens _sdcHonorCooldown (\ s a -> s{_sdcHonorCooldown = a});

-- | The name of the Auto Scaling group.
sdcAutoScalingGroupName :: Lens' SetDesiredCapacity Text
sdcAutoScalingGroupName = lens _sdcAutoScalingGroupName (\ s a -> s{_sdcAutoScalingGroupName = a});

-- | The number of EC2 instances that should be running in the Auto Scaling
-- group.
sdcDesiredCapacity :: Lens' SetDesiredCapacity Int
sdcDesiredCapacity = lens _sdcDesiredCapacity (\ s a -> s{_sdcDesiredCapacity = a});

instance AWSRequest SetDesiredCapacity where
        type Sv SetDesiredCapacity = AutoScaling
        type Rs SetDesiredCapacity =
             SetDesiredCapacityResponse
        request = post
        response = receiveNull SetDesiredCapacityResponse'

instance ToHeaders SetDesiredCapacity where
        toHeaders = const mempty

instance ToPath SetDesiredCapacity where
        toPath = const "/"

instance ToQuery SetDesiredCapacity where
        toQuery SetDesiredCapacity'{..}
          = mconcat
              ["Action" =: ("SetDesiredCapacity" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "HonorCooldown" =: _sdcHonorCooldown,
               "AutoScalingGroupName" =: _sdcAutoScalingGroupName,
               "DesiredCapacity" =: _sdcDesiredCapacity]

-- | /See:/ 'setDesiredCapacityResponse' smart constructor.
data SetDesiredCapacityResponse = SetDesiredCapacityResponse' deriving (Eq, Read, Show)

-- | 'SetDesiredCapacityResponse' smart constructor.
setDesiredCapacityResponse :: SetDesiredCapacityResponse
setDesiredCapacityResponse = SetDesiredCapacityResponse';
