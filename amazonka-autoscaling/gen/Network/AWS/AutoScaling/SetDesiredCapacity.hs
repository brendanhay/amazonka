{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets the size of the specified Auto Scaling group.
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
    , sdcrqHonorCooldown
    , sdcrqAutoScalingGroupName
    , sdcrqDesiredCapacity

    -- * Response
    , SetDesiredCapacityResponse
    -- ** Response constructor
    , setDesiredCapacityResponse
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setDesiredCapacity' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdcrqHonorCooldown'
--
-- * 'sdcrqAutoScalingGroupName'
--
-- * 'sdcrqDesiredCapacity'
data SetDesiredCapacity = SetDesiredCapacity'
    { _sdcrqHonorCooldown        :: !(Maybe Bool)
    , _sdcrqAutoScalingGroupName :: !Text
    , _sdcrqDesiredCapacity      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetDesiredCapacity' smart constructor.
setDesiredCapacity :: Text -> Int -> SetDesiredCapacity
setDesiredCapacity pAutoScalingGroupName_ pDesiredCapacity_ =
    SetDesiredCapacity'
    { _sdcrqHonorCooldown = Nothing
    , _sdcrqAutoScalingGroupName = pAutoScalingGroupName_
    , _sdcrqDesiredCapacity = pDesiredCapacity_
    }

-- | By default, @SetDesiredCapacity@ overrides any cooldown period
-- associated with the Auto Scaling group. Specify @True@ to make Auto
-- Scaling to wait for the cool-down period associated with the Auto
-- Scaling group to complete before initiating a scaling activity to set
-- your Auto Scaling group to its new capacity.
sdcrqHonorCooldown :: Lens' SetDesiredCapacity (Maybe Bool)
sdcrqHonorCooldown = lens _sdcrqHonorCooldown (\ s a -> s{_sdcrqHonorCooldown = a});

-- | The name of the Auto Scaling group.
sdcrqAutoScalingGroupName :: Lens' SetDesiredCapacity Text
sdcrqAutoScalingGroupName = lens _sdcrqAutoScalingGroupName (\ s a -> s{_sdcrqAutoScalingGroupName = a});

-- | The number of EC2 instances that should be running in the Auto Scaling
-- group.
sdcrqDesiredCapacity :: Lens' SetDesiredCapacity Int
sdcrqDesiredCapacity = lens _sdcrqDesiredCapacity (\ s a -> s{_sdcrqDesiredCapacity = a});

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
               "HonorCooldown" =: _sdcrqHonorCooldown,
               "AutoScalingGroupName" =: _sdcrqAutoScalingGroupName,
               "DesiredCapacity" =: _sdcrqDesiredCapacity]

-- | /See:/ 'setDesiredCapacityResponse' smart constructor.
data SetDesiredCapacityResponse =
    SetDesiredCapacityResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetDesiredCapacityResponse' smart constructor.
setDesiredCapacityResponse :: SetDesiredCapacityResponse
setDesiredCapacityResponse = SetDesiredCapacityResponse'
