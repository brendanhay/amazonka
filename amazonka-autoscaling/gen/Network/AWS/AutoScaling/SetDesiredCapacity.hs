{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the size of the specified Auto Scaling group.
--
--
-- For more information about desired capacity, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/WhatIsAutoScaling.html What Is Auto Scaling?> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.SetDesiredCapacity
    (
    -- * Creating a Request
      setDesiredCapacity
    , SetDesiredCapacity
    -- * Request Lenses
    , sdcHonorCooldown
    , sdcAutoScalingGroupName
    , sdcDesiredCapacity

    -- * Destructuring the Response
    , setDesiredCapacityResponse
    , SetDesiredCapacityResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setDesiredCapacity' smart constructor.
data SetDesiredCapacity = SetDesiredCapacity'
  { _sdcHonorCooldown        :: !(Maybe Bool)
  , _sdcAutoScalingGroupName :: !Text
  , _sdcDesiredCapacity      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDesiredCapacity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcHonorCooldown' - Indicates whether Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Auto Scaling does not honor the cooldown period during manual scaling activities.
--
-- * 'sdcAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'sdcDesiredCapacity' - The number of EC2 instances that should be running in the Auto Scaling group.
setDesiredCapacity
    :: Text -- ^ 'sdcAutoScalingGroupName'
    -> Int -- ^ 'sdcDesiredCapacity'
    -> SetDesiredCapacity
setDesiredCapacity pAutoScalingGroupName_ pDesiredCapacity_ =
  SetDesiredCapacity'
    { _sdcHonorCooldown = Nothing
    , _sdcAutoScalingGroupName = pAutoScalingGroupName_
    , _sdcDesiredCapacity = pDesiredCapacity_
    }


-- | Indicates whether Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Auto Scaling does not honor the cooldown period during manual scaling activities.
sdcHonorCooldown :: Lens' SetDesiredCapacity (Maybe Bool)
sdcHonorCooldown = lens _sdcHonorCooldown (\ s a -> s{_sdcHonorCooldown = a})

-- | The name of the Auto Scaling group.
sdcAutoScalingGroupName :: Lens' SetDesiredCapacity Text
sdcAutoScalingGroupName = lens _sdcAutoScalingGroupName (\ s a -> s{_sdcAutoScalingGroupName = a})

-- | The number of EC2 instances that should be running in the Auto Scaling group.
sdcDesiredCapacity :: Lens' SetDesiredCapacity Int
sdcDesiredCapacity = lens _sdcDesiredCapacity (\ s a -> s{_sdcDesiredCapacity = a})

instance AWSRequest SetDesiredCapacity where
        type Rs SetDesiredCapacity =
             SetDesiredCapacityResponse
        request = postQuery autoScaling
        response = receiveNull SetDesiredCapacityResponse'

instance Hashable SetDesiredCapacity where

instance NFData SetDesiredCapacity where

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
data SetDesiredCapacityResponse =
  SetDesiredCapacityResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetDesiredCapacityResponse' with the minimum fields required to make a request.
--
setDesiredCapacityResponse
    :: SetDesiredCapacityResponse
setDesiredCapacityResponse = SetDesiredCapacityResponse'


instance NFData SetDesiredCapacityResponse where
