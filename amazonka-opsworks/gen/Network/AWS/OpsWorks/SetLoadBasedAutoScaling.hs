{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
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

-- | Specify the load-based auto scaling configuration for a specified layer.
-- For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances>.
--
-- To use load-based auto scaling, you must create a set of load-based auto
-- scaling instances. Load-based auto scaling operates only on the
-- instances from that set, so you must ensure that you have created enough
-- instances to handle the maximum anticipated load.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_SetLoadBasedAutoScaling.html>
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
    (
    -- * Request
      SetLoadBasedAutoScaling
    -- ** Request constructor
    , setLoadBasedAutoScaling
    -- ** Request lenses
    , slbasUpScaling
    , slbasEnable
    , slbasDownScaling
    , slbasLayerId

    -- * Response
    , SetLoadBasedAutoScalingResponse
    -- ** Response constructor
    , setLoadBasedAutoScalingResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setLoadBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbasUpScaling'
--
-- * 'slbasEnable'
--
-- * 'slbasDownScaling'
--
-- * 'slbasLayerId'
data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling'{_slbasUpScaling :: Maybe AutoScalingThresholds, _slbasEnable :: Maybe Bool, _slbasDownScaling :: Maybe AutoScalingThresholds, _slbasLayerId :: Text} deriving (Eq, Read, Show)

-- | 'SetLoadBasedAutoScaling' smart constructor.
setLoadBasedAutoScaling :: Text -> SetLoadBasedAutoScaling
setLoadBasedAutoScaling pLayerId = SetLoadBasedAutoScaling'{_slbasUpScaling = Nothing, _slbasEnable = Nothing, _slbasDownScaling = Nothing, _slbasLayerId = pLayerId};

-- | An @AutoScalingThresholds@ object with the upscaling threshold
-- configuration. If the load exceeds these thresholds for a specified
-- amount of time, AWS OpsWorks starts a specified number of instances.
slbasUpScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasUpScaling = lens _slbasUpScaling (\ s a -> s{_slbasUpScaling = a});

-- | Enables load-based auto scaling for the layer.
slbasEnable :: Lens' SetLoadBasedAutoScaling (Maybe Bool)
slbasEnable = lens _slbasEnable (\ s a -> s{_slbasEnable = a});

-- | An @AutoScalingThresholds@ object with the downscaling threshold
-- configuration. If the load falls below these thresholds for a specified
-- amount of time, AWS OpsWorks stops a specified number of instances.
slbasDownScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasDownScaling = lens _slbasDownScaling (\ s a -> s{_slbasDownScaling = a});

-- | The layer ID.
slbasLayerId :: Lens' SetLoadBasedAutoScaling Text
slbasLayerId = lens _slbasLayerId (\ s a -> s{_slbasLayerId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest SetLoadBasedAutoScaling where
        type Sv SetLoadBasedAutoScaling = OpsWorks
        type Rs SetLoadBasedAutoScaling =
             SetLoadBasedAutoScalingResponse
        request = postJSON
        response
          = receiveNull SetLoadBasedAutoScalingResponse'

instance ToHeaders SetLoadBasedAutoScaling where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.SetLoadBasedAutoScaling" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetLoadBasedAutoScaling where
        toJSON SetLoadBasedAutoScaling'{..}
          = object
              ["UpScaling" .= _slbasUpScaling,
               "Enable" .= _slbasEnable,
               "DownScaling" .= _slbasDownScaling,
               "LayerId" .= _slbasLayerId]

instance ToPath SetLoadBasedAutoScaling where
        toPath = const "/"

instance ToQuery SetLoadBasedAutoScaling where
        toQuery = const mempty

-- | /See:/ 'setLoadBasedAutoScalingResponse' smart constructor.
data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse' deriving (Eq, Read, Show)

-- | 'SetLoadBasedAutoScalingResponse' smart constructor.
setLoadBasedAutoScalingResponse :: SetLoadBasedAutoScalingResponse
setLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse';
