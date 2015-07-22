{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Specify the load-based auto scaling configuration for a specified layer.
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
    , slbasrqUpScaling
    , slbasrqEnable
    , slbasrqDownScaling
    , slbasrqLayerId

    -- * Response
    , SetLoadBasedAutoScalingResponse
    -- ** Response constructor
    , setLoadBasedAutoScalingResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setLoadBasedAutoScaling' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slbasrqUpScaling'
--
-- * 'slbasrqEnable'
--
-- * 'slbasrqDownScaling'
--
-- * 'slbasrqLayerId'
data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling'
    { _slbasrqUpScaling   :: !(Maybe AutoScalingThresholds)
    , _slbasrqEnable      :: !(Maybe Bool)
    , _slbasrqDownScaling :: !(Maybe AutoScalingThresholds)
    , _slbasrqLayerId     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBasedAutoScaling' smart constructor.
setLoadBasedAutoScaling :: Text -> SetLoadBasedAutoScaling
setLoadBasedAutoScaling pLayerId =
    SetLoadBasedAutoScaling'
    { _slbasrqUpScaling = Nothing
    , _slbasrqEnable = Nothing
    , _slbasrqDownScaling = Nothing
    , _slbasrqLayerId = pLayerId
    }

-- | An @AutoScalingThresholds@ object with the upscaling threshold
-- configuration. If the load exceeds these thresholds for a specified
-- amount of time, AWS OpsWorks starts a specified number of instances.
slbasrqUpScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasrqUpScaling = lens _slbasrqUpScaling (\ s a -> s{_slbasrqUpScaling = a});

-- | Enables load-based auto scaling for the layer.
slbasrqEnable :: Lens' SetLoadBasedAutoScaling (Maybe Bool)
slbasrqEnable = lens _slbasrqEnable (\ s a -> s{_slbasrqEnable = a});

-- | An @AutoScalingThresholds@ object with the downscaling threshold
-- configuration. If the load falls below these thresholds for a specified
-- amount of time, AWS OpsWorks stops a specified number of instances.
slbasrqDownScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasrqDownScaling = lens _slbasrqDownScaling (\ s a -> s{_slbasrqDownScaling = a});

-- | The layer ID.
slbasrqLayerId :: Lens' SetLoadBasedAutoScaling Text
slbasrqLayerId = lens _slbasrqLayerId (\ s a -> s{_slbasrqLayerId = a});

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
              ["UpScaling" .= _slbasrqUpScaling,
               "Enable" .= _slbasrqEnable,
               "DownScaling" .= _slbasrqDownScaling,
               "LayerId" .= _slbasrqLayerId]

instance ToPath SetLoadBasedAutoScaling where
        toPath = const "/"

instance ToQuery SetLoadBasedAutoScaling where
        toQuery = const mempty

-- | /See:/ 'setLoadBasedAutoScalingResponse' smart constructor.
data SetLoadBasedAutoScalingResponse =
    SetLoadBasedAutoScalingResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetLoadBasedAutoScalingResponse' smart constructor.
setLoadBasedAutoScalingResponse :: SetLoadBasedAutoScalingResponse
setLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse'
