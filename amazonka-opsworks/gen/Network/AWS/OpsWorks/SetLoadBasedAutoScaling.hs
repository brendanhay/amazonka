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
-- Module      : Network.AWS.OpsWorks.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specify the load-based auto scaling configuration for a specified layer. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html Managing Load with Time-based and Load-based Instances> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.SetLoadBasedAutoScaling
    (
    -- * Creating a Request
      setLoadBasedAutoScaling
    , SetLoadBasedAutoScaling
    -- * Request Lenses
    , slbasUpScaling
    , slbasEnable
    , slbasDownScaling
    , slbasLayerId

    -- * Destructuring the Response
    , setLoadBasedAutoScalingResponse
    , SetLoadBasedAutoScalingResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setLoadBasedAutoScaling' smart constructor.
data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling'
  { _slbasUpScaling   :: !(Maybe AutoScalingThresholds)
  , _slbasEnable      :: !(Maybe Bool)
  , _slbasDownScaling :: !(Maybe AutoScalingThresholds)
  , _slbasLayerId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoadBasedAutoScaling' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slbasUpScaling' - An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
--
-- * 'slbasEnable' - Enables load-based auto scaling for the layer.
--
-- * 'slbasDownScaling' - An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
--
-- * 'slbasLayerId' - The layer ID.
setLoadBasedAutoScaling
    :: Text -- ^ 'slbasLayerId'
    -> SetLoadBasedAutoScaling
setLoadBasedAutoScaling pLayerId_ =
  SetLoadBasedAutoScaling'
    { _slbasUpScaling = Nothing
    , _slbasEnable = Nothing
    , _slbasDownScaling = Nothing
    , _slbasLayerId = pLayerId_
    }


-- | An @AutoScalingThresholds@ object with the upscaling threshold configuration. If the load exceeds these thresholds for a specified amount of time, AWS OpsWorks Stacks starts a specified number of instances.
slbasUpScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasUpScaling = lens _slbasUpScaling (\ s a -> s{_slbasUpScaling = a})

-- | Enables load-based auto scaling for the layer.
slbasEnable :: Lens' SetLoadBasedAutoScaling (Maybe Bool)
slbasEnable = lens _slbasEnable (\ s a -> s{_slbasEnable = a})

-- | An @AutoScalingThresholds@ object with the downscaling threshold configuration. If the load falls below these thresholds for a specified amount of time, AWS OpsWorks Stacks stops a specified number of instances.
slbasDownScaling :: Lens' SetLoadBasedAutoScaling (Maybe AutoScalingThresholds)
slbasDownScaling = lens _slbasDownScaling (\ s a -> s{_slbasDownScaling = a})

-- | The layer ID.
slbasLayerId :: Lens' SetLoadBasedAutoScaling Text
slbasLayerId = lens _slbasLayerId (\ s a -> s{_slbasLayerId = a})

instance AWSRequest SetLoadBasedAutoScaling where
        type Rs SetLoadBasedAutoScaling =
             SetLoadBasedAutoScalingResponse
        request = postJSON opsWorks
        response
          = receiveNull SetLoadBasedAutoScalingResponse'

instance Hashable SetLoadBasedAutoScaling where

instance NFData SetLoadBasedAutoScaling where

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
              (catMaybes
                 [("UpScaling" .=) <$> _slbasUpScaling,
                  ("Enable" .=) <$> _slbasEnable,
                  ("DownScaling" .=) <$> _slbasDownScaling,
                  Just ("LayerId" .= _slbasLayerId)])

instance ToPath SetLoadBasedAutoScaling where
        toPath = const "/"

instance ToQuery SetLoadBasedAutoScaling where
        toQuery = const mempty

-- | /See:/ 'setLoadBasedAutoScalingResponse' smart constructor.
data SetLoadBasedAutoScalingResponse =
  SetLoadBasedAutoScalingResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoadBasedAutoScalingResponse' with the minimum fields required to make a request.
--
setLoadBasedAutoScalingResponse
    :: SetLoadBasedAutoScalingResponse
setLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse'


instance NFData SetLoadBasedAutoScalingResponse where
