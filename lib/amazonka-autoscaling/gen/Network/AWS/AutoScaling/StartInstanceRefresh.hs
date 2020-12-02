{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.StartInstanceRefresh
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new instance refresh operation, which triggers a rolling replacement of all previously launched instances in the Auto Scaling group with a new group of instances.
--
--
-- If successful, this call creates a new instance refresh request with a unique ID that you can use to track its progress. To query its status, call the 'DescribeInstanceRefreshes' API. To describe the instance refreshes that have already run, call the 'DescribeInstanceRefreshes' API. To cancel an instance refresh operation in progress, use the 'CancelInstanceRefresh' API.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh> .
module Network.AWS.AutoScaling.StartInstanceRefresh
  ( -- * Creating a Request
    startInstanceRefresh,
    StartInstanceRefresh,

    -- * Request Lenses
    sirPreferences,
    sirStrategy,
    sirAutoScalingGroupName,

    -- * Destructuring the Response
    startInstanceRefreshResponse,
    StartInstanceRefreshResponse,

    -- * Response Lenses
    sirrsInstanceRefreshId,
    sirrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startInstanceRefresh' smart constructor.
data StartInstanceRefresh = StartInstanceRefresh'
  { _sirPreferences ::
      !(Maybe RefreshPreferences),
    _sirStrategy :: !(Maybe RefreshStrategy),
    _sirAutoScalingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartInstanceRefresh' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirPreferences' - Set of preferences associated with the instance refresh request. If not provided, the default values are used. For @MinHealthyPercentage@ , the default value is @90@ . For @InstanceWarmup@ , the default is to use the value specified for the health check grace period for the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences> in the /Amazon EC2 Auto Scaling API Reference/ .
--
-- * 'sirStrategy' - The strategy to use for the instance refresh. The only valid value is @Rolling@ . A rolling update is an update that is applied to all instances in an Auto Scaling group until all instances have been updated. A rolling update can fail due to failed health checks or if instances are on standby or are protected from scale in. If the rolling update process fails, any instances that were already replaced are not rolled back to their previous configuration.
--
-- * 'sirAutoScalingGroupName' - The name of the Auto Scaling group.
startInstanceRefresh ::
  -- | 'sirAutoScalingGroupName'
  Text ->
  StartInstanceRefresh
startInstanceRefresh pAutoScalingGroupName_ =
  StartInstanceRefresh'
    { _sirPreferences = Nothing,
      _sirStrategy = Nothing,
      _sirAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | Set of preferences associated with the instance refresh request. If not provided, the default values are used. For @MinHealthyPercentage@ , the default value is @90@ . For @InstanceWarmup@ , the default is to use the value specified for the health check grace period for the Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_RefreshPreferences.html RefreshPreferences> in the /Amazon EC2 Auto Scaling API Reference/ .
sirPreferences :: Lens' StartInstanceRefresh (Maybe RefreshPreferences)
sirPreferences = lens _sirPreferences (\s a -> s {_sirPreferences = a})

-- | The strategy to use for the instance refresh. The only valid value is @Rolling@ . A rolling update is an update that is applied to all instances in an Auto Scaling group until all instances have been updated. A rolling update can fail due to failed health checks or if instances are on standby or are protected from scale in. If the rolling update process fails, any instances that were already replaced are not rolled back to their previous configuration.
sirStrategy :: Lens' StartInstanceRefresh (Maybe RefreshStrategy)
sirStrategy = lens _sirStrategy (\s a -> s {_sirStrategy = a})

-- | The name of the Auto Scaling group.
sirAutoScalingGroupName :: Lens' StartInstanceRefresh Text
sirAutoScalingGroupName = lens _sirAutoScalingGroupName (\s a -> s {_sirAutoScalingGroupName = a})

instance AWSRequest StartInstanceRefresh where
  type Rs StartInstanceRefresh = StartInstanceRefreshResponse
  request = postQuery autoScaling
  response =
    receiveXMLWrapper
      "StartInstanceRefreshResult"
      ( \s h x ->
          StartInstanceRefreshResponse'
            <$> (x .@? "InstanceRefreshId") <*> (pure (fromEnum s))
      )

instance Hashable StartInstanceRefresh

instance NFData StartInstanceRefresh

instance ToHeaders StartInstanceRefresh where
  toHeaders = const mempty

instance ToPath StartInstanceRefresh where
  toPath = const "/"

instance ToQuery StartInstanceRefresh where
  toQuery StartInstanceRefresh' {..} =
    mconcat
      [ "Action" =: ("StartInstanceRefresh" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "Preferences" =: _sirPreferences,
        "Strategy" =: _sirStrategy,
        "AutoScalingGroupName" =: _sirAutoScalingGroupName
      ]

-- | /See:/ 'startInstanceRefreshResponse' smart constructor.
data StartInstanceRefreshResponse = StartInstanceRefreshResponse'
  { _sirrsInstanceRefreshId ::
      !(Maybe Text),
    _sirrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartInstanceRefreshResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirrsInstanceRefreshId' - A unique ID for tracking the progress of the request.
--
-- * 'sirrsResponseStatus' - -- | The response status code.
startInstanceRefreshResponse ::
  -- | 'sirrsResponseStatus'
  Int ->
  StartInstanceRefreshResponse
startInstanceRefreshResponse pResponseStatus_ =
  StartInstanceRefreshResponse'
    { _sirrsInstanceRefreshId = Nothing,
      _sirrsResponseStatus = pResponseStatus_
    }

-- | A unique ID for tracking the progress of the request.
sirrsInstanceRefreshId :: Lens' StartInstanceRefreshResponse (Maybe Text)
sirrsInstanceRefreshId = lens _sirrsInstanceRefreshId (\s a -> s {_sirrsInstanceRefreshId = a})

-- | -- | The response status code.
sirrsResponseStatus :: Lens' StartInstanceRefreshResponse Int
sirrsResponseStatus = lens _sirrsResponseStatus (\s a -> s {_sirrsResponseStatus = a})

instance NFData StartInstanceRefreshResponse
