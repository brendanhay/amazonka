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
-- Module      : Network.AWS.AutoScaling.ExitStandby
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves the specified instances out of the standby state.
--
--
-- After you put the instances back in service, the desired capacity is incremented.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-enter-exit-standby.html Temporarily removing instances from your Auto Scaling group> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.ExitStandby
  ( -- * Creating a Request
    exitStandby,
    ExitStandby,

    -- * Request Lenses
    eInstanceIds,
    eAutoScalingGroupName,

    -- * Destructuring the Response
    exitStandbyResponse,
    ExitStandbyResponse,

    -- * Response Lenses
    esrsActivities,
    esrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exitStandby' smart constructor.
data ExitStandby = ExitStandby'
  { _eInstanceIds :: !(Maybe [Text]),
    _eAutoScalingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExitStandby' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eInstanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- * 'eAutoScalingGroupName' - The name of the Auto Scaling group.
exitStandby ::
  -- | 'eAutoScalingGroupName'
  Text ->
  ExitStandby
exitStandby pAutoScalingGroupName_ =
  ExitStandby'
    { _eInstanceIds = Nothing,
      _eAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | The IDs of the instances. You can specify up to 20 instances.
eInstanceIds :: Lens' ExitStandby [Text]
eInstanceIds = lens _eInstanceIds (\s a -> s {_eInstanceIds = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
eAutoScalingGroupName :: Lens' ExitStandby Text
eAutoScalingGroupName = lens _eAutoScalingGroupName (\s a -> s {_eAutoScalingGroupName = a})

instance AWSRequest ExitStandby where
  type Rs ExitStandby = ExitStandbyResponse
  request = postQuery autoScaling
  response =
    receiveXMLWrapper
      "ExitStandbyResult"
      ( \s h x ->
          ExitStandbyResponse'
            <$> (x .@? "Activities" .!@ mempty >>= may (parseXMLList "member"))
            <*> (pure (fromEnum s))
      )

instance Hashable ExitStandby

instance NFData ExitStandby

instance ToHeaders ExitStandby where
  toHeaders = const mempty

instance ToPath ExitStandby where
  toPath = const "/"

instance ToQuery ExitStandby where
  toQuery ExitStandby' {..} =
    mconcat
      [ "Action" =: ("ExitStandby" :: ByteString),
        "Version" =: ("2011-01-01" :: ByteString),
        "InstanceIds" =: toQuery (toQueryList "member" <$> _eInstanceIds),
        "AutoScalingGroupName" =: _eAutoScalingGroupName
      ]

-- | /See:/ 'exitStandbyResponse' smart constructor.
data ExitStandbyResponse = ExitStandbyResponse'
  { _esrsActivities ::
      !(Maybe [Activity]),
    _esrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExitStandbyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsActivities' - The activities related to moving instances out of @Standby@ mode.
--
-- * 'esrsResponseStatus' - -- | The response status code.
exitStandbyResponse ::
  -- | 'esrsResponseStatus'
  Int ->
  ExitStandbyResponse
exitStandbyResponse pResponseStatus_ =
  ExitStandbyResponse'
    { _esrsActivities = Nothing,
      _esrsResponseStatus = pResponseStatus_
    }

-- | The activities related to moving instances out of @Standby@ mode.
esrsActivities :: Lens' ExitStandbyResponse [Activity]
esrsActivities = lens _esrsActivities (\s a -> s {_esrsActivities = a}) . _Default . _Coerce

-- | -- | The response status code.
esrsResponseStatus :: Lens' ExitStandbyResponse Int
esrsResponseStatus = lens _esrsResponseStatus (\s a -> s {_esrsResponseStatus = a})

instance NFData ExitStandbyResponse
