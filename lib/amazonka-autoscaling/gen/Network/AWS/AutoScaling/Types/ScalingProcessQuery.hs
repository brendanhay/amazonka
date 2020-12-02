{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScalingProcessQuery
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScalingProcessQuery where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'scalingProcessQuery' smart constructor.
data ScalingProcessQuery = ScalingProcessQuery'
  { _spqScalingProcesses ::
      !(Maybe [Text]),
    _spqAutoScalingGroupName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingProcessQuery' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spqScalingProcesses' - One or more of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @InstanceRefresh@      * @ReplaceUnhealthy@      * @ScheduledActions@  If you omit this parameter, all processes are specified.
--
-- * 'spqAutoScalingGroupName' - The name of the Auto Scaling group.
scalingProcessQuery ::
  -- | 'spqAutoScalingGroupName'
  Text ->
  ScalingProcessQuery
scalingProcessQuery pAutoScalingGroupName_ =
  ScalingProcessQuery'
    { _spqScalingProcesses = Nothing,
      _spqAutoScalingGroupName = pAutoScalingGroupName_
    }

-- | One or more of the following processes:     * @Launch@      * @Terminate@      * @AddToLoadBalancer@      * @AlarmNotification@      * @AZRebalance@      * @HealthCheck@      * @InstanceRefresh@      * @ReplaceUnhealthy@      * @ScheduledActions@  If you omit this parameter, all processes are specified.
spqScalingProcesses :: Lens' ScalingProcessQuery [Text]
spqScalingProcesses = lens _spqScalingProcesses (\s a -> s {_spqScalingProcesses = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
spqAutoScalingGroupName :: Lens' ScalingProcessQuery Text
spqAutoScalingGroupName = lens _spqAutoScalingGroupName (\s a -> s {_spqAutoScalingGroupName = a})

instance Hashable ScalingProcessQuery

instance NFData ScalingProcessQuery

instance ToQuery ScalingProcessQuery where
  toQuery ScalingProcessQuery' {..} =
    mconcat
      [ "ScalingProcesses"
          =: toQuery (toQueryList "member" <$> _spqScalingProcesses),
        "AutoScalingGroupName" =: _spqAutoScalingGroupName
      ]
