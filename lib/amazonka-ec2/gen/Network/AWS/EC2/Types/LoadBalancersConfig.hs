{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadBalancersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadBalancersConfig where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClassicLoadBalancersConfig
import Network.AWS.EC2.Types.TargetGroupsConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Classic Load Balancers and target groups to attach to a Spot Fleet request.
--
--
--
-- /See:/ 'loadBalancersConfig' smart constructor.
data LoadBalancersConfig = LoadBalancersConfig'
  { _lbcClassicLoadBalancersConfig ::
      !(Maybe ClassicLoadBalancersConfig),
    _lbcTargetGroupsConfig ::
      !(Maybe TargetGroupsConfig)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancersConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbcClassicLoadBalancersConfig' - The Classic Load Balancers.
--
-- * 'lbcTargetGroupsConfig' - The target groups.
loadBalancersConfig ::
  LoadBalancersConfig
loadBalancersConfig =
  LoadBalancersConfig'
    { _lbcClassicLoadBalancersConfig = Nothing,
      _lbcTargetGroupsConfig = Nothing
    }

-- | The Classic Load Balancers.
lbcClassicLoadBalancersConfig :: Lens' LoadBalancersConfig (Maybe ClassicLoadBalancersConfig)
lbcClassicLoadBalancersConfig = lens _lbcClassicLoadBalancersConfig (\s a -> s {_lbcClassicLoadBalancersConfig = a})

-- | The target groups.
lbcTargetGroupsConfig :: Lens' LoadBalancersConfig (Maybe TargetGroupsConfig)
lbcTargetGroupsConfig = lens _lbcTargetGroupsConfig (\s a -> s {_lbcTargetGroupsConfig = a})

instance FromXML LoadBalancersConfig where
  parseXML x =
    LoadBalancersConfig'
      <$> (x .@? "classicLoadBalancersConfig")
      <*> (x .@? "targetGroupsConfig")

instance Hashable LoadBalancersConfig

instance NFData LoadBalancersConfig

instance ToQuery LoadBalancersConfig where
  toQuery LoadBalancersConfig' {..} =
    mconcat
      [ "ClassicLoadBalancersConfig" =: _lbcClassicLoadBalancersConfig,
        "TargetGroupsConfig" =: _lbcTargetGroupsConfig
      ]
