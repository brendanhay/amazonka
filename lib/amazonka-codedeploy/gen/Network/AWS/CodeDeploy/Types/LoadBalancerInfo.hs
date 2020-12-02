{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LoadBalancerInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LoadBalancerInfo where

import Network.AWS.CodeDeploy.Types.ELBInfo
import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about the Elastic Load Balancing load balancer or target group used in a deployment.
--
--
--
-- /See:/ 'loadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { _lbiElbInfoList ::
      !(Maybe [ELBInfo]),
    _lbiTargetGroupInfoList :: !(Maybe [TargetGroupInfo]),
    _lbiTargetGroupPairInfoList ::
      !(Maybe [TargetGroupPairInfo])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LoadBalancerInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbiElbInfoList' - An array that contains information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
--
-- * 'lbiTargetGroupInfoList' - An array that contains information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
--
-- * 'lbiTargetGroupPairInfoList' - The target group pair information. This is an array of @TargeGroupPairInfo@ objects with a maximum size of one.
loadBalancerInfo ::
  LoadBalancerInfo
loadBalancerInfo =
  LoadBalancerInfo'
    { _lbiElbInfoList = Nothing,
      _lbiTargetGroupInfoList = Nothing,
      _lbiTargetGroupPairInfoList = Nothing
    }

-- | An array that contains information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
lbiElbInfoList :: Lens' LoadBalancerInfo [ELBInfo]
lbiElbInfoList = lens _lbiElbInfoList (\s a -> s {_lbiElbInfoList = a}) . _Default . _Coerce

-- | An array that contains information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
lbiTargetGroupInfoList :: Lens' LoadBalancerInfo [TargetGroupInfo]
lbiTargetGroupInfoList = lens _lbiTargetGroupInfoList (\s a -> s {_lbiTargetGroupInfoList = a}) . _Default . _Coerce

-- | The target group pair information. This is an array of @TargeGroupPairInfo@ objects with a maximum size of one.
lbiTargetGroupPairInfoList :: Lens' LoadBalancerInfo [TargetGroupPairInfo]
lbiTargetGroupPairInfoList = lens _lbiTargetGroupPairInfoList (\s a -> s {_lbiTargetGroupPairInfoList = a}) . _Default . _Coerce

instance FromJSON LoadBalancerInfo where
  parseJSON =
    withObject
      "LoadBalancerInfo"
      ( \x ->
          LoadBalancerInfo'
            <$> (x .:? "elbInfoList" .!= mempty)
            <*> (x .:? "targetGroupInfoList" .!= mempty)
            <*> (x .:? "targetGroupPairInfoList" .!= mempty)
      )

instance Hashable LoadBalancerInfo

instance NFData LoadBalancerInfo

instance ToJSON LoadBalancerInfo where
  toJSON LoadBalancerInfo' {..} =
    object
      ( catMaybes
          [ ("elbInfoList" .=) <$> _lbiElbInfoList,
            ("targetGroupInfoList" .=) <$> _lbiTargetGroupInfoList,
            ("targetGroupPairInfoList" .=) <$> _lbiTargetGroupPairInfoList
          ]
      )
