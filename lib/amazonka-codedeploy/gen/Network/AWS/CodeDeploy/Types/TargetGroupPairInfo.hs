{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetGroupPairInfo where

import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TrafficRoute
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about two target groups and how traffic is routed during an Amazon ECS deployment. An optional test traffic route can be specified.
--
--
--
-- /See:/ 'targetGroupPairInfo' smart constructor.
data TargetGroupPairInfo = TargetGroupPairInfo'
  { _tgpiProdTrafficRoute ::
      !(Maybe TrafficRoute),
    _tgpiTestTrafficRoute :: !(Maybe TrafficRoute),
    _tgpiTargetGroups :: !(Maybe [TargetGroupInfo])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGroupPairInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgpiProdTrafficRoute' - The path used by a load balancer to route production traffic when an Amazon ECS deployment is complete.
--
-- * 'tgpiTestTrafficRoute' - An optional path used by a load balancer to route test traffic after an Amazon ECS deployment. Validation can occur while test traffic is served during a deployment.
--
-- * 'tgpiTargetGroups' - One pair of target groups. One is associated with the original task set. The second is associated with the task set that serves traffic after the deployment is complete.
targetGroupPairInfo ::
  TargetGroupPairInfo
targetGroupPairInfo =
  TargetGroupPairInfo'
    { _tgpiProdTrafficRoute = Nothing,
      _tgpiTestTrafficRoute = Nothing,
      _tgpiTargetGroups = Nothing
    }

-- | The path used by a load balancer to route production traffic when an Amazon ECS deployment is complete.
tgpiProdTrafficRoute :: Lens' TargetGroupPairInfo (Maybe TrafficRoute)
tgpiProdTrafficRoute = lens _tgpiProdTrafficRoute (\s a -> s {_tgpiProdTrafficRoute = a})

-- | An optional path used by a load balancer to route test traffic after an Amazon ECS deployment. Validation can occur while test traffic is served during a deployment.
tgpiTestTrafficRoute :: Lens' TargetGroupPairInfo (Maybe TrafficRoute)
tgpiTestTrafficRoute = lens _tgpiTestTrafficRoute (\s a -> s {_tgpiTestTrafficRoute = a})

-- | One pair of target groups. One is associated with the original task set. The second is associated with the task set that serves traffic after the deployment is complete.
tgpiTargetGroups :: Lens' TargetGroupPairInfo [TargetGroupInfo]
tgpiTargetGroups = lens _tgpiTargetGroups (\s a -> s {_tgpiTargetGroups = a}) . _Default . _Coerce

instance FromJSON TargetGroupPairInfo where
  parseJSON =
    withObject
      "TargetGroupPairInfo"
      ( \x ->
          TargetGroupPairInfo'
            <$> (x .:? "prodTrafficRoute")
            <*> (x .:? "testTrafficRoute")
            <*> (x .:? "targetGroups" .!= mempty)
      )

instance Hashable TargetGroupPairInfo

instance NFData TargetGroupPairInfo

instance ToJSON TargetGroupPairInfo where
  toJSON TargetGroupPairInfo' {..} =
    object
      ( catMaybes
          [ ("prodTrafficRoute" .=) <$> _tgpiProdTrafficRoute,
            ("testTrafficRoute" .=) <$> _tgpiTestTrafficRoute,
            ("targetGroups" .=) <$> _tgpiTargetGroups
          ]
      )
