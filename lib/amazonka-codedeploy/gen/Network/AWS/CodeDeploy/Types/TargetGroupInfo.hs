{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetGroupInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetGroupInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.
--
--
--
-- /See:/ 'targetGroupInfo' smart constructor.
newtype TargetGroupInfo = TargetGroupInfo' {_tgiName :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TargetGroupInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tgiName' - For blue/green deployments, the name of the target group that instances in the original environment are deregistered from, and instances in the replacement environment are registered with. For in-place deployments, the name of the target group that instances are deregistered from, so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
targetGroupInfo ::
  TargetGroupInfo
targetGroupInfo = TargetGroupInfo' {_tgiName = Nothing}

-- | For blue/green deployments, the name of the target group that instances in the original environment are deregistered from, and instances in the replacement environment are registered with. For in-place deployments, the name of the target group that instances are deregistered from, so they are not serving traffic during a deployment, and then re-registered with after the deployment is complete.
tgiName :: Lens' TargetGroupInfo (Maybe Text)
tgiName = lens _tgiName (\s a -> s {_tgiName = a})

instance FromJSON TargetGroupInfo where
  parseJSON =
    withObject
      "TargetGroupInfo"
      (\x -> TargetGroupInfo' <$> (x .:? "name"))

instance Hashable TargetGroupInfo

instance NFData TargetGroupInfo

instance ToJSON TargetGroupInfo where
  toJSON TargetGroupInfo' {..} =
    object (catMaybes [("name" .=) <$> _tgiName])
