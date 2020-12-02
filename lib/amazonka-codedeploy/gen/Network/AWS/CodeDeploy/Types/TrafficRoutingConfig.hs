{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoutingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoutingConfig where

import Network.AWS.CodeDeploy.Types.TimeBasedCanary
import Network.AWS.CodeDeploy.Types.TimeBasedLinear
import Network.AWS.CodeDeploy.Types.TrafficRoutingType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment, or from one Amazon ECS task set to another during an Amazon ECS deployment.
--
--
--
-- /See:/ 'trafficRoutingConfig' smart constructor.
data TrafficRoutingConfig = TrafficRoutingConfig'
  { _trcTimeBasedCanary ::
      !(Maybe TimeBasedCanary),
    _trcTimeBasedLinear :: !(Maybe TimeBasedLinear),
    _trcType :: !(Maybe TrafficRoutingType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrafficRoutingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trcTimeBasedCanary' - A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- * 'trcTimeBasedLinear' - A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- * 'trcType' - The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration.
trafficRoutingConfig ::
  TrafficRoutingConfig
trafficRoutingConfig =
  TrafficRoutingConfig'
    { _trcTimeBasedCanary = Nothing,
      _trcTimeBasedLinear = Nothing,
      _trcType = Nothing
    }

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
trcTimeBasedCanary :: Lens' TrafficRoutingConfig (Maybe TimeBasedCanary)
trcTimeBasedCanary = lens _trcTimeBasedCanary (\s a -> s {_trcTimeBasedCanary = a})

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
trcTimeBasedLinear :: Lens' TrafficRoutingConfig (Maybe TimeBasedLinear)
trcTimeBasedLinear = lens _trcTimeBasedLinear (\s a -> s {_trcTimeBasedLinear = a})

-- | The type of traffic shifting (@TimeBasedCanary@ or @TimeBasedLinear@ ) used by a deployment configuration.
trcType :: Lens' TrafficRoutingConfig (Maybe TrafficRoutingType)
trcType = lens _trcType (\s a -> s {_trcType = a})

instance FromJSON TrafficRoutingConfig where
  parseJSON =
    withObject
      "TrafficRoutingConfig"
      ( \x ->
          TrafficRoutingConfig'
            <$> (x .:? "timeBasedCanary")
            <*> (x .:? "timeBasedLinear")
            <*> (x .:? "type")
      )

instance Hashable TrafficRoutingConfig

instance NFData TrafficRoutingConfig

instance ToJSON TrafficRoutingConfig where
  toJSON TrafficRoutingConfig' {..} =
    object
      ( catMaybes
          [ ("timeBasedCanary" .=) <$> _trcTimeBasedCanary,
            ("timeBasedLinear" .=) <$> _trcTimeBasedLinear,
            ("type" .=) <$> _trcType
          ]
      )
