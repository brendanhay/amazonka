{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedCanary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeBasedCanary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in two increments. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
--
--
-- /See:/ 'timeBasedCanary' smart constructor.
data TimeBasedCanary = TimeBasedCanary'
  { _tbcCanaryInterval ::
      !(Maybe Int),
    _tbcCanaryPercentage :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeBasedCanary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tbcCanaryInterval' - The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
--
-- * 'tbcCanaryPercentage' - The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
timeBasedCanary ::
  TimeBasedCanary
timeBasedCanary =
  TimeBasedCanary'
    { _tbcCanaryInterval = Nothing,
      _tbcCanaryPercentage = Nothing
    }

-- | The number of minutes between the first and second traffic shifts of a @TimeBasedCanary@ deployment.
tbcCanaryInterval :: Lens' TimeBasedCanary (Maybe Int)
tbcCanaryInterval = lens _tbcCanaryInterval (\s a -> s {_tbcCanaryInterval = a})

-- | The percentage of traffic to shift in the first increment of a @TimeBasedCanary@ deployment.
tbcCanaryPercentage :: Lens' TimeBasedCanary (Maybe Int)
tbcCanaryPercentage = lens _tbcCanaryPercentage (\s a -> s {_tbcCanaryPercentage = a})

instance FromJSON TimeBasedCanary where
  parseJSON =
    withObject
      "TimeBasedCanary"
      ( \x ->
          TimeBasedCanary'
            <$> (x .:? "canaryInterval") <*> (x .:? "canaryPercentage")
      )

instance Hashable TimeBasedCanary

instance NFData TimeBasedCanary

instance ToJSON TimeBasedCanary where
  toJSON TimeBasedCanary' {..} =
    object
      ( catMaybes
          [ ("canaryInterval" .=) <$> _tbcCanaryInterval,
            ("canaryPercentage" .=) <$> _tbcCanaryPercentage
          ]
      )
