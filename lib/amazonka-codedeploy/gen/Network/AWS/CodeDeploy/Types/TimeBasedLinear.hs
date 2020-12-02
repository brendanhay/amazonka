{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedLinear
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeBasedLinear where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
--
--
-- /See:/ 'timeBasedLinear' smart constructor.
data TimeBasedLinear = TimeBasedLinear'
  { _tblLinearInterval ::
      !(Maybe Int),
    _tblLinearPercentage :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeBasedLinear' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tblLinearInterval' - The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
--
-- * 'tblLinearPercentage' - The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
timeBasedLinear ::
  TimeBasedLinear
timeBasedLinear =
  TimeBasedLinear'
    { _tblLinearInterval = Nothing,
      _tblLinearPercentage = Nothing
    }

-- | The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
tblLinearInterval :: Lens' TimeBasedLinear (Maybe Int)
tblLinearInterval = lens _tblLinearInterval (\s a -> s {_tblLinearInterval = a})

-- | The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
tblLinearPercentage :: Lens' TimeBasedLinear (Maybe Int)
tblLinearPercentage = lens _tblLinearPercentage (\s a -> s {_tblLinearPercentage = a})

instance FromJSON TimeBasedLinear where
  parseJSON =
    withObject
      "TimeBasedLinear"
      ( \x ->
          TimeBasedLinear'
            <$> (x .:? "linearInterval") <*> (x .:? "linearPercentage")
      )

instance Hashable TimeBasedLinear

instance NFData TimeBasedLinear

instance ToJSON TimeBasedLinear where
  toJSON TimeBasedLinear' {..} =
    object
      ( catMaybes
          [ ("linearInterval" .=) <$> _tblLinearInterval,
            ("linearPercentage" .=) <$> _tblLinearPercentage
          ]
      )
