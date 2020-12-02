{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingAction where

import Network.AWS.EMR.Types.MarketType
import Network.AWS.EMR.Types.SimpleScalingPolicyConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
--
--
--
-- /See:/ 'scalingAction' smart constructor.
data ScalingAction = ScalingAction'
  { _saMarket ::
      !(Maybe MarketType),
    _saSimpleScalingPolicyConfiguration ::
      !SimpleScalingPolicyConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'saMarket' - Not available for instance groups. Instance groups use the market type specified for the group.
--
-- * 'saSimpleScalingPolicyConfiguration' - The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
scalingAction ::
  -- | 'saSimpleScalingPolicyConfiguration'
  SimpleScalingPolicyConfiguration ->
  ScalingAction
scalingAction pSimpleScalingPolicyConfiguration_ =
  ScalingAction'
    { _saMarket = Nothing,
      _saSimpleScalingPolicyConfiguration =
        pSimpleScalingPolicyConfiguration_
    }

-- | Not available for instance groups. Instance groups use the market type specified for the group.
saMarket :: Lens' ScalingAction (Maybe MarketType)
saMarket = lens _saMarket (\s a -> s {_saMarket = a})

-- | The type of adjustment the automatic scaling activity makes when triggered, and the periodicity of the adjustment.
saSimpleScalingPolicyConfiguration :: Lens' ScalingAction SimpleScalingPolicyConfiguration
saSimpleScalingPolicyConfiguration = lens _saSimpleScalingPolicyConfiguration (\s a -> s {_saSimpleScalingPolicyConfiguration = a})

instance FromJSON ScalingAction where
  parseJSON =
    withObject
      "ScalingAction"
      ( \x ->
          ScalingAction'
            <$> (x .:? "Market") <*> (x .: "SimpleScalingPolicyConfiguration")
      )

instance Hashable ScalingAction

instance NFData ScalingAction

instance ToJSON ScalingAction where
  toJSON ScalingAction' {..} =
    object
      ( catMaybes
          [ ("Market" .=) <$> _saMarket,
            Just
              ( "SimpleScalingPolicyConfiguration"
                  .= _saSimpleScalingPolicyConfiguration
              )
          ]
      )
