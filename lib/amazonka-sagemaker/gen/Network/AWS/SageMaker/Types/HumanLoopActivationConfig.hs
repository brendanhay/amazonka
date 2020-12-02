{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig

-- | Provides information about how and under what conditions SageMaker creates a human loop. If @HumanLoopActivationConfig@ is not given, then all requests go to humans.
--
--
--
-- /See:/ 'humanLoopActivationConfig' smart constructor.
newtype HumanLoopActivationConfig = HumanLoopActivationConfig'
  { _hlacHumanLoopActivationConditionsConfig ::
      HumanLoopActivationConditionsConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopActivationConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlacHumanLoopActivationConditionsConfig' - Container structure for defining under what conditions SageMaker creates a human loop.
humanLoopActivationConfig ::
  -- | 'hlacHumanLoopActivationConditionsConfig'
  HumanLoopActivationConditionsConfig ->
  HumanLoopActivationConfig
humanLoopActivationConfig pHumanLoopActivationConditionsConfig_ =
  HumanLoopActivationConfig'
    { _hlacHumanLoopActivationConditionsConfig =
        pHumanLoopActivationConditionsConfig_
    }

-- | Container structure for defining under what conditions SageMaker creates a human loop.
hlacHumanLoopActivationConditionsConfig :: Lens' HumanLoopActivationConfig HumanLoopActivationConditionsConfig
hlacHumanLoopActivationConditionsConfig = lens _hlacHumanLoopActivationConditionsConfig (\s a -> s {_hlacHumanLoopActivationConditionsConfig = a})

instance FromJSON HumanLoopActivationConfig where
  parseJSON =
    withObject
      "HumanLoopActivationConfig"
      ( \x ->
          HumanLoopActivationConfig'
            <$> (x .: "HumanLoopActivationConditionsConfig")
      )

instance Hashable HumanLoopActivationConfig

instance NFData HumanLoopActivationConfig

instance ToJSON HumanLoopActivationConfig where
  toJSON HumanLoopActivationConfig' {..} =
    object
      ( catMaybes
          [ Just
              ( "HumanLoopActivationConditionsConfig"
                  .= _hlacHumanLoopActivationConditionsConfig
              )
          ]
      )
