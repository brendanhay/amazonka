{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines under what conditions SageMaker creates a human loop. Used within . See for the required format of activation conditions.
--
--
--
-- /See:/ 'humanLoopActivationConditionsConfig' smart constructor.
newtype HumanLoopActivationConditionsConfig = HumanLoopActivationConditionsConfig'
  { _hlaccHumanLoopActivationConditions ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HumanLoopActivationConditionsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hlaccHumanLoopActivationConditions' - JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
humanLoopActivationConditionsConfig ::
  -- | 'hlaccHumanLoopActivationConditions'
  Text ->
  HumanLoopActivationConditionsConfig
humanLoopActivationConditionsConfig pHumanLoopActivationConditions_ =
  HumanLoopActivationConditionsConfig'
    { _hlaccHumanLoopActivationConditions =
        pHumanLoopActivationConditions_
    }

-- | JSON expressing use-case specific conditions declaratively. If any condition is matched, atomic tasks are created against the configured work team. The set of conditions is different for Rekognition and Textract. For more information about how to structure the JSON, see <https://docs.aws.amazon.com/sagemaker/latest/dg/a2i-human-fallback-conditions-json-schema.html JSON Schema for Human Loop Activation Conditions in Amazon Augmented AI> in the /Amazon SageMaker Developer Guide/ .
hlaccHumanLoopActivationConditions :: Lens' HumanLoopActivationConditionsConfig Text
hlaccHumanLoopActivationConditions = lens _hlaccHumanLoopActivationConditions (\s a -> s {_hlaccHumanLoopActivationConditions = a})

instance FromJSON HumanLoopActivationConditionsConfig where
  parseJSON =
    withObject
      "HumanLoopActivationConditionsConfig"
      ( \x ->
          HumanLoopActivationConditionsConfig'
            <$> (x .: "HumanLoopActivationConditions")
      )

instance Hashable HumanLoopActivationConditionsConfig

instance NFData HumanLoopActivationConditionsConfig

instance ToJSON HumanLoopActivationConditionsConfig where
  toJSON HumanLoopActivationConditionsConfig' {..} =
    object
      ( catMaybes
          [ Just
              ( "HumanLoopActivationConditions"
                  .= _hlaccHumanLoopActivationConditions
              )
          ]
      )
