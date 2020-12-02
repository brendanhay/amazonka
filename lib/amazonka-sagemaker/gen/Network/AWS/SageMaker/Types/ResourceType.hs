{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResourceType where

import Network.AWS.Prelude

data ResourceType
  = Experiment
  | ExperimentTrial
  | ExperimentTrialComponent
  | TrainingJob
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ResourceType where
  parser =
    takeLowerText >>= \case
      "experiment" -> pure Experiment
      "experimenttrial" -> pure ExperimentTrial
      "experimenttrialcomponent" -> pure ExperimentTrialComponent
      "trainingjob" -> pure TrainingJob
      e ->
        fromTextError $
          "Failure parsing ResourceType from value: '" <> e
            <> "'. Accepted values: experiment, experimenttrial, experimenttrialcomponent, trainingjob"

instance ToText ResourceType where
  toText = \case
    Experiment -> "Experiment"
    ExperimentTrial -> "ExperimentTrial"
    ExperimentTrialComponent -> "ExperimentTrialComponent"
    TrainingJob -> "TrainingJob"

instance Hashable ResourceType

instance NFData ResourceType

instance ToByteString ResourceType

instance ToQuery ResourceType

instance ToHeader ResourceType

instance ToJSON ResourceType where
  toJSON = toJSONText
