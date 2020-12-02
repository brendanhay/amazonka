{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobObjectiveType where

import Network.AWS.Prelude

data HyperParameterTuningJobObjectiveType
  = Maximize
  | Minimize
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

instance FromText HyperParameterTuningJobObjectiveType where
  parser =
    takeLowerText >>= \case
      "maximize" -> pure Maximize
      "minimize" -> pure Minimize
      e ->
        fromTextError $
          "Failure parsing HyperParameterTuningJobObjectiveType from value: '" <> e
            <> "'. Accepted values: maximize, minimize"

instance ToText HyperParameterTuningJobObjectiveType where
  toText = \case
    Maximize -> "Maximize"
    Minimize -> "Minimize"

instance Hashable HyperParameterTuningJobObjectiveType

instance NFData HyperParameterTuningJobObjectiveType

instance ToByteString HyperParameterTuningJobObjectiveType

instance ToQuery HyperParameterTuningJobObjectiveType

instance ToHeader HyperParameterTuningJobObjectiveType

instance ToJSON HyperParameterTuningJobObjectiveType where
  toJSON = toJSONText

instance FromJSON HyperParameterTuningJobObjectiveType where
  parseJSON = parseJSONText "HyperParameterTuningJobObjectiveType"
