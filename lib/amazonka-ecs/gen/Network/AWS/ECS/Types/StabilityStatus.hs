{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.StabilityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.StabilityStatus where

import Network.AWS.Prelude

data StabilityStatus
  = Stabilizing
  | SteadyState
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

instance FromText StabilityStatus where
  parser =
    takeLowerText >>= \case
      "stabilizing" -> pure Stabilizing
      "steady_state" -> pure SteadyState
      e ->
        fromTextError $
          "Failure parsing StabilityStatus from value: '" <> e
            <> "'. Accepted values: stabilizing, steady_state"

instance ToText StabilityStatus where
  toText = \case
    Stabilizing -> "STABILIZING"
    SteadyState -> "STEADY_STATE"

instance Hashable StabilityStatus

instance NFData StabilityStatus

instance ToByteString StabilityStatus

instance ToQuery StabilityStatus

instance ToHeader StabilityStatus

instance FromJSON StabilityStatus where
  parseJSON = parseJSONText "StabilityStatus"
