{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerCondition where

import Network.AWS.Prelude

data ContainerCondition
  = Complete
  | Healthy
  | Start
  | Success
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

instance FromText ContainerCondition where
  parser =
    takeLowerText >>= \case
      "complete" -> pure Complete
      "healthy" -> pure Healthy
      "start" -> pure Start
      "success" -> pure Success
      e ->
        fromTextError $
          "Failure parsing ContainerCondition from value: '" <> e
            <> "'. Accepted values: complete, healthy, start, success"

instance ToText ContainerCondition where
  toText = \case
    Complete -> "COMPLETE"
    Healthy -> "HEALTHY"
    Start -> "START"
    Success -> "SUCCESS"

instance Hashable ContainerCondition

instance NFData ContainerCondition

instance ToByteString ContainerCondition

instance ToQuery ContainerCondition

instance ToHeader ContainerCondition

instance ToJSON ContainerCondition where
  toJSON = toJSONText

instance FromJSON ContainerCondition where
  parseJSON = parseJSONText "ContainerCondition"
