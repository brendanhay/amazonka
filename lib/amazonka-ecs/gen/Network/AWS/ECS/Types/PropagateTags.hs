{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PropagateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PropagateTags where

import Network.AWS.Prelude

data PropagateTags
  = Service
  | TaskDefinition
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

instance FromText PropagateTags where
  parser =
    takeLowerText >>= \case
      "service" -> pure Service
      "task_definition" -> pure TaskDefinition
      e ->
        fromTextError $
          "Failure parsing PropagateTags from value: '" <> e
            <> "'. Accepted values: service, task_definition"

instance ToText PropagateTags where
  toText = \case
    Service -> "SERVICE"
    TaskDefinition -> "TASK_DEFINITION"

instance Hashable PropagateTags

instance NFData PropagateTags

instance ToByteString PropagateTags

instance ToQuery PropagateTags

instance ToHeader PropagateTags

instance ToJSON PropagateTags where
  toJSON = toJSONText

instance FromJSON PropagateTags where
  parseJSON = parseJSONText "PropagateTags"
