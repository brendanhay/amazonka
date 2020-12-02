{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ResolverKind
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ResolverKind where

import Network.AWS.Prelude

data ResolverKind
  = Pipeline
  | Unit
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

instance FromText ResolverKind where
  parser =
    takeLowerText >>= \case
      "pipeline" -> pure Pipeline
      "unit" -> pure Unit
      e ->
        fromTextError $
          "Failure parsing ResolverKind from value: '" <> e
            <> "'. Accepted values: pipeline, unit"

instance ToText ResolverKind where
  toText = \case
    Pipeline -> "PIPELINE"
    Unit -> "UNIT"

instance Hashable ResolverKind

instance NFData ResolverKind

instance ToByteString ResolverKind

instance ToQuery ResolverKind

instance ToHeader ResolverKind

instance ToJSON ResolverKind where
  toJSON = toJSONText

instance FromJSON ResolverKind where
  parseJSON = parseJSONText "ResolverKind"
