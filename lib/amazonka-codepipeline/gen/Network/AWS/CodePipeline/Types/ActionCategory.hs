{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionCategory where

import Network.AWS.Prelude

data ActionCategory
  = Approval
  | Build
  | Deploy
  | Invoke
  | Source
  | Test
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

instance FromText ActionCategory where
  parser =
    takeLowerText >>= \case
      "approval" -> pure Approval
      "build" -> pure Build
      "deploy" -> pure Deploy
      "invoke" -> pure Invoke
      "source" -> pure Source
      "test" -> pure Test
      e ->
        fromTextError $
          "Failure parsing ActionCategory from value: '" <> e
            <> "'. Accepted values: approval, build, deploy, invoke, source, test"

instance ToText ActionCategory where
  toText = \case
    Approval -> "Approval"
    Build -> "Build"
    Deploy -> "Deploy"
    Invoke -> "Invoke"
    Source -> "Source"
    Test -> "Test"

instance Hashable ActionCategory

instance NFData ActionCategory

instance ToByteString ActionCategory

instance ToQuery ActionCategory

instance ToHeader ActionCategory

instance ToJSON ActionCategory where
  toJSON = toJSONText

instance FromJSON ActionCategory where
  parseJSON = parseJSONText "ActionCategory"
