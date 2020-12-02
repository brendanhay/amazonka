{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookBuildType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookBuildType where

import Network.AWS.Prelude

data WebhookBuildType
  = Build
  | BuildBatch
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

instance FromText WebhookBuildType where
  parser =
    takeLowerText >>= \case
      "build" -> pure Build
      "build_batch" -> pure BuildBatch
      e ->
        fromTextError $
          "Failure parsing WebhookBuildType from value: '" <> e
            <> "'. Accepted values: build, build_batch"

instance ToText WebhookBuildType where
  toText = \case
    Build -> "BUILD"
    BuildBatch -> "BUILD_BATCH"

instance Hashable WebhookBuildType

instance NFData WebhookBuildType

instance ToByteString WebhookBuildType

instance ToQuery WebhookBuildType

instance ToHeader WebhookBuildType

instance ToJSON WebhookBuildType where
  toJSON = toJSONText

instance FromJSON WebhookBuildType where
  parseJSON = parseJSONText "WebhookBuildType"
