{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionLocationType where

import Network.AWS.Prelude

data RevisionLocationType
  = AppSpecContent
  | GitHub
  | S3
  | String
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

instance FromText RevisionLocationType where
  parser =
    takeLowerText >>= \case
      "appspeccontent" -> pure AppSpecContent
      "github" -> pure GitHub
      "s3" -> pure S3
      "string" -> pure String
      e ->
        fromTextError $
          "Failure parsing RevisionLocationType from value: '" <> e
            <> "'. Accepted values: appspeccontent, github, s3, string"

instance ToText RevisionLocationType where
  toText = \case
    AppSpecContent -> "AppSpecContent"
    GitHub -> "GitHub"
    S3 -> "S3"
    String -> "String"

instance Hashable RevisionLocationType

instance NFData RevisionLocationType

instance ToByteString RevisionLocationType

instance ToQuery RevisionLocationType

instance ToHeader RevisionLocationType

instance ToJSON RevisionLocationType where
  toJSON = toJSONText

instance FromJSON RevisionLocationType where
  parseJSON = parseJSONText "RevisionLocationType"
