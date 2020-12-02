{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.AppType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.AppType where

import Network.AWS.Prelude

data AppType
  = ATAWSFlowRuby
  | ATJava
  | ATNodejs
  | ATOther
  | ATPHP
  | ATRails
  | ATStatic
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

instance FromText AppType where
  parser =
    takeLowerText >>= \case
      "aws-flow-ruby" -> pure ATAWSFlowRuby
      "java" -> pure ATJava
      "nodejs" -> pure ATNodejs
      "other" -> pure ATOther
      "php" -> pure ATPHP
      "rails" -> pure ATRails
      "static" -> pure ATStatic
      e ->
        fromTextError $
          "Failure parsing AppType from value: '" <> e
            <> "'. Accepted values: aws-flow-ruby, java, nodejs, other, php, rails, static"

instance ToText AppType where
  toText = \case
    ATAWSFlowRuby -> "aws-flow-ruby"
    ATJava -> "java"
    ATNodejs -> "nodejs"
    ATOther -> "other"
    ATPHP -> "php"
    ATRails -> "rails"
    ATStatic -> "static"

instance Hashable AppType

instance NFData AppType

instance ToByteString AppType

instance ToQuery AppType

instance ToHeader AppType

instance ToJSON AppType where
  toJSON = toJSONText

instance FromJSON AppType where
  parseJSON = parseJSONText "AppType"
