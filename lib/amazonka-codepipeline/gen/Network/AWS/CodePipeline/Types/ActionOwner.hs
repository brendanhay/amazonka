{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionOwner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionOwner where

import Network.AWS.Prelude

data ActionOwner
  = AWS
  | Custom
  | ThirdParty
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

instance FromText ActionOwner where
  parser =
    takeLowerText >>= \case
      "aws" -> pure AWS
      "custom" -> pure Custom
      "thirdparty" -> pure ThirdParty
      e ->
        fromTextError $
          "Failure parsing ActionOwner from value: '" <> e
            <> "'. Accepted values: aws, custom, thirdparty"

instance ToText ActionOwner where
  toText = \case
    AWS -> "AWS"
    Custom -> "Custom"
    ThirdParty -> "ThirdParty"

instance Hashable ActionOwner

instance NFData ActionOwner

instance ToByteString ActionOwner

instance ToQuery ActionOwner

instance ToHeader ActionOwner

instance ToJSON ActionOwner where
  toJSON = toJSONText

instance FromJSON ActionOwner where
  parseJSON = parseJSONText "ActionOwner"
