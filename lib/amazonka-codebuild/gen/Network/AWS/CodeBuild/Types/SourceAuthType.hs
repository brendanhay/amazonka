{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.SourceAuthType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.SourceAuthType where

import Network.AWS.Prelude

data SourceAuthType = Oauth
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

instance FromText SourceAuthType where
  parser =
    takeLowerText >>= \case
      "oauth" -> pure Oauth
      e ->
        fromTextError $
          "Failure parsing SourceAuthType from value: '" <> e
            <> "'. Accepted values: oauth"

instance ToText SourceAuthType where
  toText = \case
    Oauth -> "OAUTH"

instance Hashable SourceAuthType

instance NFData SourceAuthType

instance ToByteString SourceAuthType

instance ToQuery SourceAuthType

instance ToHeader SourceAuthType

instance ToJSON SourceAuthType where
  toJSON = toJSONText

instance FromJSON SourceAuthType where
  parseJSON = parseJSONText "SourceAuthType"
