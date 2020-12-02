{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.SourceAccessType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.SourceAccessType where

import Network.AWS.Prelude

data SourceAccessType = BasicAuth
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

instance FromText SourceAccessType where
  parser =
    takeLowerText >>= \case
      "basic_auth" -> pure BasicAuth
      e ->
        fromTextError $
          "Failure parsing SourceAccessType from value: '" <> e
            <> "'. Accepted values: basic_auth"

instance ToText SourceAccessType where
  toText = \case
    BasicAuth -> "BASIC_AUTH"

instance Hashable SourceAccessType

instance NFData SourceAccessType

instance ToByteString SourceAccessType

instance ToQuery SourceAccessType

instance ToHeader SourceAccessType

instance ToJSON SourceAccessType where
  toJSON = toJSONText

instance FromJSON SourceAccessType where
  parseJSON = parseJSONText "SourceAccessType"
