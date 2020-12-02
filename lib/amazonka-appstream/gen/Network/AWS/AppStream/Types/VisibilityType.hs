{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.VisibilityType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.VisibilityType where

import Network.AWS.Prelude

data VisibilityType
  = Private
  | Public
  | Shared
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

instance FromText VisibilityType where
  parser =
    takeLowerText >>= \case
      "private" -> pure Private
      "public" -> pure Public
      "shared" -> pure Shared
      e ->
        fromTextError $
          "Failure parsing VisibilityType from value: '" <> e
            <> "'. Accepted values: private, public, shared"

instance ToText VisibilityType where
  toText = \case
    Private -> "PRIVATE"
    Public -> "PUBLIC"
    Shared -> "SHARED"

instance Hashable VisibilityType

instance NFData VisibilityType

instance ToByteString VisibilityType

instance ToQuery VisibilityType

instance ToHeader VisibilityType

instance ToJSON VisibilityType where
  toJSON = toJSONText

instance FromJSON VisibilityType where
  parseJSON = parseJSONText "VisibilityType"
