{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerStatus where

import Network.AWS.Prelude

data AuthorizerStatus
  = Active
  | Inactive
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

instance FromText AuthorizerStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "inactive" -> pure Inactive
      e ->
        fromTextError $
          "Failure parsing AuthorizerStatus from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText AuthorizerStatus where
  toText = \case
    Active -> "ACTIVE"
    Inactive -> "INACTIVE"

instance Hashable AuthorizerStatus

instance NFData AuthorizerStatus

instance ToByteString AuthorizerStatus

instance ToQuery AuthorizerStatus

instance ToHeader AuthorizerStatus

instance ToJSON AuthorizerStatus where
  toJSON = toJSONText

instance FromJSON AuthorizerStatus where
  parseJSON = parseJSONText "AuthorizerStatus"
