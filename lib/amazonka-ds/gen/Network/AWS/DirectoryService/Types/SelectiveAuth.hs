{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SelectiveAuth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SelectiveAuth where

import Network.AWS.Prelude

data SelectiveAuth
  = Disabled
  | Enabled
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

instance FromText SelectiveAuth where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing SelectiveAuth from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText SelectiveAuth where
  toText = \case
    Disabled -> "Disabled"
    Enabled -> "Enabled"

instance Hashable SelectiveAuth

instance NFData SelectiveAuth

instance ToByteString SelectiveAuth

instance ToQuery SelectiveAuth

instance ToHeader SelectiveAuth

instance ToJSON SelectiveAuth where
  toJSON = toJSONText

instance FromJSON SelectiveAuth where
  parseJSON = parseJSONText "SelectiveAuth"
