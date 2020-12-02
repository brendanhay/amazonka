{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupState where

import Network.AWS.Prelude

data WorkGroupState
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

instance FromText WorkGroupState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing WorkGroupState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText WorkGroupState where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable WorkGroupState

instance NFData WorkGroupState

instance ToByteString WorkGroupState

instance ToQuery WorkGroupState

instance ToHeader WorkGroupState

instance ToJSON WorkGroupState where
  toJSON = toJSONText

instance FromJSON WorkGroupState where
  parseJSON = parseJSONText "WorkGroupState"
