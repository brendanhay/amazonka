{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.KeyState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.KeyState where

import Network.AWS.Prelude

data KeyState
  = Disabled
  | Enabled
  | PendingDeletion
  | PendingImport
  | Unavailable
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

instance FromText KeyState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      "pendingdeletion" -> pure PendingDeletion
      "pendingimport" -> pure PendingImport
      "unavailable" -> pure Unavailable
      e ->
        fromTextError $
          "Failure parsing KeyState from value: '" <> e
            <> "'. Accepted values: disabled, enabled, pendingdeletion, pendingimport, unavailable"

instance ToText KeyState where
  toText = \case
    Disabled -> "Disabled"
    Enabled -> "Enabled"
    PendingDeletion -> "PendingDeletion"
    PendingImport -> "PendingImport"
    Unavailable -> "Unavailable"

instance Hashable KeyState

instance NFData KeyState

instance ToByteString KeyState

instance ToQuery KeyState

instance ToHeader KeyState

instance FromJSON KeyState where
  parseJSON = parseJSONText "KeyState"
