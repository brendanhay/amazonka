{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsItemStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsItemStatus where

import Network.AWS.Prelude

data OpsItemStatus
  = InProgress
  | Open
  | Resolved
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

instance FromText OpsItemStatus where
  parser =
    takeLowerText >>= \case
      "inprogress" -> pure InProgress
      "open" -> pure Open
      "resolved" -> pure Resolved
      e ->
        fromTextError $
          "Failure parsing OpsItemStatus from value: '" <> e
            <> "'. Accepted values: inprogress, open, resolved"

instance ToText OpsItemStatus where
  toText = \case
    InProgress -> "InProgress"
    Open -> "Open"
    Resolved -> "Resolved"

instance Hashable OpsItemStatus

instance NFData OpsItemStatus

instance ToByteString OpsItemStatus

instance ToQuery OpsItemStatus

instance ToHeader OpsItemStatus

instance ToJSON OpsItemStatus where
  toJSON = toJSONText

instance FromJSON OpsItemStatus where
  parseJSON = parseJSONText "OpsItemStatus"
