{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ConfirmationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ConfirmationStatus where

import Network.AWS.Prelude

data ConfirmationStatus
  = Confirmed
  | Denied
  | None
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

instance FromText ConfirmationStatus where
  parser =
    takeLowerText >>= \case
      "confirmed" -> pure Confirmed
      "denied" -> pure Denied
      "none" -> pure None
      e ->
        fromTextError $
          "Failure parsing ConfirmationStatus from value: '" <> e
            <> "'. Accepted values: confirmed, denied, none"

instance ToText ConfirmationStatus where
  toText = \case
    Confirmed -> "Confirmed"
    Denied -> "Denied"
    None -> "None"

instance Hashable ConfirmationStatus

instance NFData ConfirmationStatus

instance ToByteString ConfirmationStatus

instance ToQuery ConfirmationStatus

instance ToHeader ConfirmationStatus

instance ToJSON ConfirmationStatus where
  toJSON = toJSONText

instance FromJSON ConfirmationStatus where
  parseJSON = parseJSONText "ConfirmationStatus"
