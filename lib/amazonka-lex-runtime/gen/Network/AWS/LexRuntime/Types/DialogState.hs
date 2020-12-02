{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.DialogState where

import Network.AWS.Prelude

data DialogState
  = ConfirmIntent
  | ElicitIntent
  | ElicitSlot
  | Failed
  | Fulfilled
  | ReadyForFulfillment
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

instance FromText DialogState where
  parser =
    takeLowerText >>= \case
      "confirmintent" -> pure ConfirmIntent
      "elicitintent" -> pure ElicitIntent
      "elicitslot" -> pure ElicitSlot
      "failed" -> pure Failed
      "fulfilled" -> pure Fulfilled
      "readyforfulfillment" -> pure ReadyForFulfillment
      e ->
        fromTextError $
          "Failure parsing DialogState from value: '" <> e
            <> "'. Accepted values: confirmintent, elicitintent, elicitslot, failed, fulfilled, readyforfulfillment"

instance ToText DialogState where
  toText = \case
    ConfirmIntent -> "ConfirmIntent"
    ElicitIntent -> "ElicitIntent"
    ElicitSlot -> "ElicitSlot"
    Failed -> "Failed"
    Fulfilled -> "Fulfilled"
    ReadyForFulfillment -> "ReadyForFulfillment"

instance Hashable DialogState

instance NFData DialogState

instance ToByteString DialogState

instance ToQuery DialogState

instance ToHeader DialogState

instance FromJSON DialogState where
  parseJSON = parseJSONText "DialogState"
