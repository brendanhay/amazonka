{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.DialogActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.DialogActionType where

import Network.AWS.Prelude

data DialogActionType
  = DATClose
  | DATConfirmIntent
  | DATDelegate
  | DATElicitIntent
  | DATElicitSlot
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

instance FromText DialogActionType where
  parser =
    takeLowerText >>= \case
      "close" -> pure DATClose
      "confirmintent" -> pure DATConfirmIntent
      "delegate" -> pure DATDelegate
      "elicitintent" -> pure DATElicitIntent
      "elicitslot" -> pure DATElicitSlot
      e ->
        fromTextError $
          "Failure parsing DialogActionType from value: '" <> e
            <> "'. Accepted values: close, confirmintent, delegate, elicitintent, elicitslot"

instance ToText DialogActionType where
  toText = \case
    DATClose -> "Close"
    DATConfirmIntent -> "ConfirmIntent"
    DATDelegate -> "Delegate"
    DATElicitIntent -> "ElicitIntent"
    DATElicitSlot -> "ElicitSlot"

instance Hashable DialogActionType

instance NFData DialogActionType

instance ToByteString DialogActionType

instance ToQuery DialogActionType

instance ToHeader DialogActionType

instance ToJSON DialogActionType where
  toJSON = toJSONText

instance FromJSON DialogActionType where
  parseJSON = parseJSONText "DialogActionType"
