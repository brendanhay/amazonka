{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionFilterKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionFilterKey where

import Network.AWS.Prelude

data SessionFilterKey
  = SFKInvokedAfter
  | SFKInvokedBefore
  | SFKOwner
  | SFKSessionId
  | SFKStatus
  | SFKTarget
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

instance FromText SessionFilterKey where
  parser =
    takeLowerText >>= \case
      "invokedafter" -> pure SFKInvokedAfter
      "invokedbefore" -> pure SFKInvokedBefore
      "owner" -> pure SFKOwner
      "sessionid" -> pure SFKSessionId
      "status" -> pure SFKStatus
      "target" -> pure SFKTarget
      e ->
        fromTextError $
          "Failure parsing SessionFilterKey from value: '" <> e
            <> "'. Accepted values: invokedafter, invokedbefore, owner, sessionid, status, target"

instance ToText SessionFilterKey where
  toText = \case
    SFKInvokedAfter -> "InvokedAfter"
    SFKInvokedBefore -> "InvokedBefore"
    SFKOwner -> "Owner"
    SFKSessionId -> "SessionId"
    SFKStatus -> "Status"
    SFKTarget -> "Target"

instance Hashable SessionFilterKey

instance NFData SessionFilterKey

instance ToByteString SessionFilterKey

instance ToQuery SessionFilterKey

instance ToHeader SessionFilterKey

instance ToJSON SessionFilterKey where
  toJSON = toJSONText
