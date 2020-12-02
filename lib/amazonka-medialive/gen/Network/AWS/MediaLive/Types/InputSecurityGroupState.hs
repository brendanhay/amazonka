{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSecurityGroupState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSecurityGroupState where

import Network.AWS.Prelude

-- | Placeholder documentation for InputSecurityGroupState
data InputSecurityGroupState
  = ISGSDeleted
  | ISGSIdle
  | ISGSInUse
  | ISGSUpdating
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

instance FromText InputSecurityGroupState where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure ISGSDeleted
      "idle" -> pure ISGSIdle
      "in_use" -> pure ISGSInUse
      "updating" -> pure ISGSUpdating
      e ->
        fromTextError $
          "Failure parsing InputSecurityGroupState from value: '" <> e
            <> "'. Accepted values: deleted, idle, in_use, updating"

instance ToText InputSecurityGroupState where
  toText = \case
    ISGSDeleted -> "DELETED"
    ISGSIdle -> "IDLE"
    ISGSInUse -> "IN_USE"
    ISGSUpdating -> "UPDATING"

instance Hashable InputSecurityGroupState

instance NFData InputSecurityGroupState

instance ToByteString InputSecurityGroupState

instance ToQuery InputSecurityGroupState

instance ToHeader InputSecurityGroupState

instance FromJSON InputSecurityGroupState where
  parseJSON = parseJSONText "InputSecurityGroupState"
