{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.PlayerSessionCreationPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.PlayerSessionCreationPolicy where

import Network.AWS.Prelude

data PlayerSessionCreationPolicy
  = AcceptAll
  | DenyAll
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

instance FromText PlayerSessionCreationPolicy where
  parser =
    takeLowerText >>= \case
      "accept_all" -> pure AcceptAll
      "deny_all" -> pure DenyAll
      e ->
        fromTextError $
          "Failure parsing PlayerSessionCreationPolicy from value: '" <> e
            <> "'. Accepted values: accept_all, deny_all"

instance ToText PlayerSessionCreationPolicy where
  toText = \case
    AcceptAll -> "ACCEPT_ALL"
    DenyAll -> "DENY_ALL"

instance Hashable PlayerSessionCreationPolicy

instance NFData PlayerSessionCreationPolicy

instance ToByteString PlayerSessionCreationPolicy

instance ToQuery PlayerSessionCreationPolicy

instance ToHeader PlayerSessionCreationPolicy

instance ToJSON PlayerSessionCreationPolicy where
  toJSON = toJSONText

instance FromJSON PlayerSessionCreationPolicy where
  parseJSON = parseJSONText "PlayerSessionCreationPolicy"
