{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeState where

import Network.AWS.Prelude

data HandshakeState
  = Accepted
  | Canceled
  | Declined
  | Expired
  | Open
  | Requested
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

instance FromText HandshakeState where
  parser =
    takeLowerText >>= \case
      "accepted" -> pure Accepted
      "canceled" -> pure Canceled
      "declined" -> pure Declined
      "expired" -> pure Expired
      "open" -> pure Open
      "requested" -> pure Requested
      e ->
        fromTextError $
          "Failure parsing HandshakeState from value: '" <> e
            <> "'. Accepted values: accepted, canceled, declined, expired, open, requested"

instance ToText HandshakeState where
  toText = \case
    Accepted -> "ACCEPTED"
    Canceled -> "CANCELED"
    Declined -> "DECLINED"
    Expired -> "EXPIRED"
    Open -> "OPEN"
    Requested -> "REQUESTED"

instance Hashable HandshakeState

instance NFData HandshakeState

instance ToByteString HandshakeState

instance ToQuery HandshakeState

instance ToHeader HandshakeState

instance FromJSON HandshakeState where
  parseJSON = parseJSONText "HandshakeState"
