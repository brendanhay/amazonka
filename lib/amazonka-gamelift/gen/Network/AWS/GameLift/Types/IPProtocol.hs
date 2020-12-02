{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.IPProtocol
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.IPProtocol where

import Network.AWS.Prelude

data IPProtocol
  = TCP
  | Udp
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

instance FromText IPProtocol where
  parser =
    takeLowerText >>= \case
      "tcp" -> pure TCP
      "udp" -> pure Udp
      e ->
        fromTextError $
          "Failure parsing IPProtocol from value: '" <> e
            <> "'. Accepted values: tcp, udp"

instance ToText IPProtocol where
  toText = \case
    TCP -> "TCP"
    Udp -> "UDP"

instance Hashable IPProtocol

instance NFData IPProtocol

instance ToByteString IPProtocol

instance ToQuery IPProtocol

instance ToHeader IPProtocol

instance ToJSON IPProtocol where
  toJSON = toJSONText

instance FromJSON IPProtocol where
  parseJSON = parseJSONText "IPProtocol"
