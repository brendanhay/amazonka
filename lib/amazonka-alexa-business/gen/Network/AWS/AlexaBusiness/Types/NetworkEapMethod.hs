{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.NetworkEapMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.NetworkEapMethod where

import Network.AWS.Prelude

data NetworkEapMethod = EapTLS
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

instance FromText NetworkEapMethod where
  parser =
    takeLowerText >>= \case
      "eap_tls" -> pure EapTLS
      e ->
        fromTextError $
          "Failure parsing NetworkEapMethod from value: '" <> e
            <> "'. Accepted values: eap_tls"

instance ToText NetworkEapMethod where
  toText = \case
    EapTLS -> "EAP_TLS"

instance Hashable NetworkEapMethod

instance NFData NetworkEapMethod

instance ToByteString NetworkEapMethod

instance ToQuery NetworkEapMethod

instance ToHeader NetworkEapMethod

instance ToJSON NetworkEapMethod where
  toJSON = toJSONText

instance FromJSON NetworkEapMethod where
  parseJSON = parseJSONText "NetworkEapMethod"
