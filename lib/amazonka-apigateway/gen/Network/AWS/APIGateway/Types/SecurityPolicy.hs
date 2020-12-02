{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.SecurityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SecurityPolicy where

import Network.AWS.Prelude

data SecurityPolicy
  = TLS10
  | TLS12
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

instance FromText SecurityPolicy where
  parser =
    takeLowerText >>= \case
      "tls_1_0" -> pure TLS10
      "tls_1_2" -> pure TLS12
      e ->
        fromTextError $
          "Failure parsing SecurityPolicy from value: '" <> e
            <> "'. Accepted values: tls_1_0, tls_1_2"

instance ToText SecurityPolicy where
  toText = \case
    TLS10 -> "TLS_1_0"
    TLS12 -> "TLS_1_2"

instance Hashable SecurityPolicy

instance NFData SecurityPolicy

instance ToByteString SecurityPolicy

instance ToQuery SecurityPolicy

instance ToHeader SecurityPolicy

instance ToJSON SecurityPolicy where
  toJSON = toJSONText

instance FromJSON SecurityPolicy where
  parseJSON = parseJSONText "SecurityPolicy"
