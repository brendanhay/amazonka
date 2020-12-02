{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.TLSSecurityPolicy where

import Network.AWS.Prelude

data TLSSecurityPolicy
  = PolicyMinTLS10201907
  | PolicyMinTLS12201907
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

instance FromText TLSSecurityPolicy where
  parser =
    takeLowerText >>= \case
      "policy-min-tls-1-0-2019-07" -> pure PolicyMinTLS10201907
      "policy-min-tls-1-2-2019-07" -> pure PolicyMinTLS12201907
      e ->
        fromTextError $
          "Failure parsing TLSSecurityPolicy from value: '" <> e
            <> "'. Accepted values: policy-min-tls-1-0-2019-07, policy-min-tls-1-2-2019-07"

instance ToText TLSSecurityPolicy where
  toText = \case
    PolicyMinTLS10201907 -> "Policy-Min-TLS-1-0-2019-07"
    PolicyMinTLS12201907 -> "Policy-Min-TLS-1-2-2019-07"

instance Hashable TLSSecurityPolicy

instance NFData TLSSecurityPolicy

instance ToByteString TLSSecurityPolicy

instance ToQuery TLSSecurityPolicy

instance ToHeader TLSSecurityPolicy

instance ToJSON TLSSecurityPolicy where
  toJSON = toJSONText

instance FromJSON TLSSecurityPolicy where
  parseJSON = parseJSONText "TLSSecurityPolicy"
