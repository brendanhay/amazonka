{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RateKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RateKey where

import Network.AWS.Prelude

data RateKey = IP
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

instance FromText RateKey where
  parser =
    takeLowerText >>= \case
      "ip" -> pure IP
      e ->
        fromTextError $
          "Failure parsing RateKey from value: '" <> e
            <> "'. Accepted values: ip"

instance ToText RateKey where
  toText = \case
    IP -> "IP"

instance Hashable RateKey

instance NFData RateKey

instance ToByteString RateKey

instance ToQuery RateKey

instance ToHeader RateKey

instance ToJSON RateKey where
  toJSON = toJSONText

instance FromJSON RateKey where
  parseJSON = parseJSONText "RateKey"
