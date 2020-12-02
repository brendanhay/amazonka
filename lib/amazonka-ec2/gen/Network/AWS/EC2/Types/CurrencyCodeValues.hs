{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CurrencyCodeValues
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CurrencyCodeValues where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CurrencyCodeValues = Usd
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

instance FromText CurrencyCodeValues where
  parser =
    takeLowerText >>= \case
      "usd" -> pure Usd
      e ->
        fromTextError $
          "Failure parsing CurrencyCodeValues from value: '" <> e
            <> "'. Accepted values: usd"

instance ToText CurrencyCodeValues where
  toText = \case
    Usd -> "USD"

instance Hashable CurrencyCodeValues

instance NFData CurrencyCodeValues

instance ToByteString CurrencyCodeValues

instance ToQuery CurrencyCodeValues

instance ToHeader CurrencyCodeValues

instance FromXML CurrencyCodeValues where
  parseXML = parseXMLText "CurrencyCodeValues"
