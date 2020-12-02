{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CurrencyCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.CurrencyCode where

import Network.AWS.Prelude

data CurrencyCode = Usd
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

instance FromText CurrencyCode where
  parser =
    takeLowerText >>= \case
      "usd" -> pure Usd
      e ->
        fromTextError $
          "Failure parsing CurrencyCode from value: '" <> e
            <> "'. Accepted values: usd"

instance ToText CurrencyCode where
  toText = \case
    Usd -> "USD"

instance Hashable CurrencyCode

instance NFData CurrencyCode

instance ToByteString CurrencyCode

instance ToQuery CurrencyCode

instance ToHeader CurrencyCode

instance FromJSON CurrencyCode where
  parseJSON = parseJSONText "CurrencyCode"
