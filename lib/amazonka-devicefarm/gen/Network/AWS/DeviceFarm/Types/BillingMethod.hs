{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.BillingMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.BillingMethod where

import Network.AWS.Prelude

data BillingMethod
  = Metered
  | Unmetered
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

instance FromText BillingMethod where
  parser =
    takeLowerText >>= \case
      "metered" -> pure Metered
      "unmetered" -> pure Unmetered
      e ->
        fromTextError $
          "Failure parsing BillingMethod from value: '" <> e
            <> "'. Accepted values: metered, unmetered"

instance ToText BillingMethod where
  toText = \case
    Metered -> "METERED"
    Unmetered -> "UNMETERED"

instance Hashable BillingMethod

instance NFData BillingMethod

instance ToByteString BillingMethod

instance ToQuery BillingMethod

instance ToHeader BillingMethod

instance ToJSON BillingMethod where
  toJSON = toJSONText

instance FromJSON BillingMethod where
  parseJSON = parseJSONText "BillingMethod"
