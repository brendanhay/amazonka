{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransactionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingTransactionType where

import Network.AWS.Prelude

data OfferingTransactionType
  = Purchase
  | Renew
  | System
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

instance FromText OfferingTransactionType where
  parser =
    takeLowerText >>= \case
      "purchase" -> pure Purchase
      "renew" -> pure Renew
      "system" -> pure System
      e ->
        fromTextError $
          "Failure parsing OfferingTransactionType from value: '" <> e
            <> "'. Accepted values: purchase, renew, system"

instance ToText OfferingTransactionType where
  toText = \case
    Purchase -> "PURCHASE"
    Renew -> "RENEW"
    System -> "SYSTEM"

instance Hashable OfferingTransactionType

instance NFData OfferingTransactionType

instance ToByteString OfferingTransactionType

instance ToQuery OfferingTransactionType

instance ToHeader OfferingTransactionType

instance FromJSON OfferingTransactionType where
  parseJSON = parseJSONText "OfferingTransactionType"
