{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BillingMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BillingMode where

import Network.AWS.Prelude

data BillingMode
  = PayPerRequest
  | Provisioned
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

instance FromText BillingMode where
  parser =
    takeLowerText >>= \case
      "pay_per_request" -> pure PayPerRequest
      "provisioned" -> pure Provisioned
      e ->
        fromTextError $
          "Failure parsing BillingMode from value: '" <> e
            <> "'. Accepted values: pay_per_request, provisioned"

instance ToText BillingMode where
  toText = \case
    PayPerRequest -> "PAY_PER_REQUEST"
    Provisioned -> "PROVISIONED"

instance Hashable BillingMode

instance NFData BillingMode

instance ToByteString BillingMode

instance ToQuery BillingMode

instance ToHeader BillingMode

instance ToJSON BillingMode where
  toJSON = toJSONText

instance FromJSON BillingMode where
  parseJSON = parseJSONText "BillingMode"
