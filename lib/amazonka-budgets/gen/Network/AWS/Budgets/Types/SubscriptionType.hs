{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.SubscriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.SubscriptionType where

import Network.AWS.Prelude

-- | The subscription type of the subscriber. It can be SMS or EMAIL.
data SubscriptionType
  = Email
  | SNS
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

instance FromText SubscriptionType where
  parser =
    takeLowerText >>= \case
      "email" -> pure Email
      "sns" -> pure SNS
      e ->
        fromTextError $
          "Failure parsing SubscriptionType from value: '" <> e
            <> "'. Accepted values: email, sns"

instance ToText SubscriptionType where
  toText = \case
    Email -> "EMAIL"
    SNS -> "SNS"

instance Hashable SubscriptionType

instance NFData SubscriptionType

instance ToByteString SubscriptionType

instance ToQuery SubscriptionType

instance ToHeader SubscriptionType

instance ToJSON SubscriptionType where
  toJSON = toJSONText

instance FromJSON SubscriptionType where
  parseJSON = parseJSONText "SubscriptionType"
