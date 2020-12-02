{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.SubscriberType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.SubscriberType where

import Network.AWS.Prelude

data SubscriberType
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

instance FromText SubscriberType where
  parser =
    takeLowerText >>= \case
      "email" -> pure Email
      "sns" -> pure SNS
      e ->
        fromTextError $
          "Failure parsing SubscriberType from value: '" <> e
            <> "'. Accepted values: email, sns"

instance ToText SubscriberType where
  toText = \case
    Email -> "EMAIL"
    SNS -> "SNS"

instance Hashable SubscriberType

instance NFData SubscriberType

instance ToByteString SubscriberType

instance ToQuery SubscriberType

instance ToHeader SubscriberType

instance ToJSON SubscriberType where
  toJSON = toJSONText

instance FromJSON SubscriberType where
  parseJSON = parseJSONText "SubscriberType"
