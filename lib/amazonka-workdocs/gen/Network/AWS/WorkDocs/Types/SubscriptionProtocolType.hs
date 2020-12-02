{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SubscriptionProtocolType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.SubscriptionProtocolType where

import Network.AWS.Prelude

data SubscriptionProtocolType = HTTPS
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

instance FromText SubscriptionProtocolType where
  parser =
    takeLowerText >>= \case
      "https" -> pure HTTPS
      e ->
        fromTextError $
          "Failure parsing SubscriptionProtocolType from value: '" <> e
            <> "'. Accepted values: https"

instance ToText SubscriptionProtocolType where
  toText = \case
    HTTPS -> "HTTPS"

instance Hashable SubscriptionProtocolType

instance NFData SubscriptionProtocolType

instance ToByteString SubscriptionProtocolType

instance ToQuery SubscriptionProtocolType

instance ToHeader SubscriptionProtocolType

instance ToJSON SubscriptionProtocolType where
  toJSON = toJSONText

instance FromJSON SubscriptionProtocolType where
  parseJSON = parseJSONText "SubscriptionProtocolType"
