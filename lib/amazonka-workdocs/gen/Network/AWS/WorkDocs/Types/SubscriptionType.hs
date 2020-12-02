{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.SubscriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.SubscriptionType where

import Network.AWS.Prelude

data SubscriptionType = STAll
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
      "all" -> pure STAll
      e ->
        fromTextError $
          "Failure parsing SubscriptionType from value: '" <> e
            <> "'. Accepted values: all"

instance ToText SubscriptionType where
  toText = \case
    STAll -> "ALL"

instance Hashable SubscriptionType

instance NFData SubscriptionType

instance ToByteString SubscriptionType

instance ToQuery SubscriptionType

instance ToHeader SubscriptionType

instance ToJSON SubscriptionType where
  toJSON = toJSONText
