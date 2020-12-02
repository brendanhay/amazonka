{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.SubscriptionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.SubscriptionType where

import Network.AWS.Prelude

-- | Specifies the type of subscription for the HSM.
--
--
--     * __PRODUCTION__ - The HSM is being used in a production environment.
--
--     * __TRIAL__ - The HSM is being used in a product trial.
data SubscriptionType = Production
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
      "production" -> pure Production
      e ->
        fromTextError $
          "Failure parsing SubscriptionType from value: '" <> e
            <> "'. Accepted values: production"

instance ToText SubscriptionType where
  toText = \case
    Production -> "PRODUCTION"

instance Hashable SubscriptionType

instance NFData SubscriptionType

instance ToByteString SubscriptionType

instance ToQuery SubscriptionType

instance ToHeader SubscriptionType

instance ToJSON SubscriptionType where
  toJSON = toJSONText

instance FromJSON SubscriptionType where
  parseJSON = parseJSONText "SubscriptionType"
