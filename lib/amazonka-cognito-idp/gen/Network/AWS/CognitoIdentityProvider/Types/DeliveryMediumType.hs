{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType where

import Network.AWS.Prelude

data DeliveryMediumType
  = DMTEmail
  | DMTSms
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

instance FromText DeliveryMediumType where
  parser =
    takeLowerText >>= \case
      "email" -> pure DMTEmail
      "sms" -> pure DMTSms
      e ->
        fromTextError $
          "Failure parsing DeliveryMediumType from value: '" <> e
            <> "'. Accepted values: email, sms"

instance ToText DeliveryMediumType where
  toText = \case
    DMTEmail -> "EMAIL"
    DMTSms -> "SMS"

instance Hashable DeliveryMediumType

instance NFData DeliveryMediumType

instance ToByteString DeliveryMediumType

instance ToQuery DeliveryMediumType

instance ToHeader DeliveryMediumType

instance ToJSON DeliveryMediumType where
  toJSON = toJSONText

instance FromJSON DeliveryMediumType where
  parseJSON = parseJSONText "DeliveryMediumType"
