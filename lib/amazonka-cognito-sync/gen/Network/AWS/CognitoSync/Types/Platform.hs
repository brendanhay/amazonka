{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.Types.Platform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoSync.Types.Platform where

import Network.AWS.Prelude

data Platform
  = ADM
  | APNS
  | APNSSandbox
  | GCM
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

instance FromText Platform where
  parser =
    takeLowerText >>= \case
      "adm" -> pure ADM
      "apns" -> pure APNS
      "apns_sandbox" -> pure APNSSandbox
      "gcm" -> pure GCM
      e ->
        fromTextError $
          "Failure parsing Platform from value: '" <> e
            <> "'. Accepted values: adm, apns, apns_sandbox, gcm"

instance ToText Platform where
  toText = \case
    ADM -> "ADM"
    APNS -> "APNS"
    APNSSandbox -> "APNS_SANDBOX"
    GCM -> "GCM"

instance Hashable Platform

instance NFData Platform

instance ToByteString Platform

instance ToQuery Platform

instance ToHeader Platform

instance ToJSON Platform where
  toJSON = toJSONText
