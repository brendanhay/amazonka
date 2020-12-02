{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServiceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ServiceType where

import Network.AWS.Prelude

data ServiceType
  = CredentialProvider
  | Data
  | Jobs
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

instance FromText ServiceType where
  parser =
    takeLowerText >>= \case
      "credential_provider" -> pure CredentialProvider
      "data" -> pure Data
      "jobs" -> pure Jobs
      e ->
        fromTextError $
          "Failure parsing ServiceType from value: '" <> e
            <> "'. Accepted values: credential_provider, data, jobs"

instance ToText ServiceType where
  toText = \case
    CredentialProvider -> "CREDENTIAL_PROVIDER"
    Data -> "DATA"
    Jobs -> "JOBS"

instance Hashable ServiceType

instance NFData ServiceType

instance ToByteString ServiceType

instance ToQuery ServiceType

instance ToHeader ServiceType

instance ToJSON ServiceType where
  toJSON = toJSONText

instance FromJSON ServiceType where
  parseJSON = parseJSONText "ServiceType"
