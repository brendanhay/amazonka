{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppReplicationConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppReplicationConfigurationStatus where

import Network.AWS.Prelude

data AppReplicationConfigurationStatus
  = Configured
  | NotConfigured
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

instance FromText AppReplicationConfigurationStatus where
  parser =
    takeLowerText >>= \case
      "configured" -> pure Configured
      "not_configured" -> pure NotConfigured
      e ->
        fromTextError $
          "Failure parsing AppReplicationConfigurationStatus from value: '" <> e
            <> "'. Accepted values: configured, not_configured"

instance ToText AppReplicationConfigurationStatus where
  toText = \case
    Configured -> "CONFIGURED"
    NotConfigured -> "NOT_CONFIGURED"

instance Hashable AppReplicationConfigurationStatus

instance NFData AppReplicationConfigurationStatus

instance ToByteString AppReplicationConfigurationStatus

instance ToQuery AppReplicationConfigurationStatus

instance ToHeader AppReplicationConfigurationStatus

instance FromJSON AppReplicationConfigurationStatus where
  parseJSON = parseJSONText "AppReplicationConfigurationStatus"
