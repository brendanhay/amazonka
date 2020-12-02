{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConfigurationSyncStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConfigurationSyncStatus where

import Network.AWS.Prelude

data ConfigurationSyncStatus
  = InSync
  | OutOfSync
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

instance FromText ConfigurationSyncStatus where
  parser =
    takeLowerText >>= \case
      "insync" -> pure InSync
      "outofsync" -> pure OutOfSync
      e ->
        fromTextError $
          "Failure parsing ConfigurationSyncStatus from value: '" <> e
            <> "'. Accepted values: insync, outofsync"

instance ToText ConfigurationSyncStatus where
  toText = \case
    InSync -> "InSync"
    OutOfSync -> "OutOfSync"

instance Hashable ConfigurationSyncStatus

instance NFData ConfigurationSyncStatus

instance ToByteString ConfigurationSyncStatus

instance ToQuery ConfigurationSyncStatus

instance ToHeader ConfigurationSyncStatus

instance FromJSON ConfigurationSyncStatus where
  parseJSON = parseJSONText "ConfigurationSyncStatus"
