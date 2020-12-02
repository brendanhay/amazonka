{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStep
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStep where

import Network.AWS.Prelude

data UpgradeStep
  = PreUpgradeCheck
  | Snapshot
  | Upgrade
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

instance FromText UpgradeStep where
  parser =
    takeLowerText >>= \case
      "pre_upgrade_check" -> pure PreUpgradeCheck
      "snapshot" -> pure Snapshot
      "upgrade" -> pure Upgrade
      e ->
        fromTextError $
          "Failure parsing UpgradeStep from value: '" <> e
            <> "'. Accepted values: pre_upgrade_check, snapshot, upgrade"

instance ToText UpgradeStep where
  toText = \case
    PreUpgradeCheck -> "PRE_UPGRADE_CHECK"
    Snapshot -> "SNAPSHOT"
    Upgrade -> "UPGRADE"

instance Hashable UpgradeStep

instance NFData UpgradeStep

instance ToByteString UpgradeStep

instance ToQuery UpgradeStep

instance ToHeader UpgradeStep

instance FromJSON UpgradeStep where
  parseJSON = parseJSONText "UpgradeStep"
