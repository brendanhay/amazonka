{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ClusterField
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ClusterField where

import Network.AWS.Prelude

data ClusterField
  = Attachments
  | Settings
  | Statistics
  | Tags
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

instance FromText ClusterField where
  parser =
    takeLowerText >>= \case
      "attachments" -> pure Attachments
      "settings" -> pure Settings
      "statistics" -> pure Statistics
      "tags" -> pure Tags
      e ->
        fromTextError $
          "Failure parsing ClusterField from value: '" <> e
            <> "'. Accepted values: attachments, settings, statistics, tags"

instance ToText ClusterField where
  toText = \case
    Attachments -> "ATTACHMENTS"
    Settings -> "SETTINGS"
    Statistics -> "STATISTICS"
    Tags -> "TAGS"

instance Hashable ClusterField

instance NFData ClusterField

instance ToByteString ClusterField

instance ToQuery ClusterField

instance ToHeader ClusterField

instance ToJSON ClusterField where
  toJSON = toJSONText
