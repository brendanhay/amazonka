{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CrawlerLineageSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CrawlerLineageSettings where

import Network.AWS.Prelude

data CrawlerLineageSettings
  = Disable
  | Enable
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

instance FromText CrawlerLineageSettings where
  parser =
    takeLowerText >>= \case
      "disable" -> pure Disable
      "enable" -> pure Enable
      e ->
        fromTextError $
          "Failure parsing CrawlerLineageSettings from value: '" <> e
            <> "'. Accepted values: disable, enable"

instance ToText CrawlerLineageSettings where
  toText = \case
    Disable -> "DISABLE"
    Enable -> "ENABLE"

instance Hashable CrawlerLineageSettings

instance NFData CrawlerLineageSettings

instance ToByteString CrawlerLineageSettings

instance ToQuery CrawlerLineageSettings

instance ToHeader CrawlerLineageSettings

instance ToJSON CrawlerLineageSettings where
  toJSON = toJSONText

instance FromJSON CrawlerLineageSettings where
  parseJSON = parseJSONText "CrawlerLineageSettings"
