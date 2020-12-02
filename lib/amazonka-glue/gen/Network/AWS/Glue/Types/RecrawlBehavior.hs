{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.RecrawlBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.RecrawlBehavior where

import Network.AWS.Prelude

data RecrawlBehavior
  = CrawlEverything
  | CrawlNewFoldersOnly
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

instance FromText RecrawlBehavior where
  parser =
    takeLowerText >>= \case
      "crawl_everything" -> pure CrawlEverything
      "crawl_new_folders_only" -> pure CrawlNewFoldersOnly
      e ->
        fromTextError $
          "Failure parsing RecrawlBehavior from value: '" <> e
            <> "'. Accepted values: crawl_everything, crawl_new_folders_only"

instance ToText RecrawlBehavior where
  toText = \case
    CrawlEverything -> "CRAWL_EVERYTHING"
    CrawlNewFoldersOnly -> "CRAWL_NEW_FOLDERS_ONLY"

instance Hashable RecrawlBehavior

instance NFData RecrawlBehavior

instance ToByteString RecrawlBehavior

instance ToQuery RecrawlBehavior

instance ToHeader RecrawlBehavior

instance ToJSON RecrawlBehavior where
  toJSON = toJSONText

instance FromJSON RecrawlBehavior where
  parseJSON = parseJSONText "RecrawlBehavior"
