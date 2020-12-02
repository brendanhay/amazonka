{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.NodeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.NodeType where

import Network.AWS.Prelude

data NodeType
  = Crawler
  | Job
  | Trigger
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

instance FromText NodeType where
  parser =
    takeLowerText >>= \case
      "crawler" -> pure Crawler
      "job" -> pure Job
      "trigger" -> pure Trigger
      e ->
        fromTextError $
          "Failure parsing NodeType from value: '" <> e
            <> "'. Accepted values: crawler, job, trigger"

instance ToText NodeType where
  toText = \case
    Crawler -> "CRAWLER"
    Job -> "JOB"
    Trigger -> "TRIGGER"

instance Hashable NodeType

instance NFData NodeType

instance ToByteString NodeType

instance ToQuery NodeType

instance ToHeader NodeType

instance FromJSON NodeType where
  parseJSON = parseJSONText "NodeType"
