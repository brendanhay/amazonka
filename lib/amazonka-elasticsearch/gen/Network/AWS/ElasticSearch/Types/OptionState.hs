{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OptionState where

import Network.AWS.Prelude

-- | The state of a requested change. One of the following:
--
--
--     * Processing: The request change is still in-process.    * Active: The request change is processed and deployed to the Elasticsearch domain.
data OptionState
  = Active
  | Processing
  | RequiresIndexDocuments
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

instance FromText OptionState where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "processing" -> pure Processing
      "requiresindexdocuments" -> pure RequiresIndexDocuments
      e ->
        fromTextError $
          "Failure parsing OptionState from value: '" <> e
            <> "'. Accepted values: active, processing, requiresindexdocuments"

instance ToText OptionState where
  toText = \case
    Active -> "Active"
    Processing -> "Processing"
    RequiresIndexDocuments -> "RequiresIndexDocuments"

instance Hashable OptionState

instance NFData OptionState

instance ToByteString OptionState

instance ToQuery OptionState

instance ToHeader OptionState

instance FromJSON OptionState where
  parseJSON = parseJSONText "OptionState"
