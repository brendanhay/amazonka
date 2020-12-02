{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.OptionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.OptionState where

import Network.AWS.Prelude

-- | The state of processing a change to an option. One of:
--
--
--     * RequiresIndexDocuments: The option's latest value will not be deployed until 'IndexDocuments' has been called and indexing is complete.    * Processing: The option's latest value is in the process of being activated.    * Active: The option's latest value is fully deployed.     * FailedToValidate: The option value is not compatible with the domain's data and cannot be used to index the data. You must either modify the option value or update or remove the incompatible documents.
data OptionState
  = Active
  | FailedToValidate
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
      "failedtovalidate" -> pure FailedToValidate
      "processing" -> pure Processing
      "requiresindexdocuments" -> pure RequiresIndexDocuments
      e ->
        fromTextError $
          "Failure parsing OptionState from value: '" <> e
            <> "'. Accepted values: active, failedtovalidate, processing, requiresindexdocuments"

instance ToText OptionState where
  toText = \case
    Active -> "Active"
    FailedToValidate -> "FailedToValidate"
    Processing -> "Processing"
    RequiresIndexDocuments -> "RequiresIndexDocuments"

instance Hashable OptionState

instance NFData OptionState

instance ToByteString OptionState

instance ToQuery OptionState

instance ToHeader OptionState

instance FromXML OptionState where
  parseXML = parseXMLText "OptionState"
