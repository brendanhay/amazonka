{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.OrganizationFeatureSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.OrganizationFeatureSet where

import Network.AWS.Prelude

data OrganizationFeatureSet
  = All
  | ConsolidatedBilling
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

instance FromText OrganizationFeatureSet where
  parser =
    takeLowerText >>= \case
      "all" -> pure All
      "consolidated_billing" -> pure ConsolidatedBilling
      e ->
        fromTextError $
          "Failure parsing OrganizationFeatureSet from value: '" <> e
            <> "'. Accepted values: all, consolidated_billing"

instance ToText OrganizationFeatureSet where
  toText = \case
    All -> "ALL"
    ConsolidatedBilling -> "CONSOLIDATED_BILLING"

instance Hashable OrganizationFeatureSet

instance NFData OrganizationFeatureSet

instance ToByteString OrganizationFeatureSet

instance ToQuery OrganizationFeatureSet

instance ToHeader OrganizationFeatureSet

instance ToJSON OrganizationFeatureSet where
  toJSON = toJSONText

instance FromJSON OrganizationFeatureSet where
  parseJSON = parseJSONText "OrganizationFeatureSet"
