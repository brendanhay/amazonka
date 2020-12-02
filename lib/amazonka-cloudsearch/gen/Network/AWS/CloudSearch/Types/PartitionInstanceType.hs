{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.PartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.PartitionInstanceType where

import Network.AWS.Prelude

-- | The instance type (such as @search.m1.small@ ) on which an index partition is hosted.
data PartitionInstanceType
  = Search_M1_Large
  | Search_M1_Small
  | Search_M2_2XLarge
  | Search_M2_XLarge
  | Search_M3_2XLarge
  | Search_M3_Large
  | Search_M3_Medium
  | Search_M3_XLarge
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

instance FromText PartitionInstanceType where
  parser =
    takeLowerText >>= \case
      "search.m1.large" -> pure Search_M1_Large
      "search.m1.small" -> pure Search_M1_Small
      "search.m2.2xlarge" -> pure Search_M2_2XLarge
      "search.m2.xlarge" -> pure Search_M2_XLarge
      "search.m3.2xlarge" -> pure Search_M3_2XLarge
      "search.m3.large" -> pure Search_M3_Large
      "search.m3.medium" -> pure Search_M3_Medium
      "search.m3.xlarge" -> pure Search_M3_XLarge
      e ->
        fromTextError $
          "Failure parsing PartitionInstanceType from value: '" <> e
            <> "'. Accepted values: search.m1.large, search.m1.small, search.m2.2xlarge, search.m2.xlarge, search.m3.2xlarge, search.m3.large, search.m3.medium, search.m3.xlarge"

instance ToText PartitionInstanceType where
  toText = \case
    Search_M1_Large -> "search.m1.large"
    Search_M1_Small -> "search.m1.small"
    Search_M2_2XLarge -> "search.m2.2xlarge"
    Search_M2_XLarge -> "search.m2.xlarge"
    Search_M3_2XLarge -> "search.m3.2xlarge"
    Search_M3_Large -> "search.m3.large"
    Search_M3_Medium -> "search.m3.medium"
    Search_M3_XLarge -> "search.m3.xlarge"

instance Hashable PartitionInstanceType

instance NFData PartitionInstanceType

instance ToByteString PartitionInstanceType

instance ToQuery PartitionInstanceType

instance ToHeader PartitionInstanceType

instance FromXML PartitionInstanceType where
  parseXML = parseXMLText "PartitionInstanceType"
