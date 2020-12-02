{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DescribePackagesFilterName where

import Network.AWS.Prelude

data DescribePackagesFilterName
  = PackageId
  | PackageName
  | PackageStatus
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

instance FromText DescribePackagesFilterName where
  parser =
    takeLowerText >>= \case
      "packageid" -> pure PackageId
      "packagename" -> pure PackageName
      "packagestatus" -> pure PackageStatus
      e ->
        fromTextError $
          "Failure parsing DescribePackagesFilterName from value: '" <> e
            <> "'. Accepted values: packageid, packagename, packagestatus"

instance ToText DescribePackagesFilterName where
  toText = \case
    PackageId -> "PackageID"
    PackageName -> "PackageName"
    PackageStatus -> "PackageStatus"

instance Hashable DescribePackagesFilterName

instance NFData DescribePackagesFilterName

instance ToByteString DescribePackagesFilterName

instance ToQuery DescribePackagesFilterName

instance ToHeader DescribePackagesFilterName

instance ToJSON DescribePackagesFilterName where
  toJSON = toJSONText
