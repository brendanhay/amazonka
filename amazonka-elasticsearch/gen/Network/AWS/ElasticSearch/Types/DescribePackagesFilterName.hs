{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
  ( DescribePackagesFilterName
      ( ..,
        DescribePackagesFilterName_PackageID,
        DescribePackagesFilterName_PackageName,
        DescribePackagesFilterName_PackageStatus
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DescribePackagesFilterName = DescribePackagesFilterName'
  { fromDescribePackagesFilterName ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DescribePackagesFilterName_PackageID :: DescribePackagesFilterName
pattern DescribePackagesFilterName_PackageID = DescribePackagesFilterName' "PackageID"

pattern DescribePackagesFilterName_PackageName :: DescribePackagesFilterName
pattern DescribePackagesFilterName_PackageName = DescribePackagesFilterName' "PackageName"

pattern DescribePackagesFilterName_PackageStatus :: DescribePackagesFilterName
pattern DescribePackagesFilterName_PackageStatus = DescribePackagesFilterName' "PackageStatus"

{-# COMPLETE
  DescribePackagesFilterName_PackageID,
  DescribePackagesFilterName_PackageName,
  DescribePackagesFilterName_PackageStatus,
  DescribePackagesFilterName'
  #-}
