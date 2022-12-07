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
-- Module      : Amazonka.ElasticSearch.Types.DescribePackagesFilterName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.DescribePackagesFilterName
  ( DescribePackagesFilterName
      ( ..,
        DescribePackagesFilterName_PackageID,
        DescribePackagesFilterName_PackageName,
        DescribePackagesFilterName_PackageStatus
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DescribePackagesFilterName = DescribePackagesFilterName'
  { fromDescribePackagesFilterName ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
