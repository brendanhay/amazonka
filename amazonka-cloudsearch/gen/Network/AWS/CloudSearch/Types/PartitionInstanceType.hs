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
-- Module      : Network.AWS.CloudSearch.Types.PartitionInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.PartitionInstanceType
  ( PartitionInstanceType
      ( ..,
        PartitionInstanceType_Search_2xlarge,
        PartitionInstanceType_Search_large,
        PartitionInstanceType_Search_m1_large,
        PartitionInstanceType_Search_m1_small,
        PartitionInstanceType_Search_m2_2xlarge,
        PartitionInstanceType_Search_m2_xlarge,
        PartitionInstanceType_Search_m3_2xlarge,
        PartitionInstanceType_Search_m3_large,
        PartitionInstanceType_Search_m3_medium,
        PartitionInstanceType_Search_m3_xlarge,
        PartitionInstanceType_Search_medium,
        PartitionInstanceType_Search_small,
        PartitionInstanceType_Search_xlarge
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The instance type (such as @search.m1.small@) on which an index
-- partition is hosted.
newtype PartitionInstanceType = PartitionInstanceType'
  { fromPartitionInstanceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern PartitionInstanceType_Search_2xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_2xlarge = PartitionInstanceType' "search.2xlarge"

pattern PartitionInstanceType_Search_large :: PartitionInstanceType
pattern PartitionInstanceType_Search_large = PartitionInstanceType' "search.large"

pattern PartitionInstanceType_Search_m1_large :: PartitionInstanceType
pattern PartitionInstanceType_Search_m1_large = PartitionInstanceType' "search.m1.large"

pattern PartitionInstanceType_Search_m1_small :: PartitionInstanceType
pattern PartitionInstanceType_Search_m1_small = PartitionInstanceType' "search.m1.small"

pattern PartitionInstanceType_Search_m2_2xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_m2_2xlarge = PartitionInstanceType' "search.m2.2xlarge"

pattern PartitionInstanceType_Search_m2_xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_m2_xlarge = PartitionInstanceType' "search.m2.xlarge"

pattern PartitionInstanceType_Search_m3_2xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_m3_2xlarge = PartitionInstanceType' "search.m3.2xlarge"

pattern PartitionInstanceType_Search_m3_large :: PartitionInstanceType
pattern PartitionInstanceType_Search_m3_large = PartitionInstanceType' "search.m3.large"

pattern PartitionInstanceType_Search_m3_medium :: PartitionInstanceType
pattern PartitionInstanceType_Search_m3_medium = PartitionInstanceType' "search.m3.medium"

pattern PartitionInstanceType_Search_m3_xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_m3_xlarge = PartitionInstanceType' "search.m3.xlarge"

pattern PartitionInstanceType_Search_medium :: PartitionInstanceType
pattern PartitionInstanceType_Search_medium = PartitionInstanceType' "search.medium"

pattern PartitionInstanceType_Search_small :: PartitionInstanceType
pattern PartitionInstanceType_Search_small = PartitionInstanceType' "search.small"

pattern PartitionInstanceType_Search_xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_xlarge = PartitionInstanceType' "search.xlarge"

{-# COMPLETE
  PartitionInstanceType_Search_2xlarge,
  PartitionInstanceType_Search_large,
  PartitionInstanceType_Search_m1_large,
  PartitionInstanceType_Search_m1_small,
  PartitionInstanceType_Search_m2_2xlarge,
  PartitionInstanceType_Search_m2_xlarge,
  PartitionInstanceType_Search_m3_2xlarge,
  PartitionInstanceType_Search_m3_large,
  PartitionInstanceType_Search_m3_medium,
  PartitionInstanceType_Search_m3_xlarge,
  PartitionInstanceType_Search_medium,
  PartitionInstanceType_Search_small,
  PartitionInstanceType_Search_xlarge,
  PartitionInstanceType'
  #-}
