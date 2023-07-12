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
-- Module      : Amazonka.CloudSearch.Types.PartitionInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.PartitionInstanceType
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
        PartitionInstanceType_Search_previousgeneration_2xlarge,
        PartitionInstanceType_Search_previousgeneration_large,
        PartitionInstanceType_Search_previousgeneration_small,
        PartitionInstanceType_Search_previousgeneration_xlarge,
        PartitionInstanceType_Search_small,
        PartitionInstanceType_Search_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The instance type (such as @search.m1.small@) on which an index
-- partition is hosted.
newtype PartitionInstanceType = PartitionInstanceType'
  { fromPartitionInstanceType ::
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

pattern PartitionInstanceType_Search_previousgeneration_2xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_previousgeneration_2xlarge = PartitionInstanceType' "search.previousgeneration.2xlarge"

pattern PartitionInstanceType_Search_previousgeneration_large :: PartitionInstanceType
pattern PartitionInstanceType_Search_previousgeneration_large = PartitionInstanceType' "search.previousgeneration.large"

pattern PartitionInstanceType_Search_previousgeneration_small :: PartitionInstanceType
pattern PartitionInstanceType_Search_previousgeneration_small = PartitionInstanceType' "search.previousgeneration.small"

pattern PartitionInstanceType_Search_previousgeneration_xlarge :: PartitionInstanceType
pattern PartitionInstanceType_Search_previousgeneration_xlarge = PartitionInstanceType' "search.previousgeneration.xlarge"

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
  PartitionInstanceType_Search_previousgeneration_2xlarge,
  PartitionInstanceType_Search_previousgeneration_large,
  PartitionInstanceType_Search_previousgeneration_small,
  PartitionInstanceType_Search_previousgeneration_xlarge,
  PartitionInstanceType_Search_small,
  PartitionInstanceType_Search_xlarge,
  PartitionInstanceType'
  #-}
