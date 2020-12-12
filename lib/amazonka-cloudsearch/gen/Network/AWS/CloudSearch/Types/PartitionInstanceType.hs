{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.PartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.PartitionInstanceType
  ( PartitionInstanceType
      ( PartitionInstanceType',
        Search_M1_Large,
        Search_M1_Small,
        Search_M2_2XLarge,
        Search_M2_XLarge,
        Search_M3_2XLarge,
        Search_M3_Large,
        Search_M3_Medium,
        Search_M3_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The instance type (such as @search.m1.small@ ) on which an index partition is hosted.
newtype PartitionInstanceType = PartitionInstanceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Search_M1_Large :: PartitionInstanceType
pattern Search_M1_Large = PartitionInstanceType' "search.m1.large"

pattern Search_M1_Small :: PartitionInstanceType
pattern Search_M1_Small = PartitionInstanceType' "search.m1.small"

pattern Search_M2_2XLarge :: PartitionInstanceType
pattern Search_M2_2XLarge = PartitionInstanceType' "search.m2.2xlarge"

pattern Search_M2_XLarge :: PartitionInstanceType
pattern Search_M2_XLarge = PartitionInstanceType' "search.m2.xlarge"

pattern Search_M3_2XLarge :: PartitionInstanceType
pattern Search_M3_2XLarge = PartitionInstanceType' "search.m3.2xlarge"

pattern Search_M3_Large :: PartitionInstanceType
pattern Search_M3_Large = PartitionInstanceType' "search.m3.large"

pattern Search_M3_Medium :: PartitionInstanceType
pattern Search_M3_Medium = PartitionInstanceType' "search.m3.medium"

pattern Search_M3_XLarge :: PartitionInstanceType
pattern Search_M3_XLarge = PartitionInstanceType' "search.m3.xlarge"

{-# COMPLETE
  Search_M1_Large,
  Search_M1_Small,
  Search_M2_2XLarge,
  Search_M2_XLarge,
  Search_M3_2XLarge,
  Search_M3_Large,
  Search_M3_Medium,
  Search_M3_XLarge,
  PartitionInstanceType'
  #-}
