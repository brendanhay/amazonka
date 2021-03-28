{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.PartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudSearch.Types.PartitionInstanceType
  ( PartitionInstanceType
    ( PartitionInstanceType'
    , PartitionInstanceTypeSearch_M1_Small
    , PartitionInstanceTypeSearch_M1_Large
    , PartitionInstanceTypeSearch_M2_Xlarge
    , PartitionInstanceTypeSearch_M2_2xlarge
    , PartitionInstanceTypeSearch_M3_Medium
    , PartitionInstanceTypeSearch_M3_Large
    , PartitionInstanceTypeSearch_M3_Xlarge
    , PartitionInstanceTypeSearch_M3_2xlarge
    , fromPartitionInstanceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The instance type (such as @search.m1.small@ ) on which an index partition is hosted.
newtype PartitionInstanceType = PartitionInstanceType'{fromPartitionInstanceType
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern PartitionInstanceTypeSearch_M1_Small :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M1_Small = PartitionInstanceType' "search.m1.small"

pattern PartitionInstanceTypeSearch_M1_Large :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M1_Large = PartitionInstanceType' "search.m1.large"

pattern PartitionInstanceTypeSearch_M2_Xlarge :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M2_Xlarge = PartitionInstanceType' "search.m2.xlarge"

pattern PartitionInstanceTypeSearch_M2_2xlarge :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M2_2xlarge = PartitionInstanceType' "search.m2.2xlarge"

pattern PartitionInstanceTypeSearch_M3_Medium :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M3_Medium = PartitionInstanceType' "search.m3.medium"

pattern PartitionInstanceTypeSearch_M3_Large :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M3_Large = PartitionInstanceType' "search.m3.large"

pattern PartitionInstanceTypeSearch_M3_Xlarge :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M3_Xlarge = PartitionInstanceType' "search.m3.xlarge"

pattern PartitionInstanceTypeSearch_M3_2xlarge :: PartitionInstanceType
pattern PartitionInstanceTypeSearch_M3_2xlarge = PartitionInstanceType' "search.m3.2xlarge"

{-# COMPLETE 
  PartitionInstanceTypeSearch_M1_Small,

  PartitionInstanceTypeSearch_M1_Large,

  PartitionInstanceTypeSearch_M2_Xlarge,

  PartitionInstanceTypeSearch_M2_2xlarge,

  PartitionInstanceTypeSearch_M3_Medium,

  PartitionInstanceTypeSearch_M3_Large,

  PartitionInstanceTypeSearch_M3_Xlarge,

  PartitionInstanceTypeSearch_M3_2xlarge,
  PartitionInstanceType'
  #-}
