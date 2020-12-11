-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
  ( ESPartitionInstanceType
      ( ESPartitionInstanceType',
        C4_2XLarge_Elasticsearch,
        C4_4XLarge_Elasticsearch,
        C4_8XLarge_Elasticsearch,
        C4_Large_Elasticsearch,
        C4_XLarge_Elasticsearch,
        C5_18XLarge_Elasticsearch,
        C5_2XLarge_Elasticsearch,
        C5_4XLarge_Elasticsearch,
        C5_9XLarge_Elasticsearch,
        C5_Large_Elasticsearch,
        C5_XLarge_Elasticsearch,
        D2_2XLarge_Elasticsearch,
        D2_4XLarge_Elasticsearch,
        D2_8XLarge_Elasticsearch,
        D2_XLarge_Elasticsearch,
        I2_2XLarge_Elasticsearch,
        I2_XLarge_Elasticsearch,
        I3_16XLarge_Elasticsearch,
        I3_2XLarge_Elasticsearch,
        I3_4XLarge_Elasticsearch,
        I3_8XLarge_Elasticsearch,
        I3_Large_Elasticsearch,
        I3_XLarge_Elasticsearch,
        M3_2XLarge_Elasticsearch,
        M3_Large_Elasticsearch,
        M3_Medium_Elasticsearch,
        M3_XLarge_Elasticsearch,
        M4_10XLarge_Elasticsearch,
        M4_2XLarge_Elasticsearch,
        M4_4XLarge_Elasticsearch,
        M4_Large_Elasticsearch,
        M4_XLarge_Elasticsearch,
        M5_12XLarge_Elasticsearch,
        M5_2XLarge_Elasticsearch,
        M5_4XLarge_Elasticsearch,
        M5_Large_Elasticsearch,
        M5_XLarge_Elasticsearch,
        R3_2XLarge_Elasticsearch,
        R3_4XLarge_Elasticsearch,
        R3_8XLarge_Elasticsearch,
        R3_Large_Elasticsearch,
        R3_XLarge_Elasticsearch,
        R4_16XLarge_Elasticsearch,
        R4_2XLarge_Elasticsearch,
        R4_4XLarge_Elasticsearch,
        R4_8XLarge_Elasticsearch,
        R4_Large_Elasticsearch,
        R4_XLarge_Elasticsearch,
        R5_12XLarge_Elasticsearch,
        R5_2XLarge_Elasticsearch,
        R5_4XLarge_Elasticsearch,
        R5_Large_Elasticsearch,
        R5_XLarge_Elasticsearch,
        T2_Medium_Elasticsearch,
        T2_Micro_Elasticsearch,
        T2_Small_Elasticsearch,
        ULTRAWARM1_Large_Elasticsearch,
        ULTRAWARM1_Medium_Elasticsearch
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ESPartitionInstanceType = ESPartitionInstanceType' Lude.Text
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

pattern C4_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C4_2XLarge_Elasticsearch = ESPartitionInstanceType' "c4.2xlarge.elasticsearch"

pattern C4_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C4_4XLarge_Elasticsearch = ESPartitionInstanceType' "c4.4xlarge.elasticsearch"

pattern C4_8XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C4_8XLarge_Elasticsearch = ESPartitionInstanceType' "c4.8xlarge.elasticsearch"

pattern C4_Large_Elasticsearch :: ESPartitionInstanceType
pattern C4_Large_Elasticsearch = ESPartitionInstanceType' "c4.large.elasticsearch"

pattern C4_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C4_XLarge_Elasticsearch = ESPartitionInstanceType' "c4.xlarge.elasticsearch"

pattern C5_18XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C5_18XLarge_Elasticsearch = ESPartitionInstanceType' "c5.18xlarge.elasticsearch"

pattern C5_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C5_2XLarge_Elasticsearch = ESPartitionInstanceType' "c5.2xlarge.elasticsearch"

pattern C5_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C5_4XLarge_Elasticsearch = ESPartitionInstanceType' "c5.4xlarge.elasticsearch"

pattern C5_9XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C5_9XLarge_Elasticsearch = ESPartitionInstanceType' "c5.9xlarge.elasticsearch"

pattern C5_Large_Elasticsearch :: ESPartitionInstanceType
pattern C5_Large_Elasticsearch = ESPartitionInstanceType' "c5.large.elasticsearch"

pattern C5_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern C5_XLarge_Elasticsearch = ESPartitionInstanceType' "c5.xlarge.elasticsearch"

pattern D2_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern D2_2XLarge_Elasticsearch = ESPartitionInstanceType' "d2.2xlarge.elasticsearch"

pattern D2_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern D2_4XLarge_Elasticsearch = ESPartitionInstanceType' "d2.4xlarge.elasticsearch"

pattern D2_8XLarge_Elasticsearch :: ESPartitionInstanceType
pattern D2_8XLarge_Elasticsearch = ESPartitionInstanceType' "d2.8xlarge.elasticsearch"

pattern D2_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern D2_XLarge_Elasticsearch = ESPartitionInstanceType' "d2.xlarge.elasticsearch"

pattern I2_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I2_2XLarge_Elasticsearch = ESPartitionInstanceType' "i2.2xlarge.elasticsearch"

pattern I2_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I2_XLarge_Elasticsearch = ESPartitionInstanceType' "i2.xlarge.elasticsearch"

pattern I3_16XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I3_16XLarge_Elasticsearch = ESPartitionInstanceType' "i3.16xlarge.elasticsearch"

pattern I3_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I3_2XLarge_Elasticsearch = ESPartitionInstanceType' "i3.2xlarge.elasticsearch"

pattern I3_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I3_4XLarge_Elasticsearch = ESPartitionInstanceType' "i3.4xlarge.elasticsearch"

pattern I3_8XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I3_8XLarge_Elasticsearch = ESPartitionInstanceType' "i3.8xlarge.elasticsearch"

pattern I3_Large_Elasticsearch :: ESPartitionInstanceType
pattern I3_Large_Elasticsearch = ESPartitionInstanceType' "i3.large.elasticsearch"

pattern I3_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern I3_XLarge_Elasticsearch = ESPartitionInstanceType' "i3.xlarge.elasticsearch"

pattern M3_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M3_2XLarge_Elasticsearch = ESPartitionInstanceType' "m3.2xlarge.elasticsearch"

pattern M3_Large_Elasticsearch :: ESPartitionInstanceType
pattern M3_Large_Elasticsearch = ESPartitionInstanceType' "m3.large.elasticsearch"

pattern M3_Medium_Elasticsearch :: ESPartitionInstanceType
pattern M3_Medium_Elasticsearch = ESPartitionInstanceType' "m3.medium.elasticsearch"

pattern M3_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M3_XLarge_Elasticsearch = ESPartitionInstanceType' "m3.xlarge.elasticsearch"

pattern M4_10XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M4_10XLarge_Elasticsearch = ESPartitionInstanceType' "m4.10xlarge.elasticsearch"

pattern M4_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M4_2XLarge_Elasticsearch = ESPartitionInstanceType' "m4.2xlarge.elasticsearch"

pattern M4_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M4_4XLarge_Elasticsearch = ESPartitionInstanceType' "m4.4xlarge.elasticsearch"

pattern M4_Large_Elasticsearch :: ESPartitionInstanceType
pattern M4_Large_Elasticsearch = ESPartitionInstanceType' "m4.large.elasticsearch"

pattern M4_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M4_XLarge_Elasticsearch = ESPartitionInstanceType' "m4.xlarge.elasticsearch"

pattern M5_12XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M5_12XLarge_Elasticsearch = ESPartitionInstanceType' "m5.12xlarge.elasticsearch"

pattern M5_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M5_2XLarge_Elasticsearch = ESPartitionInstanceType' "m5.2xlarge.elasticsearch"

pattern M5_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M5_4XLarge_Elasticsearch = ESPartitionInstanceType' "m5.4xlarge.elasticsearch"

pattern M5_Large_Elasticsearch :: ESPartitionInstanceType
pattern M5_Large_Elasticsearch = ESPartitionInstanceType' "m5.large.elasticsearch"

pattern M5_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern M5_XLarge_Elasticsearch = ESPartitionInstanceType' "m5.xlarge.elasticsearch"

pattern R3_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R3_2XLarge_Elasticsearch = ESPartitionInstanceType' "r3.2xlarge.elasticsearch"

pattern R3_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R3_4XLarge_Elasticsearch = ESPartitionInstanceType' "r3.4xlarge.elasticsearch"

pattern R3_8XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R3_8XLarge_Elasticsearch = ESPartitionInstanceType' "r3.8xlarge.elasticsearch"

pattern R3_Large_Elasticsearch :: ESPartitionInstanceType
pattern R3_Large_Elasticsearch = ESPartitionInstanceType' "r3.large.elasticsearch"

pattern R3_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R3_XLarge_Elasticsearch = ESPartitionInstanceType' "r3.xlarge.elasticsearch"

pattern R4_16XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R4_16XLarge_Elasticsearch = ESPartitionInstanceType' "r4.16xlarge.elasticsearch"

pattern R4_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R4_2XLarge_Elasticsearch = ESPartitionInstanceType' "r4.2xlarge.elasticsearch"

pattern R4_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R4_4XLarge_Elasticsearch = ESPartitionInstanceType' "r4.4xlarge.elasticsearch"

pattern R4_8XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R4_8XLarge_Elasticsearch = ESPartitionInstanceType' "r4.8xlarge.elasticsearch"

pattern R4_Large_Elasticsearch :: ESPartitionInstanceType
pattern R4_Large_Elasticsearch = ESPartitionInstanceType' "r4.large.elasticsearch"

pattern R4_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R4_XLarge_Elasticsearch = ESPartitionInstanceType' "r4.xlarge.elasticsearch"

pattern R5_12XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R5_12XLarge_Elasticsearch = ESPartitionInstanceType' "r5.12xlarge.elasticsearch"

pattern R5_2XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R5_2XLarge_Elasticsearch = ESPartitionInstanceType' "r5.2xlarge.elasticsearch"

pattern R5_4XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R5_4XLarge_Elasticsearch = ESPartitionInstanceType' "r5.4xlarge.elasticsearch"

pattern R5_Large_Elasticsearch :: ESPartitionInstanceType
pattern R5_Large_Elasticsearch = ESPartitionInstanceType' "r5.large.elasticsearch"

pattern R5_XLarge_Elasticsearch :: ESPartitionInstanceType
pattern R5_XLarge_Elasticsearch = ESPartitionInstanceType' "r5.xlarge.elasticsearch"

pattern T2_Medium_Elasticsearch :: ESPartitionInstanceType
pattern T2_Medium_Elasticsearch = ESPartitionInstanceType' "t2.medium.elasticsearch"

pattern T2_Micro_Elasticsearch :: ESPartitionInstanceType
pattern T2_Micro_Elasticsearch = ESPartitionInstanceType' "t2.micro.elasticsearch"

pattern T2_Small_Elasticsearch :: ESPartitionInstanceType
pattern T2_Small_Elasticsearch = ESPartitionInstanceType' "t2.small.elasticsearch"

pattern ULTRAWARM1_Large_Elasticsearch :: ESPartitionInstanceType
pattern ULTRAWARM1_Large_Elasticsearch = ESPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ULTRAWARM1_Medium_Elasticsearch :: ESPartitionInstanceType
pattern ULTRAWARM1_Medium_Elasticsearch = ESPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

{-# COMPLETE
  C4_2XLarge_Elasticsearch,
  C4_4XLarge_Elasticsearch,
  C4_8XLarge_Elasticsearch,
  C4_Large_Elasticsearch,
  C4_XLarge_Elasticsearch,
  C5_18XLarge_Elasticsearch,
  C5_2XLarge_Elasticsearch,
  C5_4XLarge_Elasticsearch,
  C5_9XLarge_Elasticsearch,
  C5_Large_Elasticsearch,
  C5_XLarge_Elasticsearch,
  D2_2XLarge_Elasticsearch,
  D2_4XLarge_Elasticsearch,
  D2_8XLarge_Elasticsearch,
  D2_XLarge_Elasticsearch,
  I2_2XLarge_Elasticsearch,
  I2_XLarge_Elasticsearch,
  I3_16XLarge_Elasticsearch,
  I3_2XLarge_Elasticsearch,
  I3_4XLarge_Elasticsearch,
  I3_8XLarge_Elasticsearch,
  I3_Large_Elasticsearch,
  I3_XLarge_Elasticsearch,
  M3_2XLarge_Elasticsearch,
  M3_Large_Elasticsearch,
  M3_Medium_Elasticsearch,
  M3_XLarge_Elasticsearch,
  M4_10XLarge_Elasticsearch,
  M4_2XLarge_Elasticsearch,
  M4_4XLarge_Elasticsearch,
  M4_Large_Elasticsearch,
  M4_XLarge_Elasticsearch,
  M5_12XLarge_Elasticsearch,
  M5_2XLarge_Elasticsearch,
  M5_4XLarge_Elasticsearch,
  M5_Large_Elasticsearch,
  M5_XLarge_Elasticsearch,
  R3_2XLarge_Elasticsearch,
  R3_4XLarge_Elasticsearch,
  R3_8XLarge_Elasticsearch,
  R3_Large_Elasticsearch,
  R3_XLarge_Elasticsearch,
  R4_16XLarge_Elasticsearch,
  R4_2XLarge_Elasticsearch,
  R4_4XLarge_Elasticsearch,
  R4_8XLarge_Elasticsearch,
  R4_Large_Elasticsearch,
  R4_XLarge_Elasticsearch,
  R5_12XLarge_Elasticsearch,
  R5_2XLarge_Elasticsearch,
  R5_4XLarge_Elasticsearch,
  R5_Large_Elasticsearch,
  R5_XLarge_Elasticsearch,
  T2_Medium_Elasticsearch,
  T2_Micro_Elasticsearch,
  T2_Small_Elasticsearch,
  ULTRAWARM1_Large_Elasticsearch,
  ULTRAWARM1_Medium_Elasticsearch,
  ESPartitionInstanceType'
  #-}
