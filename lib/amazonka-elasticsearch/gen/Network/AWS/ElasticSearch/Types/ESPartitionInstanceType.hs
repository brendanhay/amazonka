{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
  ( ESPartitionInstanceType
    ( ESPartitionInstanceType'
    , ESPartitionInstanceTypeM3_Medium_Elasticsearch
    , ESPartitionInstanceTypeM3_Large_Elasticsearch
    , ESPartitionInstanceTypeM3_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeM3_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeM4_Large_Elasticsearch
    , ESPartitionInstanceTypeM4_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeM4_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeM4_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeM4_10xlarge_Elasticsearch
    , ESPartitionInstanceTypeM5_Large_Elasticsearch
    , ESPartitionInstanceTypeM5_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeM5_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeM5_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeM5_12xlarge_Elasticsearch
    , ESPartitionInstanceTypeR5_Large_Elasticsearch
    , ESPartitionInstanceTypeR5_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeR5_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeR5_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeR5_12xlarge_Elasticsearch
    , ESPartitionInstanceTypeC5_Large_Elasticsearch
    , ESPartitionInstanceTypeC5_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeC5_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeC5_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeC5_9xlarge_Elasticsearch
    , ESPartitionInstanceTypeC5_18xlarge_Elasticsearch
    , ESPartitionInstanceTypeULTRAWARM1_Medium_Elasticsearch
    , ESPartitionInstanceTypeULTRAWARM1_Large_Elasticsearch
    , ESPartitionInstanceTypeT2_Micro_Elasticsearch
    , ESPartitionInstanceTypeT2_Small_Elasticsearch
    , ESPartitionInstanceTypeT2_Medium_Elasticsearch
    , ESPartitionInstanceTypeR3_Large_Elasticsearch
    , ESPartitionInstanceTypeR3_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeR3_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeR3_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeR3_8xlarge_Elasticsearch
    , ESPartitionInstanceTypeI2_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeI2_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeD2_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeD2_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeD2_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeD2_8xlarge_Elasticsearch
    , ESPartitionInstanceTypeC4_Large_Elasticsearch
    , ESPartitionInstanceTypeC4_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeC4_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeC4_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeC4_8xlarge_Elasticsearch
    , ESPartitionInstanceTypeR4_Large_Elasticsearch
    , ESPartitionInstanceTypeR4_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeR4_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeR4_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeR4_8xlarge_Elasticsearch
    , ESPartitionInstanceTypeR4_16xlarge_Elasticsearch
    , ESPartitionInstanceTypeI3_Large_Elasticsearch
    , ESPartitionInstanceTypeI3_Xlarge_Elasticsearch
    , ESPartitionInstanceTypeI3_2xlarge_Elasticsearch
    , ESPartitionInstanceTypeI3_4xlarge_Elasticsearch
    , ESPartitionInstanceTypeI3_8xlarge_Elasticsearch
    , ESPartitionInstanceTypeI3_16xlarge_Elasticsearch
    , fromESPartitionInstanceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ESPartitionInstanceType = ESPartitionInstanceType'{fromESPartitionInstanceType
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern ESPartitionInstanceTypeM3_Medium_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM3_Medium_Elasticsearch = ESPartitionInstanceType' "m3.medium.elasticsearch"

pattern ESPartitionInstanceTypeM3_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM3_Large_Elasticsearch = ESPartitionInstanceType' "m3.large.elasticsearch"

pattern ESPartitionInstanceTypeM3_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM3_Xlarge_Elasticsearch = ESPartitionInstanceType' "m3.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM3_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM3_2xlarge_Elasticsearch = ESPartitionInstanceType' "m3.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM4_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM4_Large_Elasticsearch = ESPartitionInstanceType' "m4.large.elasticsearch"

pattern ESPartitionInstanceTypeM4_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM4_Xlarge_Elasticsearch = ESPartitionInstanceType' "m4.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM4_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM4_2xlarge_Elasticsearch = ESPartitionInstanceType' "m4.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM4_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM4_4xlarge_Elasticsearch = ESPartitionInstanceType' "m4.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM4_10xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM4_10xlarge_Elasticsearch = ESPartitionInstanceType' "m4.10xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM5_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM5_Large_Elasticsearch = ESPartitionInstanceType' "m5.large.elasticsearch"

pattern ESPartitionInstanceTypeM5_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM5_Xlarge_Elasticsearch = ESPartitionInstanceType' "m5.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM5_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM5_2xlarge_Elasticsearch = ESPartitionInstanceType' "m5.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM5_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM5_4xlarge_Elasticsearch = ESPartitionInstanceType' "m5.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeM5_12xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeM5_12xlarge_Elasticsearch = ESPartitionInstanceType' "m5.12xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR5_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR5_Large_Elasticsearch = ESPartitionInstanceType' "r5.large.elasticsearch"

pattern ESPartitionInstanceTypeR5_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR5_Xlarge_Elasticsearch = ESPartitionInstanceType' "r5.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR5_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR5_2xlarge_Elasticsearch = ESPartitionInstanceType' "r5.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR5_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR5_4xlarge_Elasticsearch = ESPartitionInstanceType' "r5.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR5_12xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR5_12xlarge_Elasticsearch = ESPartitionInstanceType' "r5.12xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC5_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC5_Large_Elasticsearch = ESPartitionInstanceType' "c5.large.elasticsearch"

pattern ESPartitionInstanceTypeC5_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC5_Xlarge_Elasticsearch = ESPartitionInstanceType' "c5.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC5_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC5_2xlarge_Elasticsearch = ESPartitionInstanceType' "c5.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC5_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC5_4xlarge_Elasticsearch = ESPartitionInstanceType' "c5.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC5_9xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC5_9xlarge_Elasticsearch = ESPartitionInstanceType' "c5.9xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC5_18xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC5_18xlarge_Elasticsearch = ESPartitionInstanceType' "c5.18xlarge.elasticsearch"

pattern ESPartitionInstanceTypeULTRAWARM1_Medium_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeULTRAWARM1_Medium_Elasticsearch = ESPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

pattern ESPartitionInstanceTypeULTRAWARM1_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeULTRAWARM1_Large_Elasticsearch = ESPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ESPartitionInstanceTypeT2_Micro_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeT2_Micro_Elasticsearch = ESPartitionInstanceType' "t2.micro.elasticsearch"

pattern ESPartitionInstanceTypeT2_Small_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeT2_Small_Elasticsearch = ESPartitionInstanceType' "t2.small.elasticsearch"

pattern ESPartitionInstanceTypeT2_Medium_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeT2_Medium_Elasticsearch = ESPartitionInstanceType' "t2.medium.elasticsearch"

pattern ESPartitionInstanceTypeR3_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR3_Large_Elasticsearch = ESPartitionInstanceType' "r3.large.elasticsearch"

pattern ESPartitionInstanceTypeR3_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR3_Xlarge_Elasticsearch = ESPartitionInstanceType' "r3.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR3_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR3_2xlarge_Elasticsearch = ESPartitionInstanceType' "r3.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR3_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR3_4xlarge_Elasticsearch = ESPartitionInstanceType' "r3.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR3_8xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR3_8xlarge_Elasticsearch = ESPartitionInstanceType' "r3.8xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI2_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI2_Xlarge_Elasticsearch = ESPartitionInstanceType' "i2.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI2_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI2_2xlarge_Elasticsearch = ESPartitionInstanceType' "i2.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeD2_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeD2_Xlarge_Elasticsearch = ESPartitionInstanceType' "d2.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeD2_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeD2_2xlarge_Elasticsearch = ESPartitionInstanceType' "d2.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeD2_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeD2_4xlarge_Elasticsearch = ESPartitionInstanceType' "d2.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeD2_8xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeD2_8xlarge_Elasticsearch = ESPartitionInstanceType' "d2.8xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC4_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC4_Large_Elasticsearch = ESPartitionInstanceType' "c4.large.elasticsearch"

pattern ESPartitionInstanceTypeC4_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC4_Xlarge_Elasticsearch = ESPartitionInstanceType' "c4.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC4_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC4_2xlarge_Elasticsearch = ESPartitionInstanceType' "c4.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC4_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC4_4xlarge_Elasticsearch = ESPartitionInstanceType' "c4.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeC4_8xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeC4_8xlarge_Elasticsearch = ESPartitionInstanceType' "c4.8xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR4_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR4_Large_Elasticsearch = ESPartitionInstanceType' "r4.large.elasticsearch"

pattern ESPartitionInstanceTypeR4_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR4_Xlarge_Elasticsearch = ESPartitionInstanceType' "r4.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR4_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR4_2xlarge_Elasticsearch = ESPartitionInstanceType' "r4.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR4_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR4_4xlarge_Elasticsearch = ESPartitionInstanceType' "r4.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR4_8xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR4_8xlarge_Elasticsearch = ESPartitionInstanceType' "r4.8xlarge.elasticsearch"

pattern ESPartitionInstanceTypeR4_16xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeR4_16xlarge_Elasticsearch = ESPartitionInstanceType' "r4.16xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI3_Large_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI3_Large_Elasticsearch = ESPartitionInstanceType' "i3.large.elasticsearch"

pattern ESPartitionInstanceTypeI3_Xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI3_Xlarge_Elasticsearch = ESPartitionInstanceType' "i3.xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI3_2xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI3_2xlarge_Elasticsearch = ESPartitionInstanceType' "i3.2xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI3_4xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI3_4xlarge_Elasticsearch = ESPartitionInstanceType' "i3.4xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI3_8xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI3_8xlarge_Elasticsearch = ESPartitionInstanceType' "i3.8xlarge.elasticsearch"

pattern ESPartitionInstanceTypeI3_16xlarge_Elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceTypeI3_16xlarge_Elasticsearch = ESPartitionInstanceType' "i3.16xlarge.elasticsearch"

{-# COMPLETE 
  ESPartitionInstanceTypeM3_Medium_Elasticsearch,

  ESPartitionInstanceTypeM3_Large_Elasticsearch,

  ESPartitionInstanceTypeM3_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeM3_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeM4_Large_Elasticsearch,

  ESPartitionInstanceTypeM4_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeM4_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeM4_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeM4_10xlarge_Elasticsearch,

  ESPartitionInstanceTypeM5_Large_Elasticsearch,

  ESPartitionInstanceTypeM5_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeM5_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeM5_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeM5_12xlarge_Elasticsearch,

  ESPartitionInstanceTypeR5_Large_Elasticsearch,

  ESPartitionInstanceTypeR5_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeR5_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeR5_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeR5_12xlarge_Elasticsearch,

  ESPartitionInstanceTypeC5_Large_Elasticsearch,

  ESPartitionInstanceTypeC5_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeC5_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeC5_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeC5_9xlarge_Elasticsearch,

  ESPartitionInstanceTypeC5_18xlarge_Elasticsearch,

  ESPartitionInstanceTypeULTRAWARM1_Medium_Elasticsearch,

  ESPartitionInstanceTypeULTRAWARM1_Large_Elasticsearch,

  ESPartitionInstanceTypeT2_Micro_Elasticsearch,

  ESPartitionInstanceTypeT2_Small_Elasticsearch,

  ESPartitionInstanceTypeT2_Medium_Elasticsearch,

  ESPartitionInstanceTypeR3_Large_Elasticsearch,

  ESPartitionInstanceTypeR3_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeR3_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeR3_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeR3_8xlarge_Elasticsearch,

  ESPartitionInstanceTypeI2_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeI2_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeD2_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeD2_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeD2_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeD2_8xlarge_Elasticsearch,

  ESPartitionInstanceTypeC4_Large_Elasticsearch,

  ESPartitionInstanceTypeC4_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeC4_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeC4_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeC4_8xlarge_Elasticsearch,

  ESPartitionInstanceTypeR4_Large_Elasticsearch,

  ESPartitionInstanceTypeR4_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeR4_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeR4_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeR4_8xlarge_Elasticsearch,

  ESPartitionInstanceTypeR4_16xlarge_Elasticsearch,

  ESPartitionInstanceTypeI3_Large_Elasticsearch,

  ESPartitionInstanceTypeI3_Xlarge_Elasticsearch,

  ESPartitionInstanceTypeI3_2xlarge_Elasticsearch,

  ESPartitionInstanceTypeI3_4xlarge_Elasticsearch,

  ESPartitionInstanceTypeI3_8xlarge_Elasticsearch,

  ESPartitionInstanceTypeI3_16xlarge_Elasticsearch,
  ESPartitionInstanceType'
  #-}
