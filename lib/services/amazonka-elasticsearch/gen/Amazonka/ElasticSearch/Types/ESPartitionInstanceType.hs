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
-- Module      : Amazonka.ElasticSearch.Types.ESPartitionInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ESPartitionInstanceType
  ( ESPartitionInstanceType
      ( ..,
        ESPartitionInstanceType_C4_2xlarge_elasticsearch,
        ESPartitionInstanceType_C4_4xlarge_elasticsearch,
        ESPartitionInstanceType_C4_8xlarge_elasticsearch,
        ESPartitionInstanceType_C4_large_elasticsearch,
        ESPartitionInstanceType_C4_xlarge_elasticsearch,
        ESPartitionInstanceType_C5_18xlarge_elasticsearch,
        ESPartitionInstanceType_C5_2xlarge_elasticsearch,
        ESPartitionInstanceType_C5_4xlarge_elasticsearch,
        ESPartitionInstanceType_C5_9xlarge_elasticsearch,
        ESPartitionInstanceType_C5_large_elasticsearch,
        ESPartitionInstanceType_C5_xlarge_elasticsearch,
        ESPartitionInstanceType_D2_2xlarge_elasticsearch,
        ESPartitionInstanceType_D2_4xlarge_elasticsearch,
        ESPartitionInstanceType_D2_8xlarge_elasticsearch,
        ESPartitionInstanceType_D2_xlarge_elasticsearch,
        ESPartitionInstanceType_I2_2xlarge_elasticsearch,
        ESPartitionInstanceType_I2_xlarge_elasticsearch,
        ESPartitionInstanceType_I3_16xlarge_elasticsearch,
        ESPartitionInstanceType_I3_2xlarge_elasticsearch,
        ESPartitionInstanceType_I3_4xlarge_elasticsearch,
        ESPartitionInstanceType_I3_8xlarge_elasticsearch,
        ESPartitionInstanceType_I3_large_elasticsearch,
        ESPartitionInstanceType_I3_xlarge_elasticsearch,
        ESPartitionInstanceType_M3_2xlarge_elasticsearch,
        ESPartitionInstanceType_M3_large_elasticsearch,
        ESPartitionInstanceType_M3_medium_elasticsearch,
        ESPartitionInstanceType_M3_xlarge_elasticsearch,
        ESPartitionInstanceType_M4_10xlarge_elasticsearch,
        ESPartitionInstanceType_M4_2xlarge_elasticsearch,
        ESPartitionInstanceType_M4_4xlarge_elasticsearch,
        ESPartitionInstanceType_M4_large_elasticsearch,
        ESPartitionInstanceType_M4_xlarge_elasticsearch,
        ESPartitionInstanceType_M5_12xlarge_elasticsearch,
        ESPartitionInstanceType_M5_2xlarge_elasticsearch,
        ESPartitionInstanceType_M5_4xlarge_elasticsearch,
        ESPartitionInstanceType_M5_large_elasticsearch,
        ESPartitionInstanceType_M5_xlarge_elasticsearch,
        ESPartitionInstanceType_R3_2xlarge_elasticsearch,
        ESPartitionInstanceType_R3_4xlarge_elasticsearch,
        ESPartitionInstanceType_R3_8xlarge_elasticsearch,
        ESPartitionInstanceType_R3_large_elasticsearch,
        ESPartitionInstanceType_R3_xlarge_elasticsearch,
        ESPartitionInstanceType_R4_16xlarge_elasticsearch,
        ESPartitionInstanceType_R4_2xlarge_elasticsearch,
        ESPartitionInstanceType_R4_4xlarge_elasticsearch,
        ESPartitionInstanceType_R4_8xlarge_elasticsearch,
        ESPartitionInstanceType_R4_large_elasticsearch,
        ESPartitionInstanceType_R4_xlarge_elasticsearch,
        ESPartitionInstanceType_R5_12xlarge_elasticsearch,
        ESPartitionInstanceType_R5_2xlarge_elasticsearch,
        ESPartitionInstanceType_R5_4xlarge_elasticsearch,
        ESPartitionInstanceType_R5_large_elasticsearch,
        ESPartitionInstanceType_R5_xlarge_elasticsearch,
        ESPartitionInstanceType_T2_medium_elasticsearch,
        ESPartitionInstanceType_T2_micro_elasticsearch,
        ESPartitionInstanceType_T2_small_elasticsearch,
        ESPartitionInstanceType_Ultrawarm1_large_elasticsearch,
        ESPartitionInstanceType_Ultrawarm1_medium_elasticsearch
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ESPartitionInstanceType = ESPartitionInstanceType'
  { fromESPartitionInstanceType ::
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

pattern ESPartitionInstanceType_C4_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C4_2xlarge_elasticsearch = ESPartitionInstanceType' "c4.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_C4_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C4_4xlarge_elasticsearch = ESPartitionInstanceType' "c4.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_C4_8xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C4_8xlarge_elasticsearch = ESPartitionInstanceType' "c4.8xlarge.elasticsearch"

pattern ESPartitionInstanceType_C4_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C4_large_elasticsearch = ESPartitionInstanceType' "c4.large.elasticsearch"

pattern ESPartitionInstanceType_C4_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C4_xlarge_elasticsearch = ESPartitionInstanceType' "c4.xlarge.elasticsearch"

pattern ESPartitionInstanceType_C5_18xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C5_18xlarge_elasticsearch = ESPartitionInstanceType' "c5.18xlarge.elasticsearch"

pattern ESPartitionInstanceType_C5_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C5_2xlarge_elasticsearch = ESPartitionInstanceType' "c5.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_C5_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C5_4xlarge_elasticsearch = ESPartitionInstanceType' "c5.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_C5_9xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C5_9xlarge_elasticsearch = ESPartitionInstanceType' "c5.9xlarge.elasticsearch"

pattern ESPartitionInstanceType_C5_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C5_large_elasticsearch = ESPartitionInstanceType' "c5.large.elasticsearch"

pattern ESPartitionInstanceType_C5_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_C5_xlarge_elasticsearch = ESPartitionInstanceType' "c5.xlarge.elasticsearch"

pattern ESPartitionInstanceType_D2_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_D2_2xlarge_elasticsearch = ESPartitionInstanceType' "d2.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_D2_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_D2_4xlarge_elasticsearch = ESPartitionInstanceType' "d2.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_D2_8xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_D2_8xlarge_elasticsearch = ESPartitionInstanceType' "d2.8xlarge.elasticsearch"

pattern ESPartitionInstanceType_D2_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_D2_xlarge_elasticsearch = ESPartitionInstanceType' "d2.xlarge.elasticsearch"

pattern ESPartitionInstanceType_I2_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I2_2xlarge_elasticsearch = ESPartitionInstanceType' "i2.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_I2_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I2_xlarge_elasticsearch = ESPartitionInstanceType' "i2.xlarge.elasticsearch"

pattern ESPartitionInstanceType_I3_16xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I3_16xlarge_elasticsearch = ESPartitionInstanceType' "i3.16xlarge.elasticsearch"

pattern ESPartitionInstanceType_I3_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I3_2xlarge_elasticsearch = ESPartitionInstanceType' "i3.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_I3_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I3_4xlarge_elasticsearch = ESPartitionInstanceType' "i3.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_I3_8xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I3_8xlarge_elasticsearch = ESPartitionInstanceType' "i3.8xlarge.elasticsearch"

pattern ESPartitionInstanceType_I3_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I3_large_elasticsearch = ESPartitionInstanceType' "i3.large.elasticsearch"

pattern ESPartitionInstanceType_I3_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_I3_xlarge_elasticsearch = ESPartitionInstanceType' "i3.xlarge.elasticsearch"

pattern ESPartitionInstanceType_M3_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M3_2xlarge_elasticsearch = ESPartitionInstanceType' "m3.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_M3_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M3_large_elasticsearch = ESPartitionInstanceType' "m3.large.elasticsearch"

pattern ESPartitionInstanceType_M3_medium_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M3_medium_elasticsearch = ESPartitionInstanceType' "m3.medium.elasticsearch"

pattern ESPartitionInstanceType_M3_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M3_xlarge_elasticsearch = ESPartitionInstanceType' "m3.xlarge.elasticsearch"

pattern ESPartitionInstanceType_M4_10xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M4_10xlarge_elasticsearch = ESPartitionInstanceType' "m4.10xlarge.elasticsearch"

pattern ESPartitionInstanceType_M4_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M4_2xlarge_elasticsearch = ESPartitionInstanceType' "m4.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_M4_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M4_4xlarge_elasticsearch = ESPartitionInstanceType' "m4.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_M4_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M4_large_elasticsearch = ESPartitionInstanceType' "m4.large.elasticsearch"

pattern ESPartitionInstanceType_M4_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M4_xlarge_elasticsearch = ESPartitionInstanceType' "m4.xlarge.elasticsearch"

pattern ESPartitionInstanceType_M5_12xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M5_12xlarge_elasticsearch = ESPartitionInstanceType' "m5.12xlarge.elasticsearch"

pattern ESPartitionInstanceType_M5_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M5_2xlarge_elasticsearch = ESPartitionInstanceType' "m5.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_M5_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M5_4xlarge_elasticsearch = ESPartitionInstanceType' "m5.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_M5_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M5_large_elasticsearch = ESPartitionInstanceType' "m5.large.elasticsearch"

pattern ESPartitionInstanceType_M5_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_M5_xlarge_elasticsearch = ESPartitionInstanceType' "m5.xlarge.elasticsearch"

pattern ESPartitionInstanceType_R3_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R3_2xlarge_elasticsearch = ESPartitionInstanceType' "r3.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_R3_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R3_4xlarge_elasticsearch = ESPartitionInstanceType' "r3.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_R3_8xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R3_8xlarge_elasticsearch = ESPartitionInstanceType' "r3.8xlarge.elasticsearch"

pattern ESPartitionInstanceType_R3_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R3_large_elasticsearch = ESPartitionInstanceType' "r3.large.elasticsearch"

pattern ESPartitionInstanceType_R3_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R3_xlarge_elasticsearch = ESPartitionInstanceType' "r3.xlarge.elasticsearch"

pattern ESPartitionInstanceType_R4_16xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R4_16xlarge_elasticsearch = ESPartitionInstanceType' "r4.16xlarge.elasticsearch"

pattern ESPartitionInstanceType_R4_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R4_2xlarge_elasticsearch = ESPartitionInstanceType' "r4.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_R4_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R4_4xlarge_elasticsearch = ESPartitionInstanceType' "r4.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_R4_8xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R4_8xlarge_elasticsearch = ESPartitionInstanceType' "r4.8xlarge.elasticsearch"

pattern ESPartitionInstanceType_R4_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R4_large_elasticsearch = ESPartitionInstanceType' "r4.large.elasticsearch"

pattern ESPartitionInstanceType_R4_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R4_xlarge_elasticsearch = ESPartitionInstanceType' "r4.xlarge.elasticsearch"

pattern ESPartitionInstanceType_R5_12xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R5_12xlarge_elasticsearch = ESPartitionInstanceType' "r5.12xlarge.elasticsearch"

pattern ESPartitionInstanceType_R5_2xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R5_2xlarge_elasticsearch = ESPartitionInstanceType' "r5.2xlarge.elasticsearch"

pattern ESPartitionInstanceType_R5_4xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R5_4xlarge_elasticsearch = ESPartitionInstanceType' "r5.4xlarge.elasticsearch"

pattern ESPartitionInstanceType_R5_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R5_large_elasticsearch = ESPartitionInstanceType' "r5.large.elasticsearch"

pattern ESPartitionInstanceType_R5_xlarge_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_R5_xlarge_elasticsearch = ESPartitionInstanceType' "r5.xlarge.elasticsearch"

pattern ESPartitionInstanceType_T2_medium_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_T2_medium_elasticsearch = ESPartitionInstanceType' "t2.medium.elasticsearch"

pattern ESPartitionInstanceType_T2_micro_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_T2_micro_elasticsearch = ESPartitionInstanceType' "t2.micro.elasticsearch"

pattern ESPartitionInstanceType_T2_small_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_T2_small_elasticsearch = ESPartitionInstanceType' "t2.small.elasticsearch"

pattern ESPartitionInstanceType_Ultrawarm1_large_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_Ultrawarm1_large_elasticsearch = ESPartitionInstanceType' "ultrawarm1.large.elasticsearch"

pattern ESPartitionInstanceType_Ultrawarm1_medium_elasticsearch :: ESPartitionInstanceType
pattern ESPartitionInstanceType_Ultrawarm1_medium_elasticsearch = ESPartitionInstanceType' "ultrawarm1.medium.elasticsearch"

{-# COMPLETE
  ESPartitionInstanceType_C4_2xlarge_elasticsearch,
  ESPartitionInstanceType_C4_4xlarge_elasticsearch,
  ESPartitionInstanceType_C4_8xlarge_elasticsearch,
  ESPartitionInstanceType_C4_large_elasticsearch,
  ESPartitionInstanceType_C4_xlarge_elasticsearch,
  ESPartitionInstanceType_C5_18xlarge_elasticsearch,
  ESPartitionInstanceType_C5_2xlarge_elasticsearch,
  ESPartitionInstanceType_C5_4xlarge_elasticsearch,
  ESPartitionInstanceType_C5_9xlarge_elasticsearch,
  ESPartitionInstanceType_C5_large_elasticsearch,
  ESPartitionInstanceType_C5_xlarge_elasticsearch,
  ESPartitionInstanceType_D2_2xlarge_elasticsearch,
  ESPartitionInstanceType_D2_4xlarge_elasticsearch,
  ESPartitionInstanceType_D2_8xlarge_elasticsearch,
  ESPartitionInstanceType_D2_xlarge_elasticsearch,
  ESPartitionInstanceType_I2_2xlarge_elasticsearch,
  ESPartitionInstanceType_I2_xlarge_elasticsearch,
  ESPartitionInstanceType_I3_16xlarge_elasticsearch,
  ESPartitionInstanceType_I3_2xlarge_elasticsearch,
  ESPartitionInstanceType_I3_4xlarge_elasticsearch,
  ESPartitionInstanceType_I3_8xlarge_elasticsearch,
  ESPartitionInstanceType_I3_large_elasticsearch,
  ESPartitionInstanceType_I3_xlarge_elasticsearch,
  ESPartitionInstanceType_M3_2xlarge_elasticsearch,
  ESPartitionInstanceType_M3_large_elasticsearch,
  ESPartitionInstanceType_M3_medium_elasticsearch,
  ESPartitionInstanceType_M3_xlarge_elasticsearch,
  ESPartitionInstanceType_M4_10xlarge_elasticsearch,
  ESPartitionInstanceType_M4_2xlarge_elasticsearch,
  ESPartitionInstanceType_M4_4xlarge_elasticsearch,
  ESPartitionInstanceType_M4_large_elasticsearch,
  ESPartitionInstanceType_M4_xlarge_elasticsearch,
  ESPartitionInstanceType_M5_12xlarge_elasticsearch,
  ESPartitionInstanceType_M5_2xlarge_elasticsearch,
  ESPartitionInstanceType_M5_4xlarge_elasticsearch,
  ESPartitionInstanceType_M5_large_elasticsearch,
  ESPartitionInstanceType_M5_xlarge_elasticsearch,
  ESPartitionInstanceType_R3_2xlarge_elasticsearch,
  ESPartitionInstanceType_R3_4xlarge_elasticsearch,
  ESPartitionInstanceType_R3_8xlarge_elasticsearch,
  ESPartitionInstanceType_R3_large_elasticsearch,
  ESPartitionInstanceType_R3_xlarge_elasticsearch,
  ESPartitionInstanceType_R4_16xlarge_elasticsearch,
  ESPartitionInstanceType_R4_2xlarge_elasticsearch,
  ESPartitionInstanceType_R4_4xlarge_elasticsearch,
  ESPartitionInstanceType_R4_8xlarge_elasticsearch,
  ESPartitionInstanceType_R4_large_elasticsearch,
  ESPartitionInstanceType_R4_xlarge_elasticsearch,
  ESPartitionInstanceType_R5_12xlarge_elasticsearch,
  ESPartitionInstanceType_R5_2xlarge_elasticsearch,
  ESPartitionInstanceType_R5_4xlarge_elasticsearch,
  ESPartitionInstanceType_R5_large_elasticsearch,
  ESPartitionInstanceType_R5_xlarge_elasticsearch,
  ESPartitionInstanceType_T2_medium_elasticsearch,
  ESPartitionInstanceType_T2_micro_elasticsearch,
  ESPartitionInstanceType_T2_small_elasticsearch,
  ESPartitionInstanceType_Ultrawarm1_large_elasticsearch,
  ESPartitionInstanceType_Ultrawarm1_medium_elasticsearch,
  ESPartitionInstanceType'
  #-}
