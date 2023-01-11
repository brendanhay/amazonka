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
-- Module      : Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
  ( OpenSearchPartitionInstanceType
      ( ..,
        OpenSearchPartitionInstanceType_C4_2xlarge_search,
        OpenSearchPartitionInstanceType_C4_4xlarge_search,
        OpenSearchPartitionInstanceType_C4_8xlarge_search,
        OpenSearchPartitionInstanceType_C4_large_search,
        OpenSearchPartitionInstanceType_C4_xlarge_search,
        OpenSearchPartitionInstanceType_C5_18xlarge_search,
        OpenSearchPartitionInstanceType_C5_2xlarge_search,
        OpenSearchPartitionInstanceType_C5_4xlarge_search,
        OpenSearchPartitionInstanceType_C5_9xlarge_search,
        OpenSearchPartitionInstanceType_C5_large_search,
        OpenSearchPartitionInstanceType_C5_xlarge_search,
        OpenSearchPartitionInstanceType_C6g_12xlarge_search,
        OpenSearchPartitionInstanceType_C6g_2xlarge_search,
        OpenSearchPartitionInstanceType_C6g_4xlarge_search,
        OpenSearchPartitionInstanceType_C6g_8xlarge_search,
        OpenSearchPartitionInstanceType_C6g_large_search,
        OpenSearchPartitionInstanceType_C6g_xlarge_search,
        OpenSearchPartitionInstanceType_D2_2xlarge_search,
        OpenSearchPartitionInstanceType_D2_4xlarge_search,
        OpenSearchPartitionInstanceType_D2_8xlarge_search,
        OpenSearchPartitionInstanceType_D2_xlarge_search,
        OpenSearchPartitionInstanceType_I2_2xlarge_search,
        OpenSearchPartitionInstanceType_I2_xlarge_search,
        OpenSearchPartitionInstanceType_I3_16xlarge_search,
        OpenSearchPartitionInstanceType_I3_2xlarge_search,
        OpenSearchPartitionInstanceType_I3_4xlarge_search,
        OpenSearchPartitionInstanceType_I3_8xlarge_search,
        OpenSearchPartitionInstanceType_I3_large_search,
        OpenSearchPartitionInstanceType_I3_xlarge_search,
        OpenSearchPartitionInstanceType_M3_2xlarge_search,
        OpenSearchPartitionInstanceType_M3_large_search,
        OpenSearchPartitionInstanceType_M3_medium_search,
        OpenSearchPartitionInstanceType_M3_xlarge_search,
        OpenSearchPartitionInstanceType_M4_10xlarge_search,
        OpenSearchPartitionInstanceType_M4_2xlarge_search,
        OpenSearchPartitionInstanceType_M4_4xlarge_search,
        OpenSearchPartitionInstanceType_M4_large_search,
        OpenSearchPartitionInstanceType_M4_xlarge_search,
        OpenSearchPartitionInstanceType_M5_12xlarge_search,
        OpenSearchPartitionInstanceType_M5_24xlarge_search,
        OpenSearchPartitionInstanceType_M5_2xlarge_search,
        OpenSearchPartitionInstanceType_M5_4xlarge_search,
        OpenSearchPartitionInstanceType_M5_large_search,
        OpenSearchPartitionInstanceType_M5_xlarge_search,
        OpenSearchPartitionInstanceType_M6g_12xlarge_search,
        OpenSearchPartitionInstanceType_M6g_2xlarge_search,
        OpenSearchPartitionInstanceType_M6g_4xlarge_search,
        OpenSearchPartitionInstanceType_M6g_8xlarge_search,
        OpenSearchPartitionInstanceType_M6g_large_search,
        OpenSearchPartitionInstanceType_M6g_xlarge_search,
        OpenSearchPartitionInstanceType_R3_2xlarge_search,
        OpenSearchPartitionInstanceType_R3_4xlarge_search,
        OpenSearchPartitionInstanceType_R3_8xlarge_search,
        OpenSearchPartitionInstanceType_R3_large_search,
        OpenSearchPartitionInstanceType_R3_xlarge_search,
        OpenSearchPartitionInstanceType_R4_16xlarge_search,
        OpenSearchPartitionInstanceType_R4_2xlarge_search,
        OpenSearchPartitionInstanceType_R4_4xlarge_search,
        OpenSearchPartitionInstanceType_R4_8xlarge_search,
        OpenSearchPartitionInstanceType_R4_large_search,
        OpenSearchPartitionInstanceType_R4_xlarge_search,
        OpenSearchPartitionInstanceType_R5_12xlarge_search,
        OpenSearchPartitionInstanceType_R5_24xlarge_search,
        OpenSearchPartitionInstanceType_R5_2xlarge_search,
        OpenSearchPartitionInstanceType_R5_4xlarge_search,
        OpenSearchPartitionInstanceType_R5_large_search,
        OpenSearchPartitionInstanceType_R5_xlarge_search,
        OpenSearchPartitionInstanceType_R6g_12xlarge_search,
        OpenSearchPartitionInstanceType_R6g_2xlarge_search,
        OpenSearchPartitionInstanceType_R6g_4xlarge_search,
        OpenSearchPartitionInstanceType_R6g_8xlarge_search,
        OpenSearchPartitionInstanceType_R6g_large_search,
        OpenSearchPartitionInstanceType_R6g_xlarge_search,
        OpenSearchPartitionInstanceType_R6gd_12xlarge_search,
        OpenSearchPartitionInstanceType_R6gd_16xlarge_search,
        OpenSearchPartitionInstanceType_R6gd_2xlarge_search,
        OpenSearchPartitionInstanceType_R6gd_4xlarge_search,
        OpenSearchPartitionInstanceType_R6gd_8xlarge_search,
        OpenSearchPartitionInstanceType_R6gd_large_search,
        OpenSearchPartitionInstanceType_R6gd_xlarge_search,
        OpenSearchPartitionInstanceType_T2_medium_search,
        OpenSearchPartitionInstanceType_T2_micro_search,
        OpenSearchPartitionInstanceType_T2_small_search,
        OpenSearchPartitionInstanceType_T3_2xlarge_search,
        OpenSearchPartitionInstanceType_T3_large_search,
        OpenSearchPartitionInstanceType_T3_medium_search,
        OpenSearchPartitionInstanceType_T3_micro_search,
        OpenSearchPartitionInstanceType_T3_nano_search,
        OpenSearchPartitionInstanceType_T3_small_search,
        OpenSearchPartitionInstanceType_T3_xlarge_search,
        OpenSearchPartitionInstanceType_T4g_medium_search,
        OpenSearchPartitionInstanceType_T4g_small_search,
        OpenSearchPartitionInstanceType_Ultrawarm1_large_search,
        OpenSearchPartitionInstanceType_Ultrawarm1_medium_search,
        OpenSearchPartitionInstanceType_Ultrawarm1_xlarge_search
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OpenSearchPartitionInstanceType = OpenSearchPartitionInstanceType'
  { fromOpenSearchPartitionInstanceType ::
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

pattern OpenSearchPartitionInstanceType_C4_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C4_2xlarge_search = OpenSearchPartitionInstanceType' "c4.2xlarge.search"

pattern OpenSearchPartitionInstanceType_C4_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C4_4xlarge_search = OpenSearchPartitionInstanceType' "c4.4xlarge.search"

pattern OpenSearchPartitionInstanceType_C4_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C4_8xlarge_search = OpenSearchPartitionInstanceType' "c4.8xlarge.search"

pattern OpenSearchPartitionInstanceType_C4_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C4_large_search = OpenSearchPartitionInstanceType' "c4.large.search"

pattern OpenSearchPartitionInstanceType_C4_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C4_xlarge_search = OpenSearchPartitionInstanceType' "c4.xlarge.search"

pattern OpenSearchPartitionInstanceType_C5_18xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C5_18xlarge_search = OpenSearchPartitionInstanceType' "c5.18xlarge.search"

pattern OpenSearchPartitionInstanceType_C5_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C5_2xlarge_search = OpenSearchPartitionInstanceType' "c5.2xlarge.search"

pattern OpenSearchPartitionInstanceType_C5_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C5_4xlarge_search = OpenSearchPartitionInstanceType' "c5.4xlarge.search"

pattern OpenSearchPartitionInstanceType_C5_9xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C5_9xlarge_search = OpenSearchPartitionInstanceType' "c5.9xlarge.search"

pattern OpenSearchPartitionInstanceType_C5_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C5_large_search = OpenSearchPartitionInstanceType' "c5.large.search"

pattern OpenSearchPartitionInstanceType_C5_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C5_xlarge_search = OpenSearchPartitionInstanceType' "c5.xlarge.search"

pattern OpenSearchPartitionInstanceType_C6g_12xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C6g_12xlarge_search = OpenSearchPartitionInstanceType' "c6g.12xlarge.search"

pattern OpenSearchPartitionInstanceType_C6g_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C6g_2xlarge_search = OpenSearchPartitionInstanceType' "c6g.2xlarge.search"

pattern OpenSearchPartitionInstanceType_C6g_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C6g_4xlarge_search = OpenSearchPartitionInstanceType' "c6g.4xlarge.search"

pattern OpenSearchPartitionInstanceType_C6g_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C6g_8xlarge_search = OpenSearchPartitionInstanceType' "c6g.8xlarge.search"

pattern OpenSearchPartitionInstanceType_C6g_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C6g_large_search = OpenSearchPartitionInstanceType' "c6g.large.search"

pattern OpenSearchPartitionInstanceType_C6g_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_C6g_xlarge_search = OpenSearchPartitionInstanceType' "c6g.xlarge.search"

pattern OpenSearchPartitionInstanceType_D2_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_D2_2xlarge_search = OpenSearchPartitionInstanceType' "d2.2xlarge.search"

pattern OpenSearchPartitionInstanceType_D2_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_D2_4xlarge_search = OpenSearchPartitionInstanceType' "d2.4xlarge.search"

pattern OpenSearchPartitionInstanceType_D2_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_D2_8xlarge_search = OpenSearchPartitionInstanceType' "d2.8xlarge.search"

pattern OpenSearchPartitionInstanceType_D2_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_D2_xlarge_search = OpenSearchPartitionInstanceType' "d2.xlarge.search"

pattern OpenSearchPartitionInstanceType_I2_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I2_2xlarge_search = OpenSearchPartitionInstanceType' "i2.2xlarge.search"

pattern OpenSearchPartitionInstanceType_I2_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I2_xlarge_search = OpenSearchPartitionInstanceType' "i2.xlarge.search"

pattern OpenSearchPartitionInstanceType_I3_16xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I3_16xlarge_search = OpenSearchPartitionInstanceType' "i3.16xlarge.search"

pattern OpenSearchPartitionInstanceType_I3_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I3_2xlarge_search = OpenSearchPartitionInstanceType' "i3.2xlarge.search"

pattern OpenSearchPartitionInstanceType_I3_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I3_4xlarge_search = OpenSearchPartitionInstanceType' "i3.4xlarge.search"

pattern OpenSearchPartitionInstanceType_I3_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I3_8xlarge_search = OpenSearchPartitionInstanceType' "i3.8xlarge.search"

pattern OpenSearchPartitionInstanceType_I3_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I3_large_search = OpenSearchPartitionInstanceType' "i3.large.search"

pattern OpenSearchPartitionInstanceType_I3_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_I3_xlarge_search = OpenSearchPartitionInstanceType' "i3.xlarge.search"

pattern OpenSearchPartitionInstanceType_M3_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M3_2xlarge_search = OpenSearchPartitionInstanceType' "m3.2xlarge.search"

pattern OpenSearchPartitionInstanceType_M3_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M3_large_search = OpenSearchPartitionInstanceType' "m3.large.search"

pattern OpenSearchPartitionInstanceType_M3_medium_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M3_medium_search = OpenSearchPartitionInstanceType' "m3.medium.search"

pattern OpenSearchPartitionInstanceType_M3_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M3_xlarge_search = OpenSearchPartitionInstanceType' "m3.xlarge.search"

pattern OpenSearchPartitionInstanceType_M4_10xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M4_10xlarge_search = OpenSearchPartitionInstanceType' "m4.10xlarge.search"

pattern OpenSearchPartitionInstanceType_M4_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M4_2xlarge_search = OpenSearchPartitionInstanceType' "m4.2xlarge.search"

pattern OpenSearchPartitionInstanceType_M4_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M4_4xlarge_search = OpenSearchPartitionInstanceType' "m4.4xlarge.search"

pattern OpenSearchPartitionInstanceType_M4_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M4_large_search = OpenSearchPartitionInstanceType' "m4.large.search"

pattern OpenSearchPartitionInstanceType_M4_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M4_xlarge_search = OpenSearchPartitionInstanceType' "m4.xlarge.search"

pattern OpenSearchPartitionInstanceType_M5_12xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M5_12xlarge_search = OpenSearchPartitionInstanceType' "m5.12xlarge.search"

pattern OpenSearchPartitionInstanceType_M5_24xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M5_24xlarge_search = OpenSearchPartitionInstanceType' "m5.24xlarge.search"

pattern OpenSearchPartitionInstanceType_M5_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M5_2xlarge_search = OpenSearchPartitionInstanceType' "m5.2xlarge.search"

pattern OpenSearchPartitionInstanceType_M5_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M5_4xlarge_search = OpenSearchPartitionInstanceType' "m5.4xlarge.search"

pattern OpenSearchPartitionInstanceType_M5_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M5_large_search = OpenSearchPartitionInstanceType' "m5.large.search"

pattern OpenSearchPartitionInstanceType_M5_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M5_xlarge_search = OpenSearchPartitionInstanceType' "m5.xlarge.search"

pattern OpenSearchPartitionInstanceType_M6g_12xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M6g_12xlarge_search = OpenSearchPartitionInstanceType' "m6g.12xlarge.search"

pattern OpenSearchPartitionInstanceType_M6g_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M6g_2xlarge_search = OpenSearchPartitionInstanceType' "m6g.2xlarge.search"

pattern OpenSearchPartitionInstanceType_M6g_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M6g_4xlarge_search = OpenSearchPartitionInstanceType' "m6g.4xlarge.search"

pattern OpenSearchPartitionInstanceType_M6g_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M6g_8xlarge_search = OpenSearchPartitionInstanceType' "m6g.8xlarge.search"

pattern OpenSearchPartitionInstanceType_M6g_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M6g_large_search = OpenSearchPartitionInstanceType' "m6g.large.search"

pattern OpenSearchPartitionInstanceType_M6g_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_M6g_xlarge_search = OpenSearchPartitionInstanceType' "m6g.xlarge.search"

pattern OpenSearchPartitionInstanceType_R3_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R3_2xlarge_search = OpenSearchPartitionInstanceType' "r3.2xlarge.search"

pattern OpenSearchPartitionInstanceType_R3_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R3_4xlarge_search = OpenSearchPartitionInstanceType' "r3.4xlarge.search"

pattern OpenSearchPartitionInstanceType_R3_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R3_8xlarge_search = OpenSearchPartitionInstanceType' "r3.8xlarge.search"

pattern OpenSearchPartitionInstanceType_R3_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R3_large_search = OpenSearchPartitionInstanceType' "r3.large.search"

pattern OpenSearchPartitionInstanceType_R3_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R3_xlarge_search = OpenSearchPartitionInstanceType' "r3.xlarge.search"

pattern OpenSearchPartitionInstanceType_R4_16xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R4_16xlarge_search = OpenSearchPartitionInstanceType' "r4.16xlarge.search"

pattern OpenSearchPartitionInstanceType_R4_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R4_2xlarge_search = OpenSearchPartitionInstanceType' "r4.2xlarge.search"

pattern OpenSearchPartitionInstanceType_R4_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R4_4xlarge_search = OpenSearchPartitionInstanceType' "r4.4xlarge.search"

pattern OpenSearchPartitionInstanceType_R4_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R4_8xlarge_search = OpenSearchPartitionInstanceType' "r4.8xlarge.search"

pattern OpenSearchPartitionInstanceType_R4_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R4_large_search = OpenSearchPartitionInstanceType' "r4.large.search"

pattern OpenSearchPartitionInstanceType_R4_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R4_xlarge_search = OpenSearchPartitionInstanceType' "r4.xlarge.search"

pattern OpenSearchPartitionInstanceType_R5_12xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R5_12xlarge_search = OpenSearchPartitionInstanceType' "r5.12xlarge.search"

pattern OpenSearchPartitionInstanceType_R5_24xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R5_24xlarge_search = OpenSearchPartitionInstanceType' "r5.24xlarge.search"

pattern OpenSearchPartitionInstanceType_R5_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R5_2xlarge_search = OpenSearchPartitionInstanceType' "r5.2xlarge.search"

pattern OpenSearchPartitionInstanceType_R5_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R5_4xlarge_search = OpenSearchPartitionInstanceType' "r5.4xlarge.search"

pattern OpenSearchPartitionInstanceType_R5_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R5_large_search = OpenSearchPartitionInstanceType' "r5.large.search"

pattern OpenSearchPartitionInstanceType_R5_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R5_xlarge_search = OpenSearchPartitionInstanceType' "r5.xlarge.search"

pattern OpenSearchPartitionInstanceType_R6g_12xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6g_12xlarge_search = OpenSearchPartitionInstanceType' "r6g.12xlarge.search"

pattern OpenSearchPartitionInstanceType_R6g_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6g_2xlarge_search = OpenSearchPartitionInstanceType' "r6g.2xlarge.search"

pattern OpenSearchPartitionInstanceType_R6g_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6g_4xlarge_search = OpenSearchPartitionInstanceType' "r6g.4xlarge.search"

pattern OpenSearchPartitionInstanceType_R6g_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6g_8xlarge_search = OpenSearchPartitionInstanceType' "r6g.8xlarge.search"

pattern OpenSearchPartitionInstanceType_R6g_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6g_large_search = OpenSearchPartitionInstanceType' "r6g.large.search"

pattern OpenSearchPartitionInstanceType_R6g_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6g_xlarge_search = OpenSearchPartitionInstanceType' "r6g.xlarge.search"

pattern OpenSearchPartitionInstanceType_R6gd_12xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_12xlarge_search = OpenSearchPartitionInstanceType' "r6gd.12xlarge.search"

pattern OpenSearchPartitionInstanceType_R6gd_16xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_16xlarge_search = OpenSearchPartitionInstanceType' "r6gd.16xlarge.search"

pattern OpenSearchPartitionInstanceType_R6gd_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_2xlarge_search = OpenSearchPartitionInstanceType' "r6gd.2xlarge.search"

pattern OpenSearchPartitionInstanceType_R6gd_4xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_4xlarge_search = OpenSearchPartitionInstanceType' "r6gd.4xlarge.search"

pattern OpenSearchPartitionInstanceType_R6gd_8xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_8xlarge_search = OpenSearchPartitionInstanceType' "r6gd.8xlarge.search"

pattern OpenSearchPartitionInstanceType_R6gd_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_large_search = OpenSearchPartitionInstanceType' "r6gd.large.search"

pattern OpenSearchPartitionInstanceType_R6gd_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_R6gd_xlarge_search = OpenSearchPartitionInstanceType' "r6gd.xlarge.search"

pattern OpenSearchPartitionInstanceType_T2_medium_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T2_medium_search = OpenSearchPartitionInstanceType' "t2.medium.search"

pattern OpenSearchPartitionInstanceType_T2_micro_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T2_micro_search = OpenSearchPartitionInstanceType' "t2.micro.search"

pattern OpenSearchPartitionInstanceType_T2_small_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T2_small_search = OpenSearchPartitionInstanceType' "t2.small.search"

pattern OpenSearchPartitionInstanceType_T3_2xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_2xlarge_search = OpenSearchPartitionInstanceType' "t3.2xlarge.search"

pattern OpenSearchPartitionInstanceType_T3_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_large_search = OpenSearchPartitionInstanceType' "t3.large.search"

pattern OpenSearchPartitionInstanceType_T3_medium_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_medium_search = OpenSearchPartitionInstanceType' "t3.medium.search"

pattern OpenSearchPartitionInstanceType_T3_micro_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_micro_search = OpenSearchPartitionInstanceType' "t3.micro.search"

pattern OpenSearchPartitionInstanceType_T3_nano_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_nano_search = OpenSearchPartitionInstanceType' "t3.nano.search"

pattern OpenSearchPartitionInstanceType_T3_small_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_small_search = OpenSearchPartitionInstanceType' "t3.small.search"

pattern OpenSearchPartitionInstanceType_T3_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T3_xlarge_search = OpenSearchPartitionInstanceType' "t3.xlarge.search"

pattern OpenSearchPartitionInstanceType_T4g_medium_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T4g_medium_search = OpenSearchPartitionInstanceType' "t4g.medium.search"

pattern OpenSearchPartitionInstanceType_T4g_small_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_T4g_small_search = OpenSearchPartitionInstanceType' "t4g.small.search"

pattern OpenSearchPartitionInstanceType_Ultrawarm1_large_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_Ultrawarm1_large_search = OpenSearchPartitionInstanceType' "ultrawarm1.large.search"

pattern OpenSearchPartitionInstanceType_Ultrawarm1_medium_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_Ultrawarm1_medium_search = OpenSearchPartitionInstanceType' "ultrawarm1.medium.search"

pattern OpenSearchPartitionInstanceType_Ultrawarm1_xlarge_search :: OpenSearchPartitionInstanceType
pattern OpenSearchPartitionInstanceType_Ultrawarm1_xlarge_search = OpenSearchPartitionInstanceType' "ultrawarm1.xlarge.search"

{-# COMPLETE
  OpenSearchPartitionInstanceType_C4_2xlarge_search,
  OpenSearchPartitionInstanceType_C4_4xlarge_search,
  OpenSearchPartitionInstanceType_C4_8xlarge_search,
  OpenSearchPartitionInstanceType_C4_large_search,
  OpenSearchPartitionInstanceType_C4_xlarge_search,
  OpenSearchPartitionInstanceType_C5_18xlarge_search,
  OpenSearchPartitionInstanceType_C5_2xlarge_search,
  OpenSearchPartitionInstanceType_C5_4xlarge_search,
  OpenSearchPartitionInstanceType_C5_9xlarge_search,
  OpenSearchPartitionInstanceType_C5_large_search,
  OpenSearchPartitionInstanceType_C5_xlarge_search,
  OpenSearchPartitionInstanceType_C6g_12xlarge_search,
  OpenSearchPartitionInstanceType_C6g_2xlarge_search,
  OpenSearchPartitionInstanceType_C6g_4xlarge_search,
  OpenSearchPartitionInstanceType_C6g_8xlarge_search,
  OpenSearchPartitionInstanceType_C6g_large_search,
  OpenSearchPartitionInstanceType_C6g_xlarge_search,
  OpenSearchPartitionInstanceType_D2_2xlarge_search,
  OpenSearchPartitionInstanceType_D2_4xlarge_search,
  OpenSearchPartitionInstanceType_D2_8xlarge_search,
  OpenSearchPartitionInstanceType_D2_xlarge_search,
  OpenSearchPartitionInstanceType_I2_2xlarge_search,
  OpenSearchPartitionInstanceType_I2_xlarge_search,
  OpenSearchPartitionInstanceType_I3_16xlarge_search,
  OpenSearchPartitionInstanceType_I3_2xlarge_search,
  OpenSearchPartitionInstanceType_I3_4xlarge_search,
  OpenSearchPartitionInstanceType_I3_8xlarge_search,
  OpenSearchPartitionInstanceType_I3_large_search,
  OpenSearchPartitionInstanceType_I3_xlarge_search,
  OpenSearchPartitionInstanceType_M3_2xlarge_search,
  OpenSearchPartitionInstanceType_M3_large_search,
  OpenSearchPartitionInstanceType_M3_medium_search,
  OpenSearchPartitionInstanceType_M3_xlarge_search,
  OpenSearchPartitionInstanceType_M4_10xlarge_search,
  OpenSearchPartitionInstanceType_M4_2xlarge_search,
  OpenSearchPartitionInstanceType_M4_4xlarge_search,
  OpenSearchPartitionInstanceType_M4_large_search,
  OpenSearchPartitionInstanceType_M4_xlarge_search,
  OpenSearchPartitionInstanceType_M5_12xlarge_search,
  OpenSearchPartitionInstanceType_M5_24xlarge_search,
  OpenSearchPartitionInstanceType_M5_2xlarge_search,
  OpenSearchPartitionInstanceType_M5_4xlarge_search,
  OpenSearchPartitionInstanceType_M5_large_search,
  OpenSearchPartitionInstanceType_M5_xlarge_search,
  OpenSearchPartitionInstanceType_M6g_12xlarge_search,
  OpenSearchPartitionInstanceType_M6g_2xlarge_search,
  OpenSearchPartitionInstanceType_M6g_4xlarge_search,
  OpenSearchPartitionInstanceType_M6g_8xlarge_search,
  OpenSearchPartitionInstanceType_M6g_large_search,
  OpenSearchPartitionInstanceType_M6g_xlarge_search,
  OpenSearchPartitionInstanceType_R3_2xlarge_search,
  OpenSearchPartitionInstanceType_R3_4xlarge_search,
  OpenSearchPartitionInstanceType_R3_8xlarge_search,
  OpenSearchPartitionInstanceType_R3_large_search,
  OpenSearchPartitionInstanceType_R3_xlarge_search,
  OpenSearchPartitionInstanceType_R4_16xlarge_search,
  OpenSearchPartitionInstanceType_R4_2xlarge_search,
  OpenSearchPartitionInstanceType_R4_4xlarge_search,
  OpenSearchPartitionInstanceType_R4_8xlarge_search,
  OpenSearchPartitionInstanceType_R4_large_search,
  OpenSearchPartitionInstanceType_R4_xlarge_search,
  OpenSearchPartitionInstanceType_R5_12xlarge_search,
  OpenSearchPartitionInstanceType_R5_24xlarge_search,
  OpenSearchPartitionInstanceType_R5_2xlarge_search,
  OpenSearchPartitionInstanceType_R5_4xlarge_search,
  OpenSearchPartitionInstanceType_R5_large_search,
  OpenSearchPartitionInstanceType_R5_xlarge_search,
  OpenSearchPartitionInstanceType_R6g_12xlarge_search,
  OpenSearchPartitionInstanceType_R6g_2xlarge_search,
  OpenSearchPartitionInstanceType_R6g_4xlarge_search,
  OpenSearchPartitionInstanceType_R6g_8xlarge_search,
  OpenSearchPartitionInstanceType_R6g_large_search,
  OpenSearchPartitionInstanceType_R6g_xlarge_search,
  OpenSearchPartitionInstanceType_R6gd_12xlarge_search,
  OpenSearchPartitionInstanceType_R6gd_16xlarge_search,
  OpenSearchPartitionInstanceType_R6gd_2xlarge_search,
  OpenSearchPartitionInstanceType_R6gd_4xlarge_search,
  OpenSearchPartitionInstanceType_R6gd_8xlarge_search,
  OpenSearchPartitionInstanceType_R6gd_large_search,
  OpenSearchPartitionInstanceType_R6gd_xlarge_search,
  OpenSearchPartitionInstanceType_T2_medium_search,
  OpenSearchPartitionInstanceType_T2_micro_search,
  OpenSearchPartitionInstanceType_T2_small_search,
  OpenSearchPartitionInstanceType_T3_2xlarge_search,
  OpenSearchPartitionInstanceType_T3_large_search,
  OpenSearchPartitionInstanceType_T3_medium_search,
  OpenSearchPartitionInstanceType_T3_micro_search,
  OpenSearchPartitionInstanceType_T3_nano_search,
  OpenSearchPartitionInstanceType_T3_small_search,
  OpenSearchPartitionInstanceType_T3_xlarge_search,
  OpenSearchPartitionInstanceType_T4g_medium_search,
  OpenSearchPartitionInstanceType_T4g_small_search,
  OpenSearchPartitionInstanceType_Ultrawarm1_large_search,
  OpenSearchPartitionInstanceType_Ultrawarm1_medium_search,
  OpenSearchPartitionInstanceType_Ultrawarm1_xlarge_search,
  OpenSearchPartitionInstanceType'
  #-}
