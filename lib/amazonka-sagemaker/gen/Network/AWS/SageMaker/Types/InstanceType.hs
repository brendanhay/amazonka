{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InstanceType
  ( InstanceType
      ( InstanceType',
        InstanceTypeMl_T2_Medium,
        InstanceTypeMl_T2_Large,
        InstanceTypeMl_T2_Xlarge,
        InstanceTypeMl_T2_2xlarge,
        InstanceTypeMl_T3_Medium,
        InstanceTypeMl_T3_Large,
        InstanceTypeMl_T3_Xlarge,
        InstanceTypeMl_T3_2xlarge,
        InstanceTypeMl_M4_Xlarge,
        InstanceTypeMl_M4_2xlarge,
        InstanceTypeMl_M4_4xlarge,
        InstanceTypeMl_M4_10xlarge,
        InstanceTypeMl_M4_16xlarge,
        InstanceTypeMl_M5_Xlarge,
        InstanceTypeMl_M5_2xlarge,
        InstanceTypeMl_M5_4xlarge,
        InstanceTypeMl_M5_12xlarge,
        InstanceTypeMl_M5_24xlarge,
        InstanceTypeMl_C4_Xlarge,
        InstanceTypeMl_C4_2xlarge,
        InstanceTypeMl_C4_4xlarge,
        InstanceTypeMl_C4_8xlarge,
        InstanceTypeMl_C5_Xlarge,
        InstanceTypeMl_C5_2xlarge,
        InstanceTypeMl_C5_4xlarge,
        InstanceTypeMl_C5_9xlarge,
        InstanceTypeMl_C5_18xlarge,
        InstanceTypeMl_C5d_Xlarge,
        InstanceTypeMl_C5d_2xlarge,
        InstanceTypeMl_C5d_4xlarge,
        InstanceTypeMl_C5d_9xlarge,
        InstanceTypeMl_C5d_18xlarge,
        InstanceTypeMl_P2_Xlarge,
        InstanceTypeMl_P2_8xlarge,
        InstanceTypeMl_P2_16xlarge,
        InstanceTypeMl_P3_2xlarge,
        InstanceTypeMl_P3_8xlarge,
        InstanceTypeMl_P3_16xlarge,
        fromInstanceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype InstanceType = InstanceType' {fromInstanceType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern InstanceTypeMl_T2_Medium :: InstanceType
pattern InstanceTypeMl_T2_Medium = InstanceType' "ml.t2.medium"

pattern InstanceTypeMl_T2_Large :: InstanceType
pattern InstanceTypeMl_T2_Large = InstanceType' "ml.t2.large"

pattern InstanceTypeMl_T2_Xlarge :: InstanceType
pattern InstanceTypeMl_T2_Xlarge = InstanceType' "ml.t2.xlarge"

pattern InstanceTypeMl_T2_2xlarge :: InstanceType
pattern InstanceTypeMl_T2_2xlarge = InstanceType' "ml.t2.2xlarge"

pattern InstanceTypeMl_T3_Medium :: InstanceType
pattern InstanceTypeMl_T3_Medium = InstanceType' "ml.t3.medium"

pattern InstanceTypeMl_T3_Large :: InstanceType
pattern InstanceTypeMl_T3_Large = InstanceType' "ml.t3.large"

pattern InstanceTypeMl_T3_Xlarge :: InstanceType
pattern InstanceTypeMl_T3_Xlarge = InstanceType' "ml.t3.xlarge"

pattern InstanceTypeMl_T3_2xlarge :: InstanceType
pattern InstanceTypeMl_T3_2xlarge = InstanceType' "ml.t3.2xlarge"

pattern InstanceTypeMl_M4_Xlarge :: InstanceType
pattern InstanceTypeMl_M4_Xlarge = InstanceType' "ml.m4.xlarge"

pattern InstanceTypeMl_M4_2xlarge :: InstanceType
pattern InstanceTypeMl_M4_2xlarge = InstanceType' "ml.m4.2xlarge"

pattern InstanceTypeMl_M4_4xlarge :: InstanceType
pattern InstanceTypeMl_M4_4xlarge = InstanceType' "ml.m4.4xlarge"

pattern InstanceTypeMl_M4_10xlarge :: InstanceType
pattern InstanceTypeMl_M4_10xlarge = InstanceType' "ml.m4.10xlarge"

pattern InstanceTypeMl_M4_16xlarge :: InstanceType
pattern InstanceTypeMl_M4_16xlarge = InstanceType' "ml.m4.16xlarge"

pattern InstanceTypeMl_M5_Xlarge :: InstanceType
pattern InstanceTypeMl_M5_Xlarge = InstanceType' "ml.m5.xlarge"

pattern InstanceTypeMl_M5_2xlarge :: InstanceType
pattern InstanceTypeMl_M5_2xlarge = InstanceType' "ml.m5.2xlarge"

pattern InstanceTypeMl_M5_4xlarge :: InstanceType
pattern InstanceTypeMl_M5_4xlarge = InstanceType' "ml.m5.4xlarge"

pattern InstanceTypeMl_M5_12xlarge :: InstanceType
pattern InstanceTypeMl_M5_12xlarge = InstanceType' "ml.m5.12xlarge"

pattern InstanceTypeMl_M5_24xlarge :: InstanceType
pattern InstanceTypeMl_M5_24xlarge = InstanceType' "ml.m5.24xlarge"

pattern InstanceTypeMl_C4_Xlarge :: InstanceType
pattern InstanceTypeMl_C4_Xlarge = InstanceType' "ml.c4.xlarge"

pattern InstanceTypeMl_C4_2xlarge :: InstanceType
pattern InstanceTypeMl_C4_2xlarge = InstanceType' "ml.c4.2xlarge"

pattern InstanceTypeMl_C4_4xlarge :: InstanceType
pattern InstanceTypeMl_C4_4xlarge = InstanceType' "ml.c4.4xlarge"

pattern InstanceTypeMl_C4_8xlarge :: InstanceType
pattern InstanceTypeMl_C4_8xlarge = InstanceType' "ml.c4.8xlarge"

pattern InstanceTypeMl_C5_Xlarge :: InstanceType
pattern InstanceTypeMl_C5_Xlarge = InstanceType' "ml.c5.xlarge"

pattern InstanceTypeMl_C5_2xlarge :: InstanceType
pattern InstanceTypeMl_C5_2xlarge = InstanceType' "ml.c5.2xlarge"

pattern InstanceTypeMl_C5_4xlarge :: InstanceType
pattern InstanceTypeMl_C5_4xlarge = InstanceType' "ml.c5.4xlarge"

pattern InstanceTypeMl_C5_9xlarge :: InstanceType
pattern InstanceTypeMl_C5_9xlarge = InstanceType' "ml.c5.9xlarge"

pattern InstanceTypeMl_C5_18xlarge :: InstanceType
pattern InstanceTypeMl_C5_18xlarge = InstanceType' "ml.c5.18xlarge"

pattern InstanceTypeMl_C5d_Xlarge :: InstanceType
pattern InstanceTypeMl_C5d_Xlarge = InstanceType' "ml.c5d.xlarge"

pattern InstanceTypeMl_C5d_2xlarge :: InstanceType
pattern InstanceTypeMl_C5d_2xlarge = InstanceType' "ml.c5d.2xlarge"

pattern InstanceTypeMl_C5d_4xlarge :: InstanceType
pattern InstanceTypeMl_C5d_4xlarge = InstanceType' "ml.c5d.4xlarge"

pattern InstanceTypeMl_C5d_9xlarge :: InstanceType
pattern InstanceTypeMl_C5d_9xlarge = InstanceType' "ml.c5d.9xlarge"

pattern InstanceTypeMl_C5d_18xlarge :: InstanceType
pattern InstanceTypeMl_C5d_18xlarge = InstanceType' "ml.c5d.18xlarge"

pattern InstanceTypeMl_P2_Xlarge :: InstanceType
pattern InstanceTypeMl_P2_Xlarge = InstanceType' "ml.p2.xlarge"

pattern InstanceTypeMl_P2_8xlarge :: InstanceType
pattern InstanceTypeMl_P2_8xlarge = InstanceType' "ml.p2.8xlarge"

pattern InstanceTypeMl_P2_16xlarge :: InstanceType
pattern InstanceTypeMl_P2_16xlarge = InstanceType' "ml.p2.16xlarge"

pattern InstanceTypeMl_P3_2xlarge :: InstanceType
pattern InstanceTypeMl_P3_2xlarge = InstanceType' "ml.p3.2xlarge"

pattern InstanceTypeMl_P3_8xlarge :: InstanceType
pattern InstanceTypeMl_P3_8xlarge = InstanceType' "ml.p3.8xlarge"

pattern InstanceTypeMl_P3_16xlarge :: InstanceType
pattern InstanceTypeMl_P3_16xlarge = InstanceType' "ml.p3.16xlarge"

{-# COMPLETE
  InstanceTypeMl_T2_Medium,
  InstanceTypeMl_T2_Large,
  InstanceTypeMl_T2_Xlarge,
  InstanceTypeMl_T2_2xlarge,
  InstanceTypeMl_T3_Medium,
  InstanceTypeMl_T3_Large,
  InstanceTypeMl_T3_Xlarge,
  InstanceTypeMl_T3_2xlarge,
  InstanceTypeMl_M4_Xlarge,
  InstanceTypeMl_M4_2xlarge,
  InstanceTypeMl_M4_4xlarge,
  InstanceTypeMl_M4_10xlarge,
  InstanceTypeMl_M4_16xlarge,
  InstanceTypeMl_M5_Xlarge,
  InstanceTypeMl_M5_2xlarge,
  InstanceTypeMl_M5_4xlarge,
  InstanceTypeMl_M5_12xlarge,
  InstanceTypeMl_M5_24xlarge,
  InstanceTypeMl_C4_Xlarge,
  InstanceTypeMl_C4_2xlarge,
  InstanceTypeMl_C4_4xlarge,
  InstanceTypeMl_C4_8xlarge,
  InstanceTypeMl_C5_Xlarge,
  InstanceTypeMl_C5_2xlarge,
  InstanceTypeMl_C5_4xlarge,
  InstanceTypeMl_C5_9xlarge,
  InstanceTypeMl_C5_18xlarge,
  InstanceTypeMl_C5d_Xlarge,
  InstanceTypeMl_C5d_2xlarge,
  InstanceTypeMl_C5d_4xlarge,
  InstanceTypeMl_C5d_9xlarge,
  InstanceTypeMl_C5d_18xlarge,
  InstanceTypeMl_P2_Xlarge,
  InstanceTypeMl_P2_8xlarge,
  InstanceTypeMl_P2_16xlarge,
  InstanceTypeMl_P3_2xlarge,
  InstanceTypeMl_P3_8xlarge,
  InstanceTypeMl_P3_16xlarge,
  InstanceType'
  #-}
