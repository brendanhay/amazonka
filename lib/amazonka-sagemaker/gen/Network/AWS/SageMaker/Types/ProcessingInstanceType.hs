{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ProcessingInstanceType
  ( ProcessingInstanceType
    ( ProcessingInstanceType'
    , ProcessingInstanceTypeMl_T3_Medium
    , ProcessingInstanceTypeMl_T3_Large
    , ProcessingInstanceTypeMl_T3_Xlarge
    , ProcessingInstanceTypeMl_T3_2xlarge
    , ProcessingInstanceTypeMl_M4_Xlarge
    , ProcessingInstanceTypeMl_M4_2xlarge
    , ProcessingInstanceTypeMl_M4_4xlarge
    , ProcessingInstanceTypeMl_M4_10xlarge
    , ProcessingInstanceTypeMl_M4_16xlarge
    , ProcessingInstanceTypeMl_C4_Xlarge
    , ProcessingInstanceTypeMl_C4_2xlarge
    , ProcessingInstanceTypeMl_C4_4xlarge
    , ProcessingInstanceTypeMl_C4_8xlarge
    , ProcessingInstanceTypeMl_P2_Xlarge
    , ProcessingInstanceTypeMl_P2_8xlarge
    , ProcessingInstanceTypeMl_P2_16xlarge
    , ProcessingInstanceTypeMl_P3_2xlarge
    , ProcessingInstanceTypeMl_P3_8xlarge
    , ProcessingInstanceTypeMl_P3_16xlarge
    , ProcessingInstanceTypeMl_C5_Xlarge
    , ProcessingInstanceTypeMl_C5_2xlarge
    , ProcessingInstanceTypeMl_C5_4xlarge
    , ProcessingInstanceTypeMl_C5_9xlarge
    , ProcessingInstanceTypeMl_C5_18xlarge
    , ProcessingInstanceTypeMl_M5_Large
    , ProcessingInstanceTypeMl_M5_Xlarge
    , ProcessingInstanceTypeMl_M5_2xlarge
    , ProcessingInstanceTypeMl_M5_4xlarge
    , ProcessingInstanceTypeMl_M5_12xlarge
    , ProcessingInstanceTypeMl_M5_24xlarge
    , ProcessingInstanceTypeMl_R5_Large
    , ProcessingInstanceTypeMl_R5_Xlarge
    , ProcessingInstanceTypeMl_R5_2xlarge
    , ProcessingInstanceTypeMl_R5_4xlarge
    , ProcessingInstanceTypeMl_R5_8xlarge
    , ProcessingInstanceTypeMl_R5_12xlarge
    , ProcessingInstanceTypeMl_R5_16xlarge
    , ProcessingInstanceTypeMl_R5_24xlarge
    , fromProcessingInstanceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProcessingInstanceType = ProcessingInstanceType'{fromProcessingInstanceType
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern ProcessingInstanceTypeMl_T3_Medium :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_T3_Medium = ProcessingInstanceType' "ml.t3.medium"

pattern ProcessingInstanceTypeMl_T3_Large :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_T3_Large = ProcessingInstanceType' "ml.t3.large"

pattern ProcessingInstanceTypeMl_T3_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_T3_Xlarge = ProcessingInstanceType' "ml.t3.xlarge"

pattern ProcessingInstanceTypeMl_T3_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_T3_2xlarge = ProcessingInstanceType' "ml.t3.2xlarge"

pattern ProcessingInstanceTypeMl_M4_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M4_Xlarge = ProcessingInstanceType' "ml.m4.xlarge"

pattern ProcessingInstanceTypeMl_M4_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M4_2xlarge = ProcessingInstanceType' "ml.m4.2xlarge"

pattern ProcessingInstanceTypeMl_M4_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M4_4xlarge = ProcessingInstanceType' "ml.m4.4xlarge"

pattern ProcessingInstanceTypeMl_M4_10xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M4_10xlarge = ProcessingInstanceType' "ml.m4.10xlarge"

pattern ProcessingInstanceTypeMl_M4_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M4_16xlarge = ProcessingInstanceType' "ml.m4.16xlarge"

pattern ProcessingInstanceTypeMl_C4_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C4_Xlarge = ProcessingInstanceType' "ml.c4.xlarge"

pattern ProcessingInstanceTypeMl_C4_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C4_2xlarge = ProcessingInstanceType' "ml.c4.2xlarge"

pattern ProcessingInstanceTypeMl_C4_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C4_4xlarge = ProcessingInstanceType' "ml.c4.4xlarge"

pattern ProcessingInstanceTypeMl_C4_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C4_8xlarge = ProcessingInstanceType' "ml.c4.8xlarge"

pattern ProcessingInstanceTypeMl_P2_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_P2_Xlarge = ProcessingInstanceType' "ml.p2.xlarge"

pattern ProcessingInstanceTypeMl_P2_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_P2_8xlarge = ProcessingInstanceType' "ml.p2.8xlarge"

pattern ProcessingInstanceTypeMl_P2_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_P2_16xlarge = ProcessingInstanceType' "ml.p2.16xlarge"

pattern ProcessingInstanceTypeMl_P3_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_P3_2xlarge = ProcessingInstanceType' "ml.p3.2xlarge"

pattern ProcessingInstanceTypeMl_P3_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_P3_8xlarge = ProcessingInstanceType' "ml.p3.8xlarge"

pattern ProcessingInstanceTypeMl_P3_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_P3_16xlarge = ProcessingInstanceType' "ml.p3.16xlarge"

pattern ProcessingInstanceTypeMl_C5_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C5_Xlarge = ProcessingInstanceType' "ml.c5.xlarge"

pattern ProcessingInstanceTypeMl_C5_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C5_2xlarge = ProcessingInstanceType' "ml.c5.2xlarge"

pattern ProcessingInstanceTypeMl_C5_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C5_4xlarge = ProcessingInstanceType' "ml.c5.4xlarge"

pattern ProcessingInstanceTypeMl_C5_9xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C5_9xlarge = ProcessingInstanceType' "ml.c5.9xlarge"

pattern ProcessingInstanceTypeMl_C5_18xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_C5_18xlarge = ProcessingInstanceType' "ml.c5.18xlarge"

pattern ProcessingInstanceTypeMl_M5_Large :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M5_Large = ProcessingInstanceType' "ml.m5.large"

pattern ProcessingInstanceTypeMl_M5_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M5_Xlarge = ProcessingInstanceType' "ml.m5.xlarge"

pattern ProcessingInstanceTypeMl_M5_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M5_2xlarge = ProcessingInstanceType' "ml.m5.2xlarge"

pattern ProcessingInstanceTypeMl_M5_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M5_4xlarge = ProcessingInstanceType' "ml.m5.4xlarge"

pattern ProcessingInstanceTypeMl_M5_12xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M5_12xlarge = ProcessingInstanceType' "ml.m5.12xlarge"

pattern ProcessingInstanceTypeMl_M5_24xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_M5_24xlarge = ProcessingInstanceType' "ml.m5.24xlarge"

pattern ProcessingInstanceTypeMl_R5_Large :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_Large = ProcessingInstanceType' "ml.r5.large"

pattern ProcessingInstanceTypeMl_R5_Xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_Xlarge = ProcessingInstanceType' "ml.r5.xlarge"

pattern ProcessingInstanceTypeMl_R5_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_2xlarge = ProcessingInstanceType' "ml.r5.2xlarge"

pattern ProcessingInstanceTypeMl_R5_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_4xlarge = ProcessingInstanceType' "ml.r5.4xlarge"

pattern ProcessingInstanceTypeMl_R5_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_8xlarge = ProcessingInstanceType' "ml.r5.8xlarge"

pattern ProcessingInstanceTypeMl_R5_12xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_12xlarge = ProcessingInstanceType' "ml.r5.12xlarge"

pattern ProcessingInstanceTypeMl_R5_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_16xlarge = ProcessingInstanceType' "ml.r5.16xlarge"

pattern ProcessingInstanceTypeMl_R5_24xlarge :: ProcessingInstanceType
pattern ProcessingInstanceTypeMl_R5_24xlarge = ProcessingInstanceType' "ml.r5.24xlarge"

{-# COMPLETE 
  ProcessingInstanceTypeMl_T3_Medium,

  ProcessingInstanceTypeMl_T3_Large,

  ProcessingInstanceTypeMl_T3_Xlarge,

  ProcessingInstanceTypeMl_T3_2xlarge,

  ProcessingInstanceTypeMl_M4_Xlarge,

  ProcessingInstanceTypeMl_M4_2xlarge,

  ProcessingInstanceTypeMl_M4_4xlarge,

  ProcessingInstanceTypeMl_M4_10xlarge,

  ProcessingInstanceTypeMl_M4_16xlarge,

  ProcessingInstanceTypeMl_C4_Xlarge,

  ProcessingInstanceTypeMl_C4_2xlarge,

  ProcessingInstanceTypeMl_C4_4xlarge,

  ProcessingInstanceTypeMl_C4_8xlarge,

  ProcessingInstanceTypeMl_P2_Xlarge,

  ProcessingInstanceTypeMl_P2_8xlarge,

  ProcessingInstanceTypeMl_P2_16xlarge,

  ProcessingInstanceTypeMl_P3_2xlarge,

  ProcessingInstanceTypeMl_P3_8xlarge,

  ProcessingInstanceTypeMl_P3_16xlarge,

  ProcessingInstanceTypeMl_C5_Xlarge,

  ProcessingInstanceTypeMl_C5_2xlarge,

  ProcessingInstanceTypeMl_C5_4xlarge,

  ProcessingInstanceTypeMl_C5_9xlarge,

  ProcessingInstanceTypeMl_C5_18xlarge,

  ProcessingInstanceTypeMl_M5_Large,

  ProcessingInstanceTypeMl_M5_Xlarge,

  ProcessingInstanceTypeMl_M5_2xlarge,

  ProcessingInstanceTypeMl_M5_4xlarge,

  ProcessingInstanceTypeMl_M5_12xlarge,

  ProcessingInstanceTypeMl_M5_24xlarge,

  ProcessingInstanceTypeMl_R5_Large,

  ProcessingInstanceTypeMl_R5_Xlarge,

  ProcessingInstanceTypeMl_R5_2xlarge,

  ProcessingInstanceTypeMl_R5_4xlarge,

  ProcessingInstanceTypeMl_R5_8xlarge,

  ProcessingInstanceTypeMl_R5_12xlarge,

  ProcessingInstanceTypeMl_R5_16xlarge,

  ProcessingInstanceTypeMl_R5_24xlarge,
  ProcessingInstanceType'
  #-}
