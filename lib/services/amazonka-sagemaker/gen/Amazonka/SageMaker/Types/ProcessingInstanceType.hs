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
-- Module      : Amazonka.SageMaker.Types.ProcessingInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingInstanceType
  ( ProcessingInstanceType
      ( ..,
        ProcessingInstanceType_Ml_c4_2xlarge,
        ProcessingInstanceType_Ml_c4_4xlarge,
        ProcessingInstanceType_Ml_c4_8xlarge,
        ProcessingInstanceType_Ml_c4_xlarge,
        ProcessingInstanceType_Ml_c5_18xlarge,
        ProcessingInstanceType_Ml_c5_2xlarge,
        ProcessingInstanceType_Ml_c5_4xlarge,
        ProcessingInstanceType_Ml_c5_9xlarge,
        ProcessingInstanceType_Ml_c5_xlarge,
        ProcessingInstanceType_Ml_g4dn_12xlarge,
        ProcessingInstanceType_Ml_g4dn_16xlarge,
        ProcessingInstanceType_Ml_g4dn_2xlarge,
        ProcessingInstanceType_Ml_g4dn_4xlarge,
        ProcessingInstanceType_Ml_g4dn_8xlarge,
        ProcessingInstanceType_Ml_g4dn_xlarge,
        ProcessingInstanceType_Ml_m4_10xlarge,
        ProcessingInstanceType_Ml_m4_16xlarge,
        ProcessingInstanceType_Ml_m4_2xlarge,
        ProcessingInstanceType_Ml_m4_4xlarge,
        ProcessingInstanceType_Ml_m4_xlarge,
        ProcessingInstanceType_Ml_m5_12xlarge,
        ProcessingInstanceType_Ml_m5_24xlarge,
        ProcessingInstanceType_Ml_m5_2xlarge,
        ProcessingInstanceType_Ml_m5_4xlarge,
        ProcessingInstanceType_Ml_m5_large,
        ProcessingInstanceType_Ml_m5_xlarge,
        ProcessingInstanceType_Ml_p2_16xlarge,
        ProcessingInstanceType_Ml_p2_8xlarge,
        ProcessingInstanceType_Ml_p2_xlarge,
        ProcessingInstanceType_Ml_p3_16xlarge,
        ProcessingInstanceType_Ml_p3_2xlarge,
        ProcessingInstanceType_Ml_p3_8xlarge,
        ProcessingInstanceType_Ml_r5_12xlarge,
        ProcessingInstanceType_Ml_r5_16xlarge,
        ProcessingInstanceType_Ml_r5_24xlarge,
        ProcessingInstanceType_Ml_r5_2xlarge,
        ProcessingInstanceType_Ml_r5_4xlarge,
        ProcessingInstanceType_Ml_r5_8xlarge,
        ProcessingInstanceType_Ml_r5_large,
        ProcessingInstanceType_Ml_r5_xlarge,
        ProcessingInstanceType_Ml_t3_2xlarge,
        ProcessingInstanceType_Ml_t3_large,
        ProcessingInstanceType_Ml_t3_medium,
        ProcessingInstanceType_Ml_t3_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProcessingInstanceType = ProcessingInstanceType'
  { fromProcessingInstanceType ::
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

pattern ProcessingInstanceType_Ml_c4_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c4_2xlarge = ProcessingInstanceType' "ml.c4.2xlarge"

pattern ProcessingInstanceType_Ml_c4_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c4_4xlarge = ProcessingInstanceType' "ml.c4.4xlarge"

pattern ProcessingInstanceType_Ml_c4_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c4_8xlarge = ProcessingInstanceType' "ml.c4.8xlarge"

pattern ProcessingInstanceType_Ml_c4_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c4_xlarge = ProcessingInstanceType' "ml.c4.xlarge"

pattern ProcessingInstanceType_Ml_c5_18xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c5_18xlarge = ProcessingInstanceType' "ml.c5.18xlarge"

pattern ProcessingInstanceType_Ml_c5_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c5_2xlarge = ProcessingInstanceType' "ml.c5.2xlarge"

pattern ProcessingInstanceType_Ml_c5_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c5_4xlarge = ProcessingInstanceType' "ml.c5.4xlarge"

pattern ProcessingInstanceType_Ml_c5_9xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c5_9xlarge = ProcessingInstanceType' "ml.c5.9xlarge"

pattern ProcessingInstanceType_Ml_c5_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_c5_xlarge = ProcessingInstanceType' "ml.c5.xlarge"

pattern ProcessingInstanceType_Ml_g4dn_12xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_g4dn_12xlarge = ProcessingInstanceType' "ml.g4dn.12xlarge"

pattern ProcessingInstanceType_Ml_g4dn_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_g4dn_16xlarge = ProcessingInstanceType' "ml.g4dn.16xlarge"

pattern ProcessingInstanceType_Ml_g4dn_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_g4dn_2xlarge = ProcessingInstanceType' "ml.g4dn.2xlarge"

pattern ProcessingInstanceType_Ml_g4dn_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_g4dn_4xlarge = ProcessingInstanceType' "ml.g4dn.4xlarge"

pattern ProcessingInstanceType_Ml_g4dn_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_g4dn_8xlarge = ProcessingInstanceType' "ml.g4dn.8xlarge"

pattern ProcessingInstanceType_Ml_g4dn_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_g4dn_xlarge = ProcessingInstanceType' "ml.g4dn.xlarge"

pattern ProcessingInstanceType_Ml_m4_10xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m4_10xlarge = ProcessingInstanceType' "ml.m4.10xlarge"

pattern ProcessingInstanceType_Ml_m4_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m4_16xlarge = ProcessingInstanceType' "ml.m4.16xlarge"

pattern ProcessingInstanceType_Ml_m4_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m4_2xlarge = ProcessingInstanceType' "ml.m4.2xlarge"

pattern ProcessingInstanceType_Ml_m4_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m4_4xlarge = ProcessingInstanceType' "ml.m4.4xlarge"

pattern ProcessingInstanceType_Ml_m4_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m4_xlarge = ProcessingInstanceType' "ml.m4.xlarge"

pattern ProcessingInstanceType_Ml_m5_12xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m5_12xlarge = ProcessingInstanceType' "ml.m5.12xlarge"

pattern ProcessingInstanceType_Ml_m5_24xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m5_24xlarge = ProcessingInstanceType' "ml.m5.24xlarge"

pattern ProcessingInstanceType_Ml_m5_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m5_2xlarge = ProcessingInstanceType' "ml.m5.2xlarge"

pattern ProcessingInstanceType_Ml_m5_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m5_4xlarge = ProcessingInstanceType' "ml.m5.4xlarge"

pattern ProcessingInstanceType_Ml_m5_large :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m5_large = ProcessingInstanceType' "ml.m5.large"

pattern ProcessingInstanceType_Ml_m5_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_m5_xlarge = ProcessingInstanceType' "ml.m5.xlarge"

pattern ProcessingInstanceType_Ml_p2_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_p2_16xlarge = ProcessingInstanceType' "ml.p2.16xlarge"

pattern ProcessingInstanceType_Ml_p2_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_p2_8xlarge = ProcessingInstanceType' "ml.p2.8xlarge"

pattern ProcessingInstanceType_Ml_p2_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_p2_xlarge = ProcessingInstanceType' "ml.p2.xlarge"

pattern ProcessingInstanceType_Ml_p3_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_p3_16xlarge = ProcessingInstanceType' "ml.p3.16xlarge"

pattern ProcessingInstanceType_Ml_p3_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_p3_2xlarge = ProcessingInstanceType' "ml.p3.2xlarge"

pattern ProcessingInstanceType_Ml_p3_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_p3_8xlarge = ProcessingInstanceType' "ml.p3.8xlarge"

pattern ProcessingInstanceType_Ml_r5_12xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_12xlarge = ProcessingInstanceType' "ml.r5.12xlarge"

pattern ProcessingInstanceType_Ml_r5_16xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_16xlarge = ProcessingInstanceType' "ml.r5.16xlarge"

pattern ProcessingInstanceType_Ml_r5_24xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_24xlarge = ProcessingInstanceType' "ml.r5.24xlarge"

pattern ProcessingInstanceType_Ml_r5_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_2xlarge = ProcessingInstanceType' "ml.r5.2xlarge"

pattern ProcessingInstanceType_Ml_r5_4xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_4xlarge = ProcessingInstanceType' "ml.r5.4xlarge"

pattern ProcessingInstanceType_Ml_r5_8xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_8xlarge = ProcessingInstanceType' "ml.r5.8xlarge"

pattern ProcessingInstanceType_Ml_r5_large :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_large = ProcessingInstanceType' "ml.r5.large"

pattern ProcessingInstanceType_Ml_r5_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_r5_xlarge = ProcessingInstanceType' "ml.r5.xlarge"

pattern ProcessingInstanceType_Ml_t3_2xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_t3_2xlarge = ProcessingInstanceType' "ml.t3.2xlarge"

pattern ProcessingInstanceType_Ml_t3_large :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_t3_large = ProcessingInstanceType' "ml.t3.large"

pattern ProcessingInstanceType_Ml_t3_medium :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_t3_medium = ProcessingInstanceType' "ml.t3.medium"

pattern ProcessingInstanceType_Ml_t3_xlarge :: ProcessingInstanceType
pattern ProcessingInstanceType_Ml_t3_xlarge = ProcessingInstanceType' "ml.t3.xlarge"

{-# COMPLETE
  ProcessingInstanceType_Ml_c4_2xlarge,
  ProcessingInstanceType_Ml_c4_4xlarge,
  ProcessingInstanceType_Ml_c4_8xlarge,
  ProcessingInstanceType_Ml_c4_xlarge,
  ProcessingInstanceType_Ml_c5_18xlarge,
  ProcessingInstanceType_Ml_c5_2xlarge,
  ProcessingInstanceType_Ml_c5_4xlarge,
  ProcessingInstanceType_Ml_c5_9xlarge,
  ProcessingInstanceType_Ml_c5_xlarge,
  ProcessingInstanceType_Ml_g4dn_12xlarge,
  ProcessingInstanceType_Ml_g4dn_16xlarge,
  ProcessingInstanceType_Ml_g4dn_2xlarge,
  ProcessingInstanceType_Ml_g4dn_4xlarge,
  ProcessingInstanceType_Ml_g4dn_8xlarge,
  ProcessingInstanceType_Ml_g4dn_xlarge,
  ProcessingInstanceType_Ml_m4_10xlarge,
  ProcessingInstanceType_Ml_m4_16xlarge,
  ProcessingInstanceType_Ml_m4_2xlarge,
  ProcessingInstanceType_Ml_m4_4xlarge,
  ProcessingInstanceType_Ml_m4_xlarge,
  ProcessingInstanceType_Ml_m5_12xlarge,
  ProcessingInstanceType_Ml_m5_24xlarge,
  ProcessingInstanceType_Ml_m5_2xlarge,
  ProcessingInstanceType_Ml_m5_4xlarge,
  ProcessingInstanceType_Ml_m5_large,
  ProcessingInstanceType_Ml_m5_xlarge,
  ProcessingInstanceType_Ml_p2_16xlarge,
  ProcessingInstanceType_Ml_p2_8xlarge,
  ProcessingInstanceType_Ml_p2_xlarge,
  ProcessingInstanceType_Ml_p3_16xlarge,
  ProcessingInstanceType_Ml_p3_2xlarge,
  ProcessingInstanceType_Ml_p3_8xlarge,
  ProcessingInstanceType_Ml_r5_12xlarge,
  ProcessingInstanceType_Ml_r5_16xlarge,
  ProcessingInstanceType_Ml_r5_24xlarge,
  ProcessingInstanceType_Ml_r5_2xlarge,
  ProcessingInstanceType_Ml_r5_4xlarge,
  ProcessingInstanceType_Ml_r5_8xlarge,
  ProcessingInstanceType_Ml_r5_large,
  ProcessingInstanceType_Ml_r5_xlarge,
  ProcessingInstanceType_Ml_t3_2xlarge,
  ProcessingInstanceType_Ml_t3_large,
  ProcessingInstanceType_Ml_t3_medium,
  ProcessingInstanceType_Ml_t3_xlarge,
  ProcessingInstanceType'
  #-}
