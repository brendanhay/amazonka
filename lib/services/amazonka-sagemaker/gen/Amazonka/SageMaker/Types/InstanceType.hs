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
-- Module      : Amazonka.SageMaker.Types.InstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InstanceType
  ( InstanceType
      ( ..,
        InstanceType_Ml_c4_2xlarge,
        InstanceType_Ml_c4_4xlarge,
        InstanceType_Ml_c4_8xlarge,
        InstanceType_Ml_c4_xlarge,
        InstanceType_Ml_c5_18xlarge,
        InstanceType_Ml_c5_2xlarge,
        InstanceType_Ml_c5_4xlarge,
        InstanceType_Ml_c5_9xlarge,
        InstanceType_Ml_c5_xlarge,
        InstanceType_Ml_c5d_18xlarge,
        InstanceType_Ml_c5d_2xlarge,
        InstanceType_Ml_c5d_4xlarge,
        InstanceType_Ml_c5d_9xlarge,
        InstanceType_Ml_c5d_xlarge,
        InstanceType_Ml_g4dn_12xlarge,
        InstanceType_Ml_g4dn_16xlarge,
        InstanceType_Ml_g4dn_2xlarge,
        InstanceType_Ml_g4dn_4xlarge,
        InstanceType_Ml_g4dn_8xlarge,
        InstanceType_Ml_g4dn_xlarge,
        InstanceType_Ml_g5_12xlarge,
        InstanceType_Ml_g5_16xlarge,
        InstanceType_Ml_g5_24xlarge,
        InstanceType_Ml_g5_2xlarge,
        InstanceType_Ml_g5_48xlarge,
        InstanceType_Ml_g5_4xlarge,
        InstanceType_Ml_g5_8xlarge,
        InstanceType_Ml_g5_xlarge,
        InstanceType_Ml_m4_10xlarge,
        InstanceType_Ml_m4_16xlarge,
        InstanceType_Ml_m4_2xlarge,
        InstanceType_Ml_m4_4xlarge,
        InstanceType_Ml_m4_xlarge,
        InstanceType_Ml_m5_12xlarge,
        InstanceType_Ml_m5_24xlarge,
        InstanceType_Ml_m5_2xlarge,
        InstanceType_Ml_m5_4xlarge,
        InstanceType_Ml_m5_xlarge,
        InstanceType_Ml_m5d_12xlarge,
        InstanceType_Ml_m5d_16xlarge,
        InstanceType_Ml_m5d_24xlarge,
        InstanceType_Ml_m5d_2xlarge,
        InstanceType_Ml_m5d_4xlarge,
        InstanceType_Ml_m5d_8xlarge,
        InstanceType_Ml_m5d_large,
        InstanceType_Ml_m5d_xlarge,
        InstanceType_Ml_p2_16xlarge,
        InstanceType_Ml_p2_8xlarge,
        InstanceType_Ml_p2_xlarge,
        InstanceType_Ml_p3_16xlarge,
        InstanceType_Ml_p3_2xlarge,
        InstanceType_Ml_p3_8xlarge,
        InstanceType_Ml_p3dn_24xlarge,
        InstanceType_Ml_r5_12xlarge,
        InstanceType_Ml_r5_16xlarge,
        InstanceType_Ml_r5_24xlarge,
        InstanceType_Ml_r5_2xlarge,
        InstanceType_Ml_r5_4xlarge,
        InstanceType_Ml_r5_8xlarge,
        InstanceType_Ml_r5_large,
        InstanceType_Ml_r5_xlarge,
        InstanceType_Ml_t2_2xlarge,
        InstanceType_Ml_t2_large,
        InstanceType_Ml_t2_medium,
        InstanceType_Ml_t2_xlarge,
        InstanceType_Ml_t3_2xlarge,
        InstanceType_Ml_t3_large,
        InstanceType_Ml_t3_medium,
        InstanceType_Ml_t3_xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceType = InstanceType'
  { fromInstanceType ::
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

pattern InstanceType_Ml_c4_2xlarge :: InstanceType
pattern InstanceType_Ml_c4_2xlarge = InstanceType' "ml.c4.2xlarge"

pattern InstanceType_Ml_c4_4xlarge :: InstanceType
pattern InstanceType_Ml_c4_4xlarge = InstanceType' "ml.c4.4xlarge"

pattern InstanceType_Ml_c4_8xlarge :: InstanceType
pattern InstanceType_Ml_c4_8xlarge = InstanceType' "ml.c4.8xlarge"

pattern InstanceType_Ml_c4_xlarge :: InstanceType
pattern InstanceType_Ml_c4_xlarge = InstanceType' "ml.c4.xlarge"

pattern InstanceType_Ml_c5_18xlarge :: InstanceType
pattern InstanceType_Ml_c5_18xlarge = InstanceType' "ml.c5.18xlarge"

pattern InstanceType_Ml_c5_2xlarge :: InstanceType
pattern InstanceType_Ml_c5_2xlarge = InstanceType' "ml.c5.2xlarge"

pattern InstanceType_Ml_c5_4xlarge :: InstanceType
pattern InstanceType_Ml_c5_4xlarge = InstanceType' "ml.c5.4xlarge"

pattern InstanceType_Ml_c5_9xlarge :: InstanceType
pattern InstanceType_Ml_c5_9xlarge = InstanceType' "ml.c5.9xlarge"

pattern InstanceType_Ml_c5_xlarge :: InstanceType
pattern InstanceType_Ml_c5_xlarge = InstanceType' "ml.c5.xlarge"

pattern InstanceType_Ml_c5d_18xlarge :: InstanceType
pattern InstanceType_Ml_c5d_18xlarge = InstanceType' "ml.c5d.18xlarge"

pattern InstanceType_Ml_c5d_2xlarge :: InstanceType
pattern InstanceType_Ml_c5d_2xlarge = InstanceType' "ml.c5d.2xlarge"

pattern InstanceType_Ml_c5d_4xlarge :: InstanceType
pattern InstanceType_Ml_c5d_4xlarge = InstanceType' "ml.c5d.4xlarge"

pattern InstanceType_Ml_c5d_9xlarge :: InstanceType
pattern InstanceType_Ml_c5d_9xlarge = InstanceType' "ml.c5d.9xlarge"

pattern InstanceType_Ml_c5d_xlarge :: InstanceType
pattern InstanceType_Ml_c5d_xlarge = InstanceType' "ml.c5d.xlarge"

pattern InstanceType_Ml_g4dn_12xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_12xlarge = InstanceType' "ml.g4dn.12xlarge"

pattern InstanceType_Ml_g4dn_16xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_16xlarge = InstanceType' "ml.g4dn.16xlarge"

pattern InstanceType_Ml_g4dn_2xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_2xlarge = InstanceType' "ml.g4dn.2xlarge"

pattern InstanceType_Ml_g4dn_4xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_4xlarge = InstanceType' "ml.g4dn.4xlarge"

pattern InstanceType_Ml_g4dn_8xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_8xlarge = InstanceType' "ml.g4dn.8xlarge"

pattern InstanceType_Ml_g4dn_xlarge :: InstanceType
pattern InstanceType_Ml_g4dn_xlarge = InstanceType' "ml.g4dn.xlarge"

pattern InstanceType_Ml_g5_12xlarge :: InstanceType
pattern InstanceType_Ml_g5_12xlarge = InstanceType' "ml.g5.12xlarge"

pattern InstanceType_Ml_g5_16xlarge :: InstanceType
pattern InstanceType_Ml_g5_16xlarge = InstanceType' "ml.g5.16xlarge"

pattern InstanceType_Ml_g5_24xlarge :: InstanceType
pattern InstanceType_Ml_g5_24xlarge = InstanceType' "ml.g5.24xlarge"

pattern InstanceType_Ml_g5_2xlarge :: InstanceType
pattern InstanceType_Ml_g5_2xlarge = InstanceType' "ml.g5.2xlarge"

pattern InstanceType_Ml_g5_48xlarge :: InstanceType
pattern InstanceType_Ml_g5_48xlarge = InstanceType' "ml.g5.48xlarge"

pattern InstanceType_Ml_g5_4xlarge :: InstanceType
pattern InstanceType_Ml_g5_4xlarge = InstanceType' "ml.g5.4xlarge"

pattern InstanceType_Ml_g5_8xlarge :: InstanceType
pattern InstanceType_Ml_g5_8xlarge = InstanceType' "ml.g5.8xlarge"

pattern InstanceType_Ml_g5_xlarge :: InstanceType
pattern InstanceType_Ml_g5_xlarge = InstanceType' "ml.g5.xlarge"

pattern InstanceType_Ml_m4_10xlarge :: InstanceType
pattern InstanceType_Ml_m4_10xlarge = InstanceType' "ml.m4.10xlarge"

pattern InstanceType_Ml_m4_16xlarge :: InstanceType
pattern InstanceType_Ml_m4_16xlarge = InstanceType' "ml.m4.16xlarge"

pattern InstanceType_Ml_m4_2xlarge :: InstanceType
pattern InstanceType_Ml_m4_2xlarge = InstanceType' "ml.m4.2xlarge"

pattern InstanceType_Ml_m4_4xlarge :: InstanceType
pattern InstanceType_Ml_m4_4xlarge = InstanceType' "ml.m4.4xlarge"

pattern InstanceType_Ml_m4_xlarge :: InstanceType
pattern InstanceType_Ml_m4_xlarge = InstanceType' "ml.m4.xlarge"

pattern InstanceType_Ml_m5_12xlarge :: InstanceType
pattern InstanceType_Ml_m5_12xlarge = InstanceType' "ml.m5.12xlarge"

pattern InstanceType_Ml_m5_24xlarge :: InstanceType
pattern InstanceType_Ml_m5_24xlarge = InstanceType' "ml.m5.24xlarge"

pattern InstanceType_Ml_m5_2xlarge :: InstanceType
pattern InstanceType_Ml_m5_2xlarge = InstanceType' "ml.m5.2xlarge"

pattern InstanceType_Ml_m5_4xlarge :: InstanceType
pattern InstanceType_Ml_m5_4xlarge = InstanceType' "ml.m5.4xlarge"

pattern InstanceType_Ml_m5_xlarge :: InstanceType
pattern InstanceType_Ml_m5_xlarge = InstanceType' "ml.m5.xlarge"

pattern InstanceType_Ml_m5d_12xlarge :: InstanceType
pattern InstanceType_Ml_m5d_12xlarge = InstanceType' "ml.m5d.12xlarge"

pattern InstanceType_Ml_m5d_16xlarge :: InstanceType
pattern InstanceType_Ml_m5d_16xlarge = InstanceType' "ml.m5d.16xlarge"

pattern InstanceType_Ml_m5d_24xlarge :: InstanceType
pattern InstanceType_Ml_m5d_24xlarge = InstanceType' "ml.m5d.24xlarge"

pattern InstanceType_Ml_m5d_2xlarge :: InstanceType
pattern InstanceType_Ml_m5d_2xlarge = InstanceType' "ml.m5d.2xlarge"

pattern InstanceType_Ml_m5d_4xlarge :: InstanceType
pattern InstanceType_Ml_m5d_4xlarge = InstanceType' "ml.m5d.4xlarge"

pattern InstanceType_Ml_m5d_8xlarge :: InstanceType
pattern InstanceType_Ml_m5d_8xlarge = InstanceType' "ml.m5d.8xlarge"

pattern InstanceType_Ml_m5d_large :: InstanceType
pattern InstanceType_Ml_m5d_large = InstanceType' "ml.m5d.large"

pattern InstanceType_Ml_m5d_xlarge :: InstanceType
pattern InstanceType_Ml_m5d_xlarge = InstanceType' "ml.m5d.xlarge"

pattern InstanceType_Ml_p2_16xlarge :: InstanceType
pattern InstanceType_Ml_p2_16xlarge = InstanceType' "ml.p2.16xlarge"

pattern InstanceType_Ml_p2_8xlarge :: InstanceType
pattern InstanceType_Ml_p2_8xlarge = InstanceType' "ml.p2.8xlarge"

pattern InstanceType_Ml_p2_xlarge :: InstanceType
pattern InstanceType_Ml_p2_xlarge = InstanceType' "ml.p2.xlarge"

pattern InstanceType_Ml_p3_16xlarge :: InstanceType
pattern InstanceType_Ml_p3_16xlarge = InstanceType' "ml.p3.16xlarge"

pattern InstanceType_Ml_p3_2xlarge :: InstanceType
pattern InstanceType_Ml_p3_2xlarge = InstanceType' "ml.p3.2xlarge"

pattern InstanceType_Ml_p3_8xlarge :: InstanceType
pattern InstanceType_Ml_p3_8xlarge = InstanceType' "ml.p3.8xlarge"

pattern InstanceType_Ml_p3dn_24xlarge :: InstanceType
pattern InstanceType_Ml_p3dn_24xlarge = InstanceType' "ml.p3dn.24xlarge"

pattern InstanceType_Ml_r5_12xlarge :: InstanceType
pattern InstanceType_Ml_r5_12xlarge = InstanceType' "ml.r5.12xlarge"

pattern InstanceType_Ml_r5_16xlarge :: InstanceType
pattern InstanceType_Ml_r5_16xlarge = InstanceType' "ml.r5.16xlarge"

pattern InstanceType_Ml_r5_24xlarge :: InstanceType
pattern InstanceType_Ml_r5_24xlarge = InstanceType' "ml.r5.24xlarge"

pattern InstanceType_Ml_r5_2xlarge :: InstanceType
pattern InstanceType_Ml_r5_2xlarge = InstanceType' "ml.r5.2xlarge"

pattern InstanceType_Ml_r5_4xlarge :: InstanceType
pattern InstanceType_Ml_r5_4xlarge = InstanceType' "ml.r5.4xlarge"

pattern InstanceType_Ml_r5_8xlarge :: InstanceType
pattern InstanceType_Ml_r5_8xlarge = InstanceType' "ml.r5.8xlarge"

pattern InstanceType_Ml_r5_large :: InstanceType
pattern InstanceType_Ml_r5_large = InstanceType' "ml.r5.large"

pattern InstanceType_Ml_r5_xlarge :: InstanceType
pattern InstanceType_Ml_r5_xlarge = InstanceType' "ml.r5.xlarge"

pattern InstanceType_Ml_t2_2xlarge :: InstanceType
pattern InstanceType_Ml_t2_2xlarge = InstanceType' "ml.t2.2xlarge"

pattern InstanceType_Ml_t2_large :: InstanceType
pattern InstanceType_Ml_t2_large = InstanceType' "ml.t2.large"

pattern InstanceType_Ml_t2_medium :: InstanceType
pattern InstanceType_Ml_t2_medium = InstanceType' "ml.t2.medium"

pattern InstanceType_Ml_t2_xlarge :: InstanceType
pattern InstanceType_Ml_t2_xlarge = InstanceType' "ml.t2.xlarge"

pattern InstanceType_Ml_t3_2xlarge :: InstanceType
pattern InstanceType_Ml_t3_2xlarge = InstanceType' "ml.t3.2xlarge"

pattern InstanceType_Ml_t3_large :: InstanceType
pattern InstanceType_Ml_t3_large = InstanceType' "ml.t3.large"

pattern InstanceType_Ml_t3_medium :: InstanceType
pattern InstanceType_Ml_t3_medium = InstanceType' "ml.t3.medium"

pattern InstanceType_Ml_t3_xlarge :: InstanceType
pattern InstanceType_Ml_t3_xlarge = InstanceType' "ml.t3.xlarge"

{-# COMPLETE
  InstanceType_Ml_c4_2xlarge,
  InstanceType_Ml_c4_4xlarge,
  InstanceType_Ml_c4_8xlarge,
  InstanceType_Ml_c4_xlarge,
  InstanceType_Ml_c5_18xlarge,
  InstanceType_Ml_c5_2xlarge,
  InstanceType_Ml_c5_4xlarge,
  InstanceType_Ml_c5_9xlarge,
  InstanceType_Ml_c5_xlarge,
  InstanceType_Ml_c5d_18xlarge,
  InstanceType_Ml_c5d_2xlarge,
  InstanceType_Ml_c5d_4xlarge,
  InstanceType_Ml_c5d_9xlarge,
  InstanceType_Ml_c5d_xlarge,
  InstanceType_Ml_g4dn_12xlarge,
  InstanceType_Ml_g4dn_16xlarge,
  InstanceType_Ml_g4dn_2xlarge,
  InstanceType_Ml_g4dn_4xlarge,
  InstanceType_Ml_g4dn_8xlarge,
  InstanceType_Ml_g4dn_xlarge,
  InstanceType_Ml_g5_12xlarge,
  InstanceType_Ml_g5_16xlarge,
  InstanceType_Ml_g5_24xlarge,
  InstanceType_Ml_g5_2xlarge,
  InstanceType_Ml_g5_48xlarge,
  InstanceType_Ml_g5_4xlarge,
  InstanceType_Ml_g5_8xlarge,
  InstanceType_Ml_g5_xlarge,
  InstanceType_Ml_m4_10xlarge,
  InstanceType_Ml_m4_16xlarge,
  InstanceType_Ml_m4_2xlarge,
  InstanceType_Ml_m4_4xlarge,
  InstanceType_Ml_m4_xlarge,
  InstanceType_Ml_m5_12xlarge,
  InstanceType_Ml_m5_24xlarge,
  InstanceType_Ml_m5_2xlarge,
  InstanceType_Ml_m5_4xlarge,
  InstanceType_Ml_m5_xlarge,
  InstanceType_Ml_m5d_12xlarge,
  InstanceType_Ml_m5d_16xlarge,
  InstanceType_Ml_m5d_24xlarge,
  InstanceType_Ml_m5d_2xlarge,
  InstanceType_Ml_m5d_4xlarge,
  InstanceType_Ml_m5d_8xlarge,
  InstanceType_Ml_m5d_large,
  InstanceType_Ml_m5d_xlarge,
  InstanceType_Ml_p2_16xlarge,
  InstanceType_Ml_p2_8xlarge,
  InstanceType_Ml_p2_xlarge,
  InstanceType_Ml_p3_16xlarge,
  InstanceType_Ml_p3_2xlarge,
  InstanceType_Ml_p3_8xlarge,
  InstanceType_Ml_p3dn_24xlarge,
  InstanceType_Ml_r5_12xlarge,
  InstanceType_Ml_r5_16xlarge,
  InstanceType_Ml_r5_24xlarge,
  InstanceType_Ml_r5_2xlarge,
  InstanceType_Ml_r5_4xlarge,
  InstanceType_Ml_r5_8xlarge,
  InstanceType_Ml_r5_large,
  InstanceType_Ml_r5_xlarge,
  InstanceType_Ml_t2_2xlarge,
  InstanceType_Ml_t2_large,
  InstanceType_Ml_t2_medium,
  InstanceType_Ml_t2_xlarge,
  InstanceType_Ml_t3_2xlarge,
  InstanceType_Ml_t3_large,
  InstanceType_Ml_t3_medium,
  InstanceType_Ml_t3_xlarge,
  InstanceType'
  #-}
