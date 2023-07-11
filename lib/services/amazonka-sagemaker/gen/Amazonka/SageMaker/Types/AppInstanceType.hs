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
-- Module      : Amazonka.SageMaker.Types.AppInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AppInstanceType
  ( AppInstanceType
      ( ..,
        AppInstanceType_Ml_c5_12xlarge,
        AppInstanceType_Ml_c5_18xlarge,
        AppInstanceType_Ml_c5_24xlarge,
        AppInstanceType_Ml_c5_2xlarge,
        AppInstanceType_Ml_c5_4xlarge,
        AppInstanceType_Ml_c5_9xlarge,
        AppInstanceType_Ml_c5_large,
        AppInstanceType_Ml_c5_xlarge,
        AppInstanceType_Ml_g4dn_12xlarge,
        AppInstanceType_Ml_g4dn_16xlarge,
        AppInstanceType_Ml_g4dn_2xlarge,
        AppInstanceType_Ml_g4dn_4xlarge,
        AppInstanceType_Ml_g4dn_8xlarge,
        AppInstanceType_Ml_g4dn_xlarge,
        AppInstanceType_Ml_g5_12xlarge,
        AppInstanceType_Ml_g5_16xlarge,
        AppInstanceType_Ml_g5_24xlarge,
        AppInstanceType_Ml_g5_2xlarge,
        AppInstanceType_Ml_g5_48xlarge,
        AppInstanceType_Ml_g5_4xlarge,
        AppInstanceType_Ml_g5_8xlarge,
        AppInstanceType_Ml_g5_xlarge,
        AppInstanceType_Ml_m5_12xlarge,
        AppInstanceType_Ml_m5_16xlarge,
        AppInstanceType_Ml_m5_24xlarge,
        AppInstanceType_Ml_m5_2xlarge,
        AppInstanceType_Ml_m5_4xlarge,
        AppInstanceType_Ml_m5_8xlarge,
        AppInstanceType_Ml_m5_large,
        AppInstanceType_Ml_m5_xlarge,
        AppInstanceType_Ml_m5d_12xlarge,
        AppInstanceType_Ml_m5d_16xlarge,
        AppInstanceType_Ml_m5d_24xlarge,
        AppInstanceType_Ml_m5d_2xlarge,
        AppInstanceType_Ml_m5d_4xlarge,
        AppInstanceType_Ml_m5d_8xlarge,
        AppInstanceType_Ml_m5d_large,
        AppInstanceType_Ml_m5d_xlarge,
        AppInstanceType_Ml_p3_16xlarge,
        AppInstanceType_Ml_p3_2xlarge,
        AppInstanceType_Ml_p3_8xlarge,
        AppInstanceType_Ml_p3dn_24xlarge,
        AppInstanceType_Ml_r5_12xlarge,
        AppInstanceType_Ml_r5_16xlarge,
        AppInstanceType_Ml_r5_24xlarge,
        AppInstanceType_Ml_r5_2xlarge,
        AppInstanceType_Ml_r5_4xlarge,
        AppInstanceType_Ml_r5_8xlarge,
        AppInstanceType_Ml_r5_large,
        AppInstanceType_Ml_r5_xlarge,
        AppInstanceType_Ml_t3_2xlarge,
        AppInstanceType_Ml_t3_large,
        AppInstanceType_Ml_t3_medium,
        AppInstanceType_Ml_t3_micro,
        AppInstanceType_Ml_t3_small,
        AppInstanceType_Ml_t3_xlarge,
        AppInstanceType_System
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppInstanceType = AppInstanceType'
  { fromAppInstanceType ::
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

pattern AppInstanceType_Ml_c5_12xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_12xlarge = AppInstanceType' "ml.c5.12xlarge"

pattern AppInstanceType_Ml_c5_18xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_18xlarge = AppInstanceType' "ml.c5.18xlarge"

pattern AppInstanceType_Ml_c5_24xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_24xlarge = AppInstanceType' "ml.c5.24xlarge"

pattern AppInstanceType_Ml_c5_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_2xlarge = AppInstanceType' "ml.c5.2xlarge"

pattern AppInstanceType_Ml_c5_4xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_4xlarge = AppInstanceType' "ml.c5.4xlarge"

pattern AppInstanceType_Ml_c5_9xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_9xlarge = AppInstanceType' "ml.c5.9xlarge"

pattern AppInstanceType_Ml_c5_large :: AppInstanceType
pattern AppInstanceType_Ml_c5_large = AppInstanceType' "ml.c5.large"

pattern AppInstanceType_Ml_c5_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_c5_xlarge = AppInstanceType' "ml.c5.xlarge"

pattern AppInstanceType_Ml_g4dn_12xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g4dn_12xlarge = AppInstanceType' "ml.g4dn.12xlarge"

pattern AppInstanceType_Ml_g4dn_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g4dn_16xlarge = AppInstanceType' "ml.g4dn.16xlarge"

pattern AppInstanceType_Ml_g4dn_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g4dn_2xlarge = AppInstanceType' "ml.g4dn.2xlarge"

pattern AppInstanceType_Ml_g4dn_4xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g4dn_4xlarge = AppInstanceType' "ml.g4dn.4xlarge"

pattern AppInstanceType_Ml_g4dn_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g4dn_8xlarge = AppInstanceType' "ml.g4dn.8xlarge"

pattern AppInstanceType_Ml_g4dn_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g4dn_xlarge = AppInstanceType' "ml.g4dn.xlarge"

pattern AppInstanceType_Ml_g5_12xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_12xlarge = AppInstanceType' "ml.g5.12xlarge"

pattern AppInstanceType_Ml_g5_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_16xlarge = AppInstanceType' "ml.g5.16xlarge"

pattern AppInstanceType_Ml_g5_24xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_24xlarge = AppInstanceType' "ml.g5.24xlarge"

pattern AppInstanceType_Ml_g5_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_2xlarge = AppInstanceType' "ml.g5.2xlarge"

pattern AppInstanceType_Ml_g5_48xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_48xlarge = AppInstanceType' "ml.g5.48xlarge"

pattern AppInstanceType_Ml_g5_4xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_4xlarge = AppInstanceType' "ml.g5.4xlarge"

pattern AppInstanceType_Ml_g5_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_8xlarge = AppInstanceType' "ml.g5.8xlarge"

pattern AppInstanceType_Ml_g5_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_g5_xlarge = AppInstanceType' "ml.g5.xlarge"

pattern AppInstanceType_Ml_m5_12xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_12xlarge = AppInstanceType' "ml.m5.12xlarge"

pattern AppInstanceType_Ml_m5_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_16xlarge = AppInstanceType' "ml.m5.16xlarge"

pattern AppInstanceType_Ml_m5_24xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_24xlarge = AppInstanceType' "ml.m5.24xlarge"

pattern AppInstanceType_Ml_m5_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_2xlarge = AppInstanceType' "ml.m5.2xlarge"

pattern AppInstanceType_Ml_m5_4xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_4xlarge = AppInstanceType' "ml.m5.4xlarge"

pattern AppInstanceType_Ml_m5_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_8xlarge = AppInstanceType' "ml.m5.8xlarge"

pattern AppInstanceType_Ml_m5_large :: AppInstanceType
pattern AppInstanceType_Ml_m5_large = AppInstanceType' "ml.m5.large"

pattern AppInstanceType_Ml_m5_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5_xlarge = AppInstanceType' "ml.m5.xlarge"

pattern AppInstanceType_Ml_m5d_12xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_12xlarge = AppInstanceType' "ml.m5d.12xlarge"

pattern AppInstanceType_Ml_m5d_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_16xlarge = AppInstanceType' "ml.m5d.16xlarge"

pattern AppInstanceType_Ml_m5d_24xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_24xlarge = AppInstanceType' "ml.m5d.24xlarge"

pattern AppInstanceType_Ml_m5d_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_2xlarge = AppInstanceType' "ml.m5d.2xlarge"

pattern AppInstanceType_Ml_m5d_4xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_4xlarge = AppInstanceType' "ml.m5d.4xlarge"

pattern AppInstanceType_Ml_m5d_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_8xlarge = AppInstanceType' "ml.m5d.8xlarge"

pattern AppInstanceType_Ml_m5d_large :: AppInstanceType
pattern AppInstanceType_Ml_m5d_large = AppInstanceType' "ml.m5d.large"

pattern AppInstanceType_Ml_m5d_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_m5d_xlarge = AppInstanceType' "ml.m5d.xlarge"

pattern AppInstanceType_Ml_p3_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3_16xlarge = AppInstanceType' "ml.p3.16xlarge"

pattern AppInstanceType_Ml_p3_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3_2xlarge = AppInstanceType' "ml.p3.2xlarge"

pattern AppInstanceType_Ml_p3_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3_8xlarge = AppInstanceType' "ml.p3.8xlarge"

pattern AppInstanceType_Ml_p3dn_24xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3dn_24xlarge = AppInstanceType' "ml.p3dn.24xlarge"

pattern AppInstanceType_Ml_r5_12xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_12xlarge = AppInstanceType' "ml.r5.12xlarge"

pattern AppInstanceType_Ml_r5_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_16xlarge = AppInstanceType' "ml.r5.16xlarge"

pattern AppInstanceType_Ml_r5_24xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_24xlarge = AppInstanceType' "ml.r5.24xlarge"

pattern AppInstanceType_Ml_r5_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_2xlarge = AppInstanceType' "ml.r5.2xlarge"

pattern AppInstanceType_Ml_r5_4xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_4xlarge = AppInstanceType' "ml.r5.4xlarge"

pattern AppInstanceType_Ml_r5_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_8xlarge = AppInstanceType' "ml.r5.8xlarge"

pattern AppInstanceType_Ml_r5_large :: AppInstanceType
pattern AppInstanceType_Ml_r5_large = AppInstanceType' "ml.r5.large"

pattern AppInstanceType_Ml_r5_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_r5_xlarge = AppInstanceType' "ml.r5.xlarge"

pattern AppInstanceType_Ml_t3_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_t3_2xlarge = AppInstanceType' "ml.t3.2xlarge"

pattern AppInstanceType_Ml_t3_large :: AppInstanceType
pattern AppInstanceType_Ml_t3_large = AppInstanceType' "ml.t3.large"

pattern AppInstanceType_Ml_t3_medium :: AppInstanceType
pattern AppInstanceType_Ml_t3_medium = AppInstanceType' "ml.t3.medium"

pattern AppInstanceType_Ml_t3_micro :: AppInstanceType
pattern AppInstanceType_Ml_t3_micro = AppInstanceType' "ml.t3.micro"

pattern AppInstanceType_Ml_t3_small :: AppInstanceType
pattern AppInstanceType_Ml_t3_small = AppInstanceType' "ml.t3.small"

pattern AppInstanceType_Ml_t3_xlarge :: AppInstanceType
pattern AppInstanceType_Ml_t3_xlarge = AppInstanceType' "ml.t3.xlarge"

pattern AppInstanceType_System :: AppInstanceType
pattern AppInstanceType_System = AppInstanceType' "system"

{-# COMPLETE
  AppInstanceType_Ml_c5_12xlarge,
  AppInstanceType_Ml_c5_18xlarge,
  AppInstanceType_Ml_c5_24xlarge,
  AppInstanceType_Ml_c5_2xlarge,
  AppInstanceType_Ml_c5_4xlarge,
  AppInstanceType_Ml_c5_9xlarge,
  AppInstanceType_Ml_c5_large,
  AppInstanceType_Ml_c5_xlarge,
  AppInstanceType_Ml_g4dn_12xlarge,
  AppInstanceType_Ml_g4dn_16xlarge,
  AppInstanceType_Ml_g4dn_2xlarge,
  AppInstanceType_Ml_g4dn_4xlarge,
  AppInstanceType_Ml_g4dn_8xlarge,
  AppInstanceType_Ml_g4dn_xlarge,
  AppInstanceType_Ml_g5_12xlarge,
  AppInstanceType_Ml_g5_16xlarge,
  AppInstanceType_Ml_g5_24xlarge,
  AppInstanceType_Ml_g5_2xlarge,
  AppInstanceType_Ml_g5_48xlarge,
  AppInstanceType_Ml_g5_4xlarge,
  AppInstanceType_Ml_g5_8xlarge,
  AppInstanceType_Ml_g5_xlarge,
  AppInstanceType_Ml_m5_12xlarge,
  AppInstanceType_Ml_m5_16xlarge,
  AppInstanceType_Ml_m5_24xlarge,
  AppInstanceType_Ml_m5_2xlarge,
  AppInstanceType_Ml_m5_4xlarge,
  AppInstanceType_Ml_m5_8xlarge,
  AppInstanceType_Ml_m5_large,
  AppInstanceType_Ml_m5_xlarge,
  AppInstanceType_Ml_m5d_12xlarge,
  AppInstanceType_Ml_m5d_16xlarge,
  AppInstanceType_Ml_m5d_24xlarge,
  AppInstanceType_Ml_m5d_2xlarge,
  AppInstanceType_Ml_m5d_4xlarge,
  AppInstanceType_Ml_m5d_8xlarge,
  AppInstanceType_Ml_m5d_large,
  AppInstanceType_Ml_m5d_xlarge,
  AppInstanceType_Ml_p3_16xlarge,
  AppInstanceType_Ml_p3_2xlarge,
  AppInstanceType_Ml_p3_8xlarge,
  AppInstanceType_Ml_p3dn_24xlarge,
  AppInstanceType_Ml_r5_12xlarge,
  AppInstanceType_Ml_r5_16xlarge,
  AppInstanceType_Ml_r5_24xlarge,
  AppInstanceType_Ml_r5_2xlarge,
  AppInstanceType_Ml_r5_4xlarge,
  AppInstanceType_Ml_r5_8xlarge,
  AppInstanceType_Ml_r5_large,
  AppInstanceType_Ml_r5_xlarge,
  AppInstanceType_Ml_t3_2xlarge,
  AppInstanceType_Ml_t3_large,
  AppInstanceType_Ml_t3_medium,
  AppInstanceType_Ml_t3_micro,
  AppInstanceType_Ml_t3_small,
  AppInstanceType_Ml_t3_xlarge,
  AppInstanceType_System,
  AppInstanceType'
  #-}
