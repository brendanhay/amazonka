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
-- Module      : Network.AWS.SageMaker.Types.AppInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppInstanceType
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
        AppInstanceType_Ml_m5_12xlarge,
        AppInstanceType_Ml_m5_16xlarge,
        AppInstanceType_Ml_m5_24xlarge,
        AppInstanceType_Ml_m5_2xlarge,
        AppInstanceType_Ml_m5_4xlarge,
        AppInstanceType_Ml_m5_8xlarge,
        AppInstanceType_Ml_m5_large,
        AppInstanceType_Ml_m5_xlarge,
        AppInstanceType_Ml_p3_16xlarge,
        AppInstanceType_Ml_p3_2xlarge,
        AppInstanceType_Ml_p3_8xlarge,
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

import qualified Network.AWS.Core as Core

newtype AppInstanceType = AppInstanceType'
  { fromAppInstanceType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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

pattern AppInstanceType_Ml_p3_16xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3_16xlarge = AppInstanceType' "ml.p3.16xlarge"

pattern AppInstanceType_Ml_p3_2xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3_2xlarge = AppInstanceType' "ml.p3.2xlarge"

pattern AppInstanceType_Ml_p3_8xlarge :: AppInstanceType
pattern AppInstanceType_Ml_p3_8xlarge = AppInstanceType' "ml.p3.8xlarge"

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
  AppInstanceType_Ml_m5_12xlarge,
  AppInstanceType_Ml_m5_16xlarge,
  AppInstanceType_Ml_m5_24xlarge,
  AppInstanceType_Ml_m5_2xlarge,
  AppInstanceType_Ml_m5_4xlarge,
  AppInstanceType_Ml_m5_8xlarge,
  AppInstanceType_Ml_m5_large,
  AppInstanceType_Ml_m5_xlarge,
  AppInstanceType_Ml_p3_16xlarge,
  AppInstanceType_Ml_p3_2xlarge,
  AppInstanceType_Ml_p3_8xlarge,
  AppInstanceType_Ml_t3_2xlarge,
  AppInstanceType_Ml_t3_large,
  AppInstanceType_Ml_t3_medium,
  AppInstanceType_Ml_t3_micro,
  AppInstanceType_Ml_t3_small,
  AppInstanceType_Ml_t3_xlarge,
  AppInstanceType_System,
  AppInstanceType'
  #-}
