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
-- Module      : Network.AWS.SageMaker.Types.ProductionVariantInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProductionVariantInstanceType
  ( ProductionVariantInstanceType
      ( ..,
        ProductionVariantInstanceType_Ml_c4_2xlarge,
        ProductionVariantInstanceType_Ml_c4_4xlarge,
        ProductionVariantInstanceType_Ml_c4_8xlarge,
        ProductionVariantInstanceType_Ml_c4_large,
        ProductionVariantInstanceType_Ml_c4_xlarge,
        ProductionVariantInstanceType_Ml_c5_18xlarge,
        ProductionVariantInstanceType_Ml_c5_2xlarge,
        ProductionVariantInstanceType_Ml_c5_4xlarge,
        ProductionVariantInstanceType_Ml_c5_9xlarge,
        ProductionVariantInstanceType_Ml_c5_large,
        ProductionVariantInstanceType_Ml_c5_xlarge,
        ProductionVariantInstanceType_Ml_c5d_18xlarge,
        ProductionVariantInstanceType_Ml_c5d_2xlarge,
        ProductionVariantInstanceType_Ml_c5d_4xlarge,
        ProductionVariantInstanceType_Ml_c5d_9xlarge,
        ProductionVariantInstanceType_Ml_c5d_large,
        ProductionVariantInstanceType_Ml_c5d_xlarge,
        ProductionVariantInstanceType_Ml_g4dn_12xlarge,
        ProductionVariantInstanceType_Ml_g4dn_16xlarge,
        ProductionVariantInstanceType_Ml_g4dn_2xlarge,
        ProductionVariantInstanceType_Ml_g4dn_4xlarge,
        ProductionVariantInstanceType_Ml_g4dn_8xlarge,
        ProductionVariantInstanceType_Ml_g4dn_xlarge,
        ProductionVariantInstanceType_Ml_inf1_24xlarge,
        ProductionVariantInstanceType_Ml_inf1_2xlarge,
        ProductionVariantInstanceType_Ml_inf1_6xlarge,
        ProductionVariantInstanceType_Ml_inf1_xlarge,
        ProductionVariantInstanceType_Ml_m4_10xlarge,
        ProductionVariantInstanceType_Ml_m4_16xlarge,
        ProductionVariantInstanceType_Ml_m4_2xlarge,
        ProductionVariantInstanceType_Ml_m4_4xlarge,
        ProductionVariantInstanceType_Ml_m4_xlarge,
        ProductionVariantInstanceType_Ml_m5_12xlarge,
        ProductionVariantInstanceType_Ml_m5_24xlarge,
        ProductionVariantInstanceType_Ml_m5_2xlarge,
        ProductionVariantInstanceType_Ml_m5_4xlarge,
        ProductionVariantInstanceType_Ml_m5_large,
        ProductionVariantInstanceType_Ml_m5_xlarge,
        ProductionVariantInstanceType_Ml_m5d_12xlarge,
        ProductionVariantInstanceType_Ml_m5d_24xlarge,
        ProductionVariantInstanceType_Ml_m5d_2xlarge,
        ProductionVariantInstanceType_Ml_m5d_4xlarge,
        ProductionVariantInstanceType_Ml_m5d_large,
        ProductionVariantInstanceType_Ml_m5d_xlarge,
        ProductionVariantInstanceType_Ml_p2_16xlarge,
        ProductionVariantInstanceType_Ml_p2_8xlarge,
        ProductionVariantInstanceType_Ml_p2_xlarge,
        ProductionVariantInstanceType_Ml_p3_16xlarge,
        ProductionVariantInstanceType_Ml_p3_2xlarge,
        ProductionVariantInstanceType_Ml_p3_8xlarge,
        ProductionVariantInstanceType_Ml_r5_12xlarge,
        ProductionVariantInstanceType_Ml_r5_24xlarge,
        ProductionVariantInstanceType_Ml_r5_2xlarge,
        ProductionVariantInstanceType_Ml_r5_4xlarge,
        ProductionVariantInstanceType_Ml_r5_large,
        ProductionVariantInstanceType_Ml_r5_xlarge,
        ProductionVariantInstanceType_Ml_r5d_12xlarge,
        ProductionVariantInstanceType_Ml_r5d_24xlarge,
        ProductionVariantInstanceType_Ml_r5d_2xlarge,
        ProductionVariantInstanceType_Ml_r5d_4xlarge,
        ProductionVariantInstanceType_Ml_r5d_large,
        ProductionVariantInstanceType_Ml_r5d_xlarge,
        ProductionVariantInstanceType_Ml_t2_2xlarge,
        ProductionVariantInstanceType_Ml_t2_large,
        ProductionVariantInstanceType_Ml_t2_medium,
        ProductionVariantInstanceType_Ml_t2_xlarge
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProductionVariantInstanceType = ProductionVariantInstanceType'
  { fromProductionVariantInstanceType ::
      Core.Text
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

pattern ProductionVariantInstanceType_Ml_c4_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c4_2xlarge = ProductionVariantInstanceType' "ml.c4.2xlarge"

pattern ProductionVariantInstanceType_Ml_c4_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c4_4xlarge = ProductionVariantInstanceType' "ml.c4.4xlarge"

pattern ProductionVariantInstanceType_Ml_c4_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c4_8xlarge = ProductionVariantInstanceType' "ml.c4.8xlarge"

pattern ProductionVariantInstanceType_Ml_c4_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c4_large = ProductionVariantInstanceType' "ml.c4.large"

pattern ProductionVariantInstanceType_Ml_c4_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c4_xlarge = ProductionVariantInstanceType' "ml.c4.xlarge"

pattern ProductionVariantInstanceType_Ml_c5_18xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5_18xlarge = ProductionVariantInstanceType' "ml.c5.18xlarge"

pattern ProductionVariantInstanceType_Ml_c5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5_2xlarge = ProductionVariantInstanceType' "ml.c5.2xlarge"

pattern ProductionVariantInstanceType_Ml_c5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5_4xlarge = ProductionVariantInstanceType' "ml.c5.4xlarge"

pattern ProductionVariantInstanceType_Ml_c5_9xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5_9xlarge = ProductionVariantInstanceType' "ml.c5.9xlarge"

pattern ProductionVariantInstanceType_Ml_c5_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5_large = ProductionVariantInstanceType' "ml.c5.large"

pattern ProductionVariantInstanceType_Ml_c5_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5_xlarge = ProductionVariantInstanceType' "ml.c5.xlarge"

pattern ProductionVariantInstanceType_Ml_c5d_18xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5d_18xlarge = ProductionVariantInstanceType' "ml.c5d.18xlarge"

pattern ProductionVariantInstanceType_Ml_c5d_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5d_2xlarge = ProductionVariantInstanceType' "ml.c5d.2xlarge"

pattern ProductionVariantInstanceType_Ml_c5d_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5d_4xlarge = ProductionVariantInstanceType' "ml.c5d.4xlarge"

pattern ProductionVariantInstanceType_Ml_c5d_9xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5d_9xlarge = ProductionVariantInstanceType' "ml.c5d.9xlarge"

pattern ProductionVariantInstanceType_Ml_c5d_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5d_large = ProductionVariantInstanceType' "ml.c5d.large"

pattern ProductionVariantInstanceType_Ml_c5d_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c5d_xlarge = ProductionVariantInstanceType' "ml.c5d.xlarge"

pattern ProductionVariantInstanceType_Ml_g4dn_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g4dn_12xlarge = ProductionVariantInstanceType' "ml.g4dn.12xlarge"

pattern ProductionVariantInstanceType_Ml_g4dn_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g4dn_16xlarge = ProductionVariantInstanceType' "ml.g4dn.16xlarge"

pattern ProductionVariantInstanceType_Ml_g4dn_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g4dn_2xlarge = ProductionVariantInstanceType' "ml.g4dn.2xlarge"

pattern ProductionVariantInstanceType_Ml_g4dn_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g4dn_4xlarge = ProductionVariantInstanceType' "ml.g4dn.4xlarge"

pattern ProductionVariantInstanceType_Ml_g4dn_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g4dn_8xlarge = ProductionVariantInstanceType' "ml.g4dn.8xlarge"

pattern ProductionVariantInstanceType_Ml_g4dn_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g4dn_xlarge = ProductionVariantInstanceType' "ml.g4dn.xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_24xlarge = ProductionVariantInstanceType' "ml.inf1.24xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_2xlarge = ProductionVariantInstanceType' "ml.inf1.2xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_6xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_6xlarge = ProductionVariantInstanceType' "ml.inf1.6xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_xlarge = ProductionVariantInstanceType' "ml.inf1.xlarge"

pattern ProductionVariantInstanceType_Ml_m4_10xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m4_10xlarge = ProductionVariantInstanceType' "ml.m4.10xlarge"

pattern ProductionVariantInstanceType_Ml_m4_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m4_16xlarge = ProductionVariantInstanceType' "ml.m4.16xlarge"

pattern ProductionVariantInstanceType_Ml_m4_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m4_2xlarge = ProductionVariantInstanceType' "ml.m4.2xlarge"

pattern ProductionVariantInstanceType_Ml_m4_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m4_4xlarge = ProductionVariantInstanceType' "ml.m4.4xlarge"

pattern ProductionVariantInstanceType_Ml_m4_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m4_xlarge = ProductionVariantInstanceType' "ml.m4.xlarge"

pattern ProductionVariantInstanceType_Ml_m5_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5_12xlarge = ProductionVariantInstanceType' "ml.m5.12xlarge"

pattern ProductionVariantInstanceType_Ml_m5_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5_24xlarge = ProductionVariantInstanceType' "ml.m5.24xlarge"

pattern ProductionVariantInstanceType_Ml_m5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5_2xlarge = ProductionVariantInstanceType' "ml.m5.2xlarge"

pattern ProductionVariantInstanceType_Ml_m5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5_4xlarge = ProductionVariantInstanceType' "ml.m5.4xlarge"

pattern ProductionVariantInstanceType_Ml_m5_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5_large = ProductionVariantInstanceType' "ml.m5.large"

pattern ProductionVariantInstanceType_Ml_m5_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5_xlarge = ProductionVariantInstanceType' "ml.m5.xlarge"

pattern ProductionVariantInstanceType_Ml_m5d_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5d_12xlarge = ProductionVariantInstanceType' "ml.m5d.12xlarge"

pattern ProductionVariantInstanceType_Ml_m5d_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5d_24xlarge = ProductionVariantInstanceType' "ml.m5d.24xlarge"

pattern ProductionVariantInstanceType_Ml_m5d_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5d_2xlarge = ProductionVariantInstanceType' "ml.m5d.2xlarge"

pattern ProductionVariantInstanceType_Ml_m5d_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5d_4xlarge = ProductionVariantInstanceType' "ml.m5d.4xlarge"

pattern ProductionVariantInstanceType_Ml_m5d_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5d_large = ProductionVariantInstanceType' "ml.m5d.large"

pattern ProductionVariantInstanceType_Ml_m5d_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m5d_xlarge = ProductionVariantInstanceType' "ml.m5d.xlarge"

pattern ProductionVariantInstanceType_Ml_p2_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p2_16xlarge = ProductionVariantInstanceType' "ml.p2.16xlarge"

pattern ProductionVariantInstanceType_Ml_p2_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p2_8xlarge = ProductionVariantInstanceType' "ml.p2.8xlarge"

pattern ProductionVariantInstanceType_Ml_p2_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p2_xlarge = ProductionVariantInstanceType' "ml.p2.xlarge"

pattern ProductionVariantInstanceType_Ml_p3_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p3_16xlarge = ProductionVariantInstanceType' "ml.p3.16xlarge"

pattern ProductionVariantInstanceType_Ml_p3_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p3_2xlarge = ProductionVariantInstanceType' "ml.p3.2xlarge"

pattern ProductionVariantInstanceType_Ml_p3_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p3_8xlarge = ProductionVariantInstanceType' "ml.p3.8xlarge"

pattern ProductionVariantInstanceType_Ml_r5_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5_12xlarge = ProductionVariantInstanceType' "ml.r5.12xlarge"

pattern ProductionVariantInstanceType_Ml_r5_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5_24xlarge = ProductionVariantInstanceType' "ml.r5.24xlarge"

pattern ProductionVariantInstanceType_Ml_r5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5_2xlarge = ProductionVariantInstanceType' "ml.r5.2xlarge"

pattern ProductionVariantInstanceType_Ml_r5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5_4xlarge = ProductionVariantInstanceType' "ml.r5.4xlarge"

pattern ProductionVariantInstanceType_Ml_r5_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5_large = ProductionVariantInstanceType' "ml.r5.large"

pattern ProductionVariantInstanceType_Ml_r5_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5_xlarge = ProductionVariantInstanceType' "ml.r5.xlarge"

pattern ProductionVariantInstanceType_Ml_r5d_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5d_12xlarge = ProductionVariantInstanceType' "ml.r5d.12xlarge"

pattern ProductionVariantInstanceType_Ml_r5d_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5d_24xlarge = ProductionVariantInstanceType' "ml.r5d.24xlarge"

pattern ProductionVariantInstanceType_Ml_r5d_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5d_2xlarge = ProductionVariantInstanceType' "ml.r5d.2xlarge"

pattern ProductionVariantInstanceType_Ml_r5d_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5d_4xlarge = ProductionVariantInstanceType' "ml.r5d.4xlarge"

pattern ProductionVariantInstanceType_Ml_r5d_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5d_large = ProductionVariantInstanceType' "ml.r5d.large"

pattern ProductionVariantInstanceType_Ml_r5d_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r5d_xlarge = ProductionVariantInstanceType' "ml.r5d.xlarge"

pattern ProductionVariantInstanceType_Ml_t2_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_2xlarge = ProductionVariantInstanceType' "ml.t2.2xlarge"

pattern ProductionVariantInstanceType_Ml_t2_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_large = ProductionVariantInstanceType' "ml.t2.large"

pattern ProductionVariantInstanceType_Ml_t2_medium :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_medium = ProductionVariantInstanceType' "ml.t2.medium"

pattern ProductionVariantInstanceType_Ml_t2_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_xlarge = ProductionVariantInstanceType' "ml.t2.xlarge"

{-# COMPLETE
  ProductionVariantInstanceType_Ml_c4_2xlarge,
  ProductionVariantInstanceType_Ml_c4_4xlarge,
  ProductionVariantInstanceType_Ml_c4_8xlarge,
  ProductionVariantInstanceType_Ml_c4_large,
  ProductionVariantInstanceType_Ml_c4_xlarge,
  ProductionVariantInstanceType_Ml_c5_18xlarge,
  ProductionVariantInstanceType_Ml_c5_2xlarge,
  ProductionVariantInstanceType_Ml_c5_4xlarge,
  ProductionVariantInstanceType_Ml_c5_9xlarge,
  ProductionVariantInstanceType_Ml_c5_large,
  ProductionVariantInstanceType_Ml_c5_xlarge,
  ProductionVariantInstanceType_Ml_c5d_18xlarge,
  ProductionVariantInstanceType_Ml_c5d_2xlarge,
  ProductionVariantInstanceType_Ml_c5d_4xlarge,
  ProductionVariantInstanceType_Ml_c5d_9xlarge,
  ProductionVariantInstanceType_Ml_c5d_large,
  ProductionVariantInstanceType_Ml_c5d_xlarge,
  ProductionVariantInstanceType_Ml_g4dn_12xlarge,
  ProductionVariantInstanceType_Ml_g4dn_16xlarge,
  ProductionVariantInstanceType_Ml_g4dn_2xlarge,
  ProductionVariantInstanceType_Ml_g4dn_4xlarge,
  ProductionVariantInstanceType_Ml_g4dn_8xlarge,
  ProductionVariantInstanceType_Ml_g4dn_xlarge,
  ProductionVariantInstanceType_Ml_inf1_24xlarge,
  ProductionVariantInstanceType_Ml_inf1_2xlarge,
  ProductionVariantInstanceType_Ml_inf1_6xlarge,
  ProductionVariantInstanceType_Ml_inf1_xlarge,
  ProductionVariantInstanceType_Ml_m4_10xlarge,
  ProductionVariantInstanceType_Ml_m4_16xlarge,
  ProductionVariantInstanceType_Ml_m4_2xlarge,
  ProductionVariantInstanceType_Ml_m4_4xlarge,
  ProductionVariantInstanceType_Ml_m4_xlarge,
  ProductionVariantInstanceType_Ml_m5_12xlarge,
  ProductionVariantInstanceType_Ml_m5_24xlarge,
  ProductionVariantInstanceType_Ml_m5_2xlarge,
  ProductionVariantInstanceType_Ml_m5_4xlarge,
  ProductionVariantInstanceType_Ml_m5_large,
  ProductionVariantInstanceType_Ml_m5_xlarge,
  ProductionVariantInstanceType_Ml_m5d_12xlarge,
  ProductionVariantInstanceType_Ml_m5d_24xlarge,
  ProductionVariantInstanceType_Ml_m5d_2xlarge,
  ProductionVariantInstanceType_Ml_m5d_4xlarge,
  ProductionVariantInstanceType_Ml_m5d_large,
  ProductionVariantInstanceType_Ml_m5d_xlarge,
  ProductionVariantInstanceType_Ml_p2_16xlarge,
  ProductionVariantInstanceType_Ml_p2_8xlarge,
  ProductionVariantInstanceType_Ml_p2_xlarge,
  ProductionVariantInstanceType_Ml_p3_16xlarge,
  ProductionVariantInstanceType_Ml_p3_2xlarge,
  ProductionVariantInstanceType_Ml_p3_8xlarge,
  ProductionVariantInstanceType_Ml_r5_12xlarge,
  ProductionVariantInstanceType_Ml_r5_24xlarge,
  ProductionVariantInstanceType_Ml_r5_2xlarge,
  ProductionVariantInstanceType_Ml_r5_4xlarge,
  ProductionVariantInstanceType_Ml_r5_large,
  ProductionVariantInstanceType_Ml_r5_xlarge,
  ProductionVariantInstanceType_Ml_r5d_12xlarge,
  ProductionVariantInstanceType_Ml_r5d_24xlarge,
  ProductionVariantInstanceType_Ml_r5d_2xlarge,
  ProductionVariantInstanceType_Ml_r5d_4xlarge,
  ProductionVariantInstanceType_Ml_r5d_large,
  ProductionVariantInstanceType_Ml_r5d_xlarge,
  ProductionVariantInstanceType_Ml_t2_2xlarge,
  ProductionVariantInstanceType_Ml_t2_large,
  ProductionVariantInstanceType_Ml_t2_medium,
  ProductionVariantInstanceType_Ml_t2_xlarge,
  ProductionVariantInstanceType'
  #-}
