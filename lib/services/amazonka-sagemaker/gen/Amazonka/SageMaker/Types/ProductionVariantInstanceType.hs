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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantInstanceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantInstanceType
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
        ProductionVariantInstanceType_Ml_c6g_12xlarge,
        ProductionVariantInstanceType_Ml_c6g_16xlarge,
        ProductionVariantInstanceType_Ml_c6g_2xlarge,
        ProductionVariantInstanceType_Ml_c6g_4xlarge,
        ProductionVariantInstanceType_Ml_c6g_8xlarge,
        ProductionVariantInstanceType_Ml_c6g_large,
        ProductionVariantInstanceType_Ml_c6g_xlarge,
        ProductionVariantInstanceType_Ml_c6gd_12xlarge,
        ProductionVariantInstanceType_Ml_c6gd_16xlarge,
        ProductionVariantInstanceType_Ml_c6gd_2xlarge,
        ProductionVariantInstanceType_Ml_c6gd_4xlarge,
        ProductionVariantInstanceType_Ml_c6gd_8xlarge,
        ProductionVariantInstanceType_Ml_c6gd_large,
        ProductionVariantInstanceType_Ml_c6gd_xlarge,
        ProductionVariantInstanceType_Ml_c6gn_12xlarge,
        ProductionVariantInstanceType_Ml_c6gn_16xlarge,
        ProductionVariantInstanceType_Ml_c6gn_2xlarge,
        ProductionVariantInstanceType_Ml_c6gn_4xlarge,
        ProductionVariantInstanceType_Ml_c6gn_8xlarge,
        ProductionVariantInstanceType_Ml_c6gn_large,
        ProductionVariantInstanceType_Ml_c6gn_xlarge,
        ProductionVariantInstanceType_Ml_c6i_12xlarge,
        ProductionVariantInstanceType_Ml_c6i_16xlarge,
        ProductionVariantInstanceType_Ml_c6i_24xlarge,
        ProductionVariantInstanceType_Ml_c6i_2xlarge,
        ProductionVariantInstanceType_Ml_c6i_32xlarge,
        ProductionVariantInstanceType_Ml_c6i_4xlarge,
        ProductionVariantInstanceType_Ml_c6i_8xlarge,
        ProductionVariantInstanceType_Ml_c6i_large,
        ProductionVariantInstanceType_Ml_c6i_xlarge,
        ProductionVariantInstanceType_Ml_c7g_12xlarge,
        ProductionVariantInstanceType_Ml_c7g_16xlarge,
        ProductionVariantInstanceType_Ml_c7g_2xlarge,
        ProductionVariantInstanceType_Ml_c7g_4xlarge,
        ProductionVariantInstanceType_Ml_c7g_8xlarge,
        ProductionVariantInstanceType_Ml_c7g_large,
        ProductionVariantInstanceType_Ml_c7g_xlarge,
        ProductionVariantInstanceType_Ml_g4dn_12xlarge,
        ProductionVariantInstanceType_Ml_g4dn_16xlarge,
        ProductionVariantInstanceType_Ml_g4dn_2xlarge,
        ProductionVariantInstanceType_Ml_g4dn_4xlarge,
        ProductionVariantInstanceType_Ml_g4dn_8xlarge,
        ProductionVariantInstanceType_Ml_g4dn_xlarge,
        ProductionVariantInstanceType_Ml_g5_12xlarge,
        ProductionVariantInstanceType_Ml_g5_16xlarge,
        ProductionVariantInstanceType_Ml_g5_24xlarge,
        ProductionVariantInstanceType_Ml_g5_2xlarge,
        ProductionVariantInstanceType_Ml_g5_48xlarge,
        ProductionVariantInstanceType_Ml_g5_4xlarge,
        ProductionVariantInstanceType_Ml_g5_8xlarge,
        ProductionVariantInstanceType_Ml_g5_xlarge,
        ProductionVariantInstanceType_Ml_inf1_24xlarge,
        ProductionVariantInstanceType_Ml_inf1_2xlarge,
        ProductionVariantInstanceType_Ml_inf1_6xlarge,
        ProductionVariantInstanceType_Ml_inf1_xlarge,
        ProductionVariantInstanceType_Ml_inf2_24xlarge,
        ProductionVariantInstanceType_Ml_inf2_48xlarge,
        ProductionVariantInstanceType_Ml_inf2_8xlarge,
        ProductionVariantInstanceType_Ml_inf2_xlarge,
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
        ProductionVariantInstanceType_Ml_m6g_12xlarge,
        ProductionVariantInstanceType_Ml_m6g_16xlarge,
        ProductionVariantInstanceType_Ml_m6g_2xlarge,
        ProductionVariantInstanceType_Ml_m6g_4xlarge,
        ProductionVariantInstanceType_Ml_m6g_8xlarge,
        ProductionVariantInstanceType_Ml_m6g_large,
        ProductionVariantInstanceType_Ml_m6g_xlarge,
        ProductionVariantInstanceType_Ml_m6gd_12xlarge,
        ProductionVariantInstanceType_Ml_m6gd_16xlarge,
        ProductionVariantInstanceType_Ml_m6gd_2xlarge,
        ProductionVariantInstanceType_Ml_m6gd_4xlarge,
        ProductionVariantInstanceType_Ml_m6gd_8xlarge,
        ProductionVariantInstanceType_Ml_m6gd_large,
        ProductionVariantInstanceType_Ml_m6gd_xlarge,
        ProductionVariantInstanceType_Ml_p2_16xlarge,
        ProductionVariantInstanceType_Ml_p2_8xlarge,
        ProductionVariantInstanceType_Ml_p2_xlarge,
        ProductionVariantInstanceType_Ml_p3_16xlarge,
        ProductionVariantInstanceType_Ml_p3_2xlarge,
        ProductionVariantInstanceType_Ml_p3_8xlarge,
        ProductionVariantInstanceType_Ml_p4d_24xlarge,
        ProductionVariantInstanceType_Ml_p4de_24xlarge,
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
        ProductionVariantInstanceType_Ml_r6g_12xlarge,
        ProductionVariantInstanceType_Ml_r6g_16xlarge,
        ProductionVariantInstanceType_Ml_r6g_2xlarge,
        ProductionVariantInstanceType_Ml_r6g_4xlarge,
        ProductionVariantInstanceType_Ml_r6g_8xlarge,
        ProductionVariantInstanceType_Ml_r6g_large,
        ProductionVariantInstanceType_Ml_r6g_xlarge,
        ProductionVariantInstanceType_Ml_r6gd_12xlarge,
        ProductionVariantInstanceType_Ml_r6gd_16xlarge,
        ProductionVariantInstanceType_Ml_r6gd_2xlarge,
        ProductionVariantInstanceType_Ml_r6gd_4xlarge,
        ProductionVariantInstanceType_Ml_r6gd_8xlarge,
        ProductionVariantInstanceType_Ml_r6gd_large,
        ProductionVariantInstanceType_Ml_r6gd_xlarge,
        ProductionVariantInstanceType_Ml_t2_2xlarge,
        ProductionVariantInstanceType_Ml_t2_large,
        ProductionVariantInstanceType_Ml_t2_medium,
        ProductionVariantInstanceType_Ml_t2_xlarge,
        ProductionVariantInstanceType_Ml_trn1_2xlarge,
        ProductionVariantInstanceType_Ml_trn1_32xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProductionVariantInstanceType = ProductionVariantInstanceType'
  { fromProductionVariantInstanceType ::
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

pattern ProductionVariantInstanceType_Ml_c6g_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_12xlarge = ProductionVariantInstanceType' "ml.c6g.12xlarge"

pattern ProductionVariantInstanceType_Ml_c6g_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_16xlarge = ProductionVariantInstanceType' "ml.c6g.16xlarge"

pattern ProductionVariantInstanceType_Ml_c6g_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_2xlarge = ProductionVariantInstanceType' "ml.c6g.2xlarge"

pattern ProductionVariantInstanceType_Ml_c6g_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_4xlarge = ProductionVariantInstanceType' "ml.c6g.4xlarge"

pattern ProductionVariantInstanceType_Ml_c6g_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_8xlarge = ProductionVariantInstanceType' "ml.c6g.8xlarge"

pattern ProductionVariantInstanceType_Ml_c6g_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_large = ProductionVariantInstanceType' "ml.c6g.large"

pattern ProductionVariantInstanceType_Ml_c6g_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6g_xlarge = ProductionVariantInstanceType' "ml.c6g.xlarge"

pattern ProductionVariantInstanceType_Ml_c6gd_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_12xlarge = ProductionVariantInstanceType' "ml.c6gd.12xlarge"

pattern ProductionVariantInstanceType_Ml_c6gd_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_16xlarge = ProductionVariantInstanceType' "ml.c6gd.16xlarge"

pattern ProductionVariantInstanceType_Ml_c6gd_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_2xlarge = ProductionVariantInstanceType' "ml.c6gd.2xlarge"

pattern ProductionVariantInstanceType_Ml_c6gd_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_4xlarge = ProductionVariantInstanceType' "ml.c6gd.4xlarge"

pattern ProductionVariantInstanceType_Ml_c6gd_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_8xlarge = ProductionVariantInstanceType' "ml.c6gd.8xlarge"

pattern ProductionVariantInstanceType_Ml_c6gd_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_large = ProductionVariantInstanceType' "ml.c6gd.large"

pattern ProductionVariantInstanceType_Ml_c6gd_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gd_xlarge = ProductionVariantInstanceType' "ml.c6gd.xlarge"

pattern ProductionVariantInstanceType_Ml_c6gn_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_12xlarge = ProductionVariantInstanceType' "ml.c6gn.12xlarge"

pattern ProductionVariantInstanceType_Ml_c6gn_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_16xlarge = ProductionVariantInstanceType' "ml.c6gn.16xlarge"

pattern ProductionVariantInstanceType_Ml_c6gn_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_2xlarge = ProductionVariantInstanceType' "ml.c6gn.2xlarge"

pattern ProductionVariantInstanceType_Ml_c6gn_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_4xlarge = ProductionVariantInstanceType' "ml.c6gn.4xlarge"

pattern ProductionVariantInstanceType_Ml_c6gn_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_8xlarge = ProductionVariantInstanceType' "ml.c6gn.8xlarge"

pattern ProductionVariantInstanceType_Ml_c6gn_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_large = ProductionVariantInstanceType' "ml.c6gn.large"

pattern ProductionVariantInstanceType_Ml_c6gn_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6gn_xlarge = ProductionVariantInstanceType' "ml.c6gn.xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_12xlarge = ProductionVariantInstanceType' "ml.c6i.12xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_16xlarge = ProductionVariantInstanceType' "ml.c6i.16xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_24xlarge = ProductionVariantInstanceType' "ml.c6i.24xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_2xlarge = ProductionVariantInstanceType' "ml.c6i.2xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_32xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_32xlarge = ProductionVariantInstanceType' "ml.c6i.32xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_4xlarge = ProductionVariantInstanceType' "ml.c6i.4xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_8xlarge = ProductionVariantInstanceType' "ml.c6i.8xlarge"

pattern ProductionVariantInstanceType_Ml_c6i_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_large = ProductionVariantInstanceType' "ml.c6i.large"

pattern ProductionVariantInstanceType_Ml_c6i_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c6i_xlarge = ProductionVariantInstanceType' "ml.c6i.xlarge"

pattern ProductionVariantInstanceType_Ml_c7g_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_12xlarge = ProductionVariantInstanceType' "ml.c7g.12xlarge"

pattern ProductionVariantInstanceType_Ml_c7g_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_16xlarge = ProductionVariantInstanceType' "ml.c7g.16xlarge"

pattern ProductionVariantInstanceType_Ml_c7g_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_2xlarge = ProductionVariantInstanceType' "ml.c7g.2xlarge"

pattern ProductionVariantInstanceType_Ml_c7g_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_4xlarge = ProductionVariantInstanceType' "ml.c7g.4xlarge"

pattern ProductionVariantInstanceType_Ml_c7g_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_8xlarge = ProductionVariantInstanceType' "ml.c7g.8xlarge"

pattern ProductionVariantInstanceType_Ml_c7g_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_large = ProductionVariantInstanceType' "ml.c7g.large"

pattern ProductionVariantInstanceType_Ml_c7g_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_c7g_xlarge = ProductionVariantInstanceType' "ml.c7g.xlarge"

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

pattern ProductionVariantInstanceType_Ml_g5_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_12xlarge = ProductionVariantInstanceType' "ml.g5.12xlarge"

pattern ProductionVariantInstanceType_Ml_g5_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_16xlarge = ProductionVariantInstanceType' "ml.g5.16xlarge"

pattern ProductionVariantInstanceType_Ml_g5_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_24xlarge = ProductionVariantInstanceType' "ml.g5.24xlarge"

pattern ProductionVariantInstanceType_Ml_g5_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_2xlarge = ProductionVariantInstanceType' "ml.g5.2xlarge"

pattern ProductionVariantInstanceType_Ml_g5_48xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_48xlarge = ProductionVariantInstanceType' "ml.g5.48xlarge"

pattern ProductionVariantInstanceType_Ml_g5_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_4xlarge = ProductionVariantInstanceType' "ml.g5.4xlarge"

pattern ProductionVariantInstanceType_Ml_g5_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_8xlarge = ProductionVariantInstanceType' "ml.g5.8xlarge"

pattern ProductionVariantInstanceType_Ml_g5_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_g5_xlarge = ProductionVariantInstanceType' "ml.g5.xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_24xlarge = ProductionVariantInstanceType' "ml.inf1.24xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_2xlarge = ProductionVariantInstanceType' "ml.inf1.2xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_6xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_6xlarge = ProductionVariantInstanceType' "ml.inf1.6xlarge"

pattern ProductionVariantInstanceType_Ml_inf1_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf1_xlarge = ProductionVariantInstanceType' "ml.inf1.xlarge"

pattern ProductionVariantInstanceType_Ml_inf2_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf2_24xlarge = ProductionVariantInstanceType' "ml.inf2.24xlarge"

pattern ProductionVariantInstanceType_Ml_inf2_48xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf2_48xlarge = ProductionVariantInstanceType' "ml.inf2.48xlarge"

pattern ProductionVariantInstanceType_Ml_inf2_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf2_8xlarge = ProductionVariantInstanceType' "ml.inf2.8xlarge"

pattern ProductionVariantInstanceType_Ml_inf2_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_inf2_xlarge = ProductionVariantInstanceType' "ml.inf2.xlarge"

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

pattern ProductionVariantInstanceType_Ml_m6g_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_12xlarge = ProductionVariantInstanceType' "ml.m6g.12xlarge"

pattern ProductionVariantInstanceType_Ml_m6g_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_16xlarge = ProductionVariantInstanceType' "ml.m6g.16xlarge"

pattern ProductionVariantInstanceType_Ml_m6g_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_2xlarge = ProductionVariantInstanceType' "ml.m6g.2xlarge"

pattern ProductionVariantInstanceType_Ml_m6g_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_4xlarge = ProductionVariantInstanceType' "ml.m6g.4xlarge"

pattern ProductionVariantInstanceType_Ml_m6g_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_8xlarge = ProductionVariantInstanceType' "ml.m6g.8xlarge"

pattern ProductionVariantInstanceType_Ml_m6g_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_large = ProductionVariantInstanceType' "ml.m6g.large"

pattern ProductionVariantInstanceType_Ml_m6g_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6g_xlarge = ProductionVariantInstanceType' "ml.m6g.xlarge"

pattern ProductionVariantInstanceType_Ml_m6gd_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_12xlarge = ProductionVariantInstanceType' "ml.m6gd.12xlarge"

pattern ProductionVariantInstanceType_Ml_m6gd_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_16xlarge = ProductionVariantInstanceType' "ml.m6gd.16xlarge"

pattern ProductionVariantInstanceType_Ml_m6gd_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_2xlarge = ProductionVariantInstanceType' "ml.m6gd.2xlarge"

pattern ProductionVariantInstanceType_Ml_m6gd_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_4xlarge = ProductionVariantInstanceType' "ml.m6gd.4xlarge"

pattern ProductionVariantInstanceType_Ml_m6gd_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_8xlarge = ProductionVariantInstanceType' "ml.m6gd.8xlarge"

pattern ProductionVariantInstanceType_Ml_m6gd_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_large = ProductionVariantInstanceType' "ml.m6gd.large"

pattern ProductionVariantInstanceType_Ml_m6gd_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_m6gd_xlarge = ProductionVariantInstanceType' "ml.m6gd.xlarge"

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

pattern ProductionVariantInstanceType_Ml_p4d_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p4d_24xlarge = ProductionVariantInstanceType' "ml.p4d.24xlarge"

pattern ProductionVariantInstanceType_Ml_p4de_24xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_p4de_24xlarge = ProductionVariantInstanceType' "ml.p4de.24xlarge"

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

pattern ProductionVariantInstanceType_Ml_r6g_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_12xlarge = ProductionVariantInstanceType' "ml.r6g.12xlarge"

pattern ProductionVariantInstanceType_Ml_r6g_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_16xlarge = ProductionVariantInstanceType' "ml.r6g.16xlarge"

pattern ProductionVariantInstanceType_Ml_r6g_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_2xlarge = ProductionVariantInstanceType' "ml.r6g.2xlarge"

pattern ProductionVariantInstanceType_Ml_r6g_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_4xlarge = ProductionVariantInstanceType' "ml.r6g.4xlarge"

pattern ProductionVariantInstanceType_Ml_r6g_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_8xlarge = ProductionVariantInstanceType' "ml.r6g.8xlarge"

pattern ProductionVariantInstanceType_Ml_r6g_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_large = ProductionVariantInstanceType' "ml.r6g.large"

pattern ProductionVariantInstanceType_Ml_r6g_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6g_xlarge = ProductionVariantInstanceType' "ml.r6g.xlarge"

pattern ProductionVariantInstanceType_Ml_r6gd_12xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_12xlarge = ProductionVariantInstanceType' "ml.r6gd.12xlarge"

pattern ProductionVariantInstanceType_Ml_r6gd_16xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_16xlarge = ProductionVariantInstanceType' "ml.r6gd.16xlarge"

pattern ProductionVariantInstanceType_Ml_r6gd_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_2xlarge = ProductionVariantInstanceType' "ml.r6gd.2xlarge"

pattern ProductionVariantInstanceType_Ml_r6gd_4xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_4xlarge = ProductionVariantInstanceType' "ml.r6gd.4xlarge"

pattern ProductionVariantInstanceType_Ml_r6gd_8xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_8xlarge = ProductionVariantInstanceType' "ml.r6gd.8xlarge"

pattern ProductionVariantInstanceType_Ml_r6gd_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_large = ProductionVariantInstanceType' "ml.r6gd.large"

pattern ProductionVariantInstanceType_Ml_r6gd_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_r6gd_xlarge = ProductionVariantInstanceType' "ml.r6gd.xlarge"

pattern ProductionVariantInstanceType_Ml_t2_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_2xlarge = ProductionVariantInstanceType' "ml.t2.2xlarge"

pattern ProductionVariantInstanceType_Ml_t2_large :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_large = ProductionVariantInstanceType' "ml.t2.large"

pattern ProductionVariantInstanceType_Ml_t2_medium :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_medium = ProductionVariantInstanceType' "ml.t2.medium"

pattern ProductionVariantInstanceType_Ml_t2_xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_t2_xlarge = ProductionVariantInstanceType' "ml.t2.xlarge"

pattern ProductionVariantInstanceType_Ml_trn1_2xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_trn1_2xlarge = ProductionVariantInstanceType' "ml.trn1.2xlarge"

pattern ProductionVariantInstanceType_Ml_trn1_32xlarge :: ProductionVariantInstanceType
pattern ProductionVariantInstanceType_Ml_trn1_32xlarge = ProductionVariantInstanceType' "ml.trn1.32xlarge"

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
  ProductionVariantInstanceType_Ml_c6g_12xlarge,
  ProductionVariantInstanceType_Ml_c6g_16xlarge,
  ProductionVariantInstanceType_Ml_c6g_2xlarge,
  ProductionVariantInstanceType_Ml_c6g_4xlarge,
  ProductionVariantInstanceType_Ml_c6g_8xlarge,
  ProductionVariantInstanceType_Ml_c6g_large,
  ProductionVariantInstanceType_Ml_c6g_xlarge,
  ProductionVariantInstanceType_Ml_c6gd_12xlarge,
  ProductionVariantInstanceType_Ml_c6gd_16xlarge,
  ProductionVariantInstanceType_Ml_c6gd_2xlarge,
  ProductionVariantInstanceType_Ml_c6gd_4xlarge,
  ProductionVariantInstanceType_Ml_c6gd_8xlarge,
  ProductionVariantInstanceType_Ml_c6gd_large,
  ProductionVariantInstanceType_Ml_c6gd_xlarge,
  ProductionVariantInstanceType_Ml_c6gn_12xlarge,
  ProductionVariantInstanceType_Ml_c6gn_16xlarge,
  ProductionVariantInstanceType_Ml_c6gn_2xlarge,
  ProductionVariantInstanceType_Ml_c6gn_4xlarge,
  ProductionVariantInstanceType_Ml_c6gn_8xlarge,
  ProductionVariantInstanceType_Ml_c6gn_large,
  ProductionVariantInstanceType_Ml_c6gn_xlarge,
  ProductionVariantInstanceType_Ml_c6i_12xlarge,
  ProductionVariantInstanceType_Ml_c6i_16xlarge,
  ProductionVariantInstanceType_Ml_c6i_24xlarge,
  ProductionVariantInstanceType_Ml_c6i_2xlarge,
  ProductionVariantInstanceType_Ml_c6i_32xlarge,
  ProductionVariantInstanceType_Ml_c6i_4xlarge,
  ProductionVariantInstanceType_Ml_c6i_8xlarge,
  ProductionVariantInstanceType_Ml_c6i_large,
  ProductionVariantInstanceType_Ml_c6i_xlarge,
  ProductionVariantInstanceType_Ml_c7g_12xlarge,
  ProductionVariantInstanceType_Ml_c7g_16xlarge,
  ProductionVariantInstanceType_Ml_c7g_2xlarge,
  ProductionVariantInstanceType_Ml_c7g_4xlarge,
  ProductionVariantInstanceType_Ml_c7g_8xlarge,
  ProductionVariantInstanceType_Ml_c7g_large,
  ProductionVariantInstanceType_Ml_c7g_xlarge,
  ProductionVariantInstanceType_Ml_g4dn_12xlarge,
  ProductionVariantInstanceType_Ml_g4dn_16xlarge,
  ProductionVariantInstanceType_Ml_g4dn_2xlarge,
  ProductionVariantInstanceType_Ml_g4dn_4xlarge,
  ProductionVariantInstanceType_Ml_g4dn_8xlarge,
  ProductionVariantInstanceType_Ml_g4dn_xlarge,
  ProductionVariantInstanceType_Ml_g5_12xlarge,
  ProductionVariantInstanceType_Ml_g5_16xlarge,
  ProductionVariantInstanceType_Ml_g5_24xlarge,
  ProductionVariantInstanceType_Ml_g5_2xlarge,
  ProductionVariantInstanceType_Ml_g5_48xlarge,
  ProductionVariantInstanceType_Ml_g5_4xlarge,
  ProductionVariantInstanceType_Ml_g5_8xlarge,
  ProductionVariantInstanceType_Ml_g5_xlarge,
  ProductionVariantInstanceType_Ml_inf1_24xlarge,
  ProductionVariantInstanceType_Ml_inf1_2xlarge,
  ProductionVariantInstanceType_Ml_inf1_6xlarge,
  ProductionVariantInstanceType_Ml_inf1_xlarge,
  ProductionVariantInstanceType_Ml_inf2_24xlarge,
  ProductionVariantInstanceType_Ml_inf2_48xlarge,
  ProductionVariantInstanceType_Ml_inf2_8xlarge,
  ProductionVariantInstanceType_Ml_inf2_xlarge,
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
  ProductionVariantInstanceType_Ml_m6g_12xlarge,
  ProductionVariantInstanceType_Ml_m6g_16xlarge,
  ProductionVariantInstanceType_Ml_m6g_2xlarge,
  ProductionVariantInstanceType_Ml_m6g_4xlarge,
  ProductionVariantInstanceType_Ml_m6g_8xlarge,
  ProductionVariantInstanceType_Ml_m6g_large,
  ProductionVariantInstanceType_Ml_m6g_xlarge,
  ProductionVariantInstanceType_Ml_m6gd_12xlarge,
  ProductionVariantInstanceType_Ml_m6gd_16xlarge,
  ProductionVariantInstanceType_Ml_m6gd_2xlarge,
  ProductionVariantInstanceType_Ml_m6gd_4xlarge,
  ProductionVariantInstanceType_Ml_m6gd_8xlarge,
  ProductionVariantInstanceType_Ml_m6gd_large,
  ProductionVariantInstanceType_Ml_m6gd_xlarge,
  ProductionVariantInstanceType_Ml_p2_16xlarge,
  ProductionVariantInstanceType_Ml_p2_8xlarge,
  ProductionVariantInstanceType_Ml_p2_xlarge,
  ProductionVariantInstanceType_Ml_p3_16xlarge,
  ProductionVariantInstanceType_Ml_p3_2xlarge,
  ProductionVariantInstanceType_Ml_p3_8xlarge,
  ProductionVariantInstanceType_Ml_p4d_24xlarge,
  ProductionVariantInstanceType_Ml_p4de_24xlarge,
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
  ProductionVariantInstanceType_Ml_r6g_12xlarge,
  ProductionVariantInstanceType_Ml_r6g_16xlarge,
  ProductionVariantInstanceType_Ml_r6g_2xlarge,
  ProductionVariantInstanceType_Ml_r6g_4xlarge,
  ProductionVariantInstanceType_Ml_r6g_8xlarge,
  ProductionVariantInstanceType_Ml_r6g_large,
  ProductionVariantInstanceType_Ml_r6g_xlarge,
  ProductionVariantInstanceType_Ml_r6gd_12xlarge,
  ProductionVariantInstanceType_Ml_r6gd_16xlarge,
  ProductionVariantInstanceType_Ml_r6gd_2xlarge,
  ProductionVariantInstanceType_Ml_r6gd_4xlarge,
  ProductionVariantInstanceType_Ml_r6gd_8xlarge,
  ProductionVariantInstanceType_Ml_r6gd_large,
  ProductionVariantInstanceType_Ml_r6gd_xlarge,
  ProductionVariantInstanceType_Ml_t2_2xlarge,
  ProductionVariantInstanceType_Ml_t2_large,
  ProductionVariantInstanceType_Ml_t2_medium,
  ProductionVariantInstanceType_Ml_t2_xlarge,
  ProductionVariantInstanceType_Ml_trn1_2xlarge,
  ProductionVariantInstanceType_Ml_trn1_32xlarge,
  ProductionVariantInstanceType'
  #-}
