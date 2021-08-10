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
-- Module      : Network.AWS.SageMaker.Types.InstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.InstanceType
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
        InstanceType_Ml_p2_16xlarge,
        InstanceType_Ml_p2_8xlarge,
        InstanceType_Ml_p2_xlarge,
        InstanceType_Ml_p3_16xlarge,
        InstanceType_Ml_p3_2xlarge,
        InstanceType_Ml_p3_8xlarge,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceType = InstanceType'
  { fromInstanceType ::
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
  InstanceType_Ml_p2_16xlarge,
  InstanceType_Ml_p2_8xlarge,
  InstanceType_Ml_p2_xlarge,
  InstanceType_Ml_p3_16xlarge,
  InstanceType_Ml_p3_2xlarge,
  InstanceType_Ml_p3_8xlarge,
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
