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
-- Module      : Amazonka.SageMaker.Types.TransformInstanceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TransformInstanceType
  ( TransformInstanceType
      ( ..,
        TransformInstanceType_Ml_c4_2xlarge,
        TransformInstanceType_Ml_c4_4xlarge,
        TransformInstanceType_Ml_c4_8xlarge,
        TransformInstanceType_Ml_c4_xlarge,
        TransformInstanceType_Ml_c5_18xlarge,
        TransformInstanceType_Ml_c5_2xlarge,
        TransformInstanceType_Ml_c5_4xlarge,
        TransformInstanceType_Ml_c5_9xlarge,
        TransformInstanceType_Ml_c5_xlarge,
        TransformInstanceType_Ml_g4dn_12xlarge,
        TransformInstanceType_Ml_g4dn_16xlarge,
        TransformInstanceType_Ml_g4dn_2xlarge,
        TransformInstanceType_Ml_g4dn_4xlarge,
        TransformInstanceType_Ml_g4dn_8xlarge,
        TransformInstanceType_Ml_g4dn_xlarge,
        TransformInstanceType_Ml_m4_10xlarge,
        TransformInstanceType_Ml_m4_16xlarge,
        TransformInstanceType_Ml_m4_2xlarge,
        TransformInstanceType_Ml_m4_4xlarge,
        TransformInstanceType_Ml_m4_xlarge,
        TransformInstanceType_Ml_m5_12xlarge,
        TransformInstanceType_Ml_m5_24xlarge,
        TransformInstanceType_Ml_m5_2xlarge,
        TransformInstanceType_Ml_m5_4xlarge,
        TransformInstanceType_Ml_m5_large,
        TransformInstanceType_Ml_m5_xlarge,
        TransformInstanceType_Ml_p2_16xlarge,
        TransformInstanceType_Ml_p2_8xlarge,
        TransformInstanceType_Ml_p2_xlarge,
        TransformInstanceType_Ml_p3_16xlarge,
        TransformInstanceType_Ml_p3_2xlarge,
        TransformInstanceType_Ml_p3_8xlarge
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TransformInstanceType = TransformInstanceType'
  { fromTransformInstanceType ::
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

pattern TransformInstanceType_Ml_c4_2xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c4_2xlarge = TransformInstanceType' "ml.c4.2xlarge"

pattern TransformInstanceType_Ml_c4_4xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c4_4xlarge = TransformInstanceType' "ml.c4.4xlarge"

pattern TransformInstanceType_Ml_c4_8xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c4_8xlarge = TransformInstanceType' "ml.c4.8xlarge"

pattern TransformInstanceType_Ml_c4_xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c4_xlarge = TransformInstanceType' "ml.c4.xlarge"

pattern TransformInstanceType_Ml_c5_18xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c5_18xlarge = TransformInstanceType' "ml.c5.18xlarge"

pattern TransformInstanceType_Ml_c5_2xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c5_2xlarge = TransformInstanceType' "ml.c5.2xlarge"

pattern TransformInstanceType_Ml_c5_4xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c5_4xlarge = TransformInstanceType' "ml.c5.4xlarge"

pattern TransformInstanceType_Ml_c5_9xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c5_9xlarge = TransformInstanceType' "ml.c5.9xlarge"

pattern TransformInstanceType_Ml_c5_xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_c5_xlarge = TransformInstanceType' "ml.c5.xlarge"

pattern TransformInstanceType_Ml_g4dn_12xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_g4dn_12xlarge = TransformInstanceType' "ml.g4dn.12xlarge"

pattern TransformInstanceType_Ml_g4dn_16xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_g4dn_16xlarge = TransformInstanceType' "ml.g4dn.16xlarge"

pattern TransformInstanceType_Ml_g4dn_2xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_g4dn_2xlarge = TransformInstanceType' "ml.g4dn.2xlarge"

pattern TransformInstanceType_Ml_g4dn_4xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_g4dn_4xlarge = TransformInstanceType' "ml.g4dn.4xlarge"

pattern TransformInstanceType_Ml_g4dn_8xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_g4dn_8xlarge = TransformInstanceType' "ml.g4dn.8xlarge"

pattern TransformInstanceType_Ml_g4dn_xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_g4dn_xlarge = TransformInstanceType' "ml.g4dn.xlarge"

pattern TransformInstanceType_Ml_m4_10xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m4_10xlarge = TransformInstanceType' "ml.m4.10xlarge"

pattern TransformInstanceType_Ml_m4_16xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m4_16xlarge = TransformInstanceType' "ml.m4.16xlarge"

pattern TransformInstanceType_Ml_m4_2xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m4_2xlarge = TransformInstanceType' "ml.m4.2xlarge"

pattern TransformInstanceType_Ml_m4_4xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m4_4xlarge = TransformInstanceType' "ml.m4.4xlarge"

pattern TransformInstanceType_Ml_m4_xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m4_xlarge = TransformInstanceType' "ml.m4.xlarge"

pattern TransformInstanceType_Ml_m5_12xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m5_12xlarge = TransformInstanceType' "ml.m5.12xlarge"

pattern TransformInstanceType_Ml_m5_24xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m5_24xlarge = TransformInstanceType' "ml.m5.24xlarge"

pattern TransformInstanceType_Ml_m5_2xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m5_2xlarge = TransformInstanceType' "ml.m5.2xlarge"

pattern TransformInstanceType_Ml_m5_4xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m5_4xlarge = TransformInstanceType' "ml.m5.4xlarge"

pattern TransformInstanceType_Ml_m5_large :: TransformInstanceType
pattern TransformInstanceType_Ml_m5_large = TransformInstanceType' "ml.m5.large"

pattern TransformInstanceType_Ml_m5_xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_m5_xlarge = TransformInstanceType' "ml.m5.xlarge"

pattern TransformInstanceType_Ml_p2_16xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_p2_16xlarge = TransformInstanceType' "ml.p2.16xlarge"

pattern TransformInstanceType_Ml_p2_8xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_p2_8xlarge = TransformInstanceType' "ml.p2.8xlarge"

pattern TransformInstanceType_Ml_p2_xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_p2_xlarge = TransformInstanceType' "ml.p2.xlarge"

pattern TransformInstanceType_Ml_p3_16xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_p3_16xlarge = TransformInstanceType' "ml.p3.16xlarge"

pattern TransformInstanceType_Ml_p3_2xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_p3_2xlarge = TransformInstanceType' "ml.p3.2xlarge"

pattern TransformInstanceType_Ml_p3_8xlarge :: TransformInstanceType
pattern TransformInstanceType_Ml_p3_8xlarge = TransformInstanceType' "ml.p3.8xlarge"

{-# COMPLETE
  TransformInstanceType_Ml_c4_2xlarge,
  TransformInstanceType_Ml_c4_4xlarge,
  TransformInstanceType_Ml_c4_8xlarge,
  TransformInstanceType_Ml_c4_xlarge,
  TransformInstanceType_Ml_c5_18xlarge,
  TransformInstanceType_Ml_c5_2xlarge,
  TransformInstanceType_Ml_c5_4xlarge,
  TransformInstanceType_Ml_c5_9xlarge,
  TransformInstanceType_Ml_c5_xlarge,
  TransformInstanceType_Ml_g4dn_12xlarge,
  TransformInstanceType_Ml_g4dn_16xlarge,
  TransformInstanceType_Ml_g4dn_2xlarge,
  TransformInstanceType_Ml_g4dn_4xlarge,
  TransformInstanceType_Ml_g4dn_8xlarge,
  TransformInstanceType_Ml_g4dn_xlarge,
  TransformInstanceType_Ml_m4_10xlarge,
  TransformInstanceType_Ml_m4_16xlarge,
  TransformInstanceType_Ml_m4_2xlarge,
  TransformInstanceType_Ml_m4_4xlarge,
  TransformInstanceType_Ml_m4_xlarge,
  TransformInstanceType_Ml_m5_12xlarge,
  TransformInstanceType_Ml_m5_24xlarge,
  TransformInstanceType_Ml_m5_2xlarge,
  TransformInstanceType_Ml_m5_4xlarge,
  TransformInstanceType_Ml_m5_large,
  TransformInstanceType_Ml_m5_xlarge,
  TransformInstanceType_Ml_p2_16xlarge,
  TransformInstanceType_Ml_p2_8xlarge,
  TransformInstanceType_Ml_p2_xlarge,
  TransformInstanceType_Ml_p3_16xlarge,
  TransformInstanceType_Ml_p3_2xlarge,
  TransformInstanceType_Ml_p3_8xlarge,
  TransformInstanceType'
  #-}
