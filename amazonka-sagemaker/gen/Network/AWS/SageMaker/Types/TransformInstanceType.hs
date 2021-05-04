{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformInstanceType
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

import qualified Network.AWS.Prelude as Prelude

newtype TransformInstanceType = TransformInstanceType'
  { fromTransformInstanceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
