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
-- Module      : Network.AWS.SageMaker.Types.TrainingInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingInstanceType
  ( TrainingInstanceType
      ( ..,
        TrainingInstanceType_Ml_c4_2xlarge,
        TrainingInstanceType_Ml_c4_4xlarge,
        TrainingInstanceType_Ml_c4_8xlarge,
        TrainingInstanceType_Ml_c4_xlarge,
        TrainingInstanceType_Ml_c5_18xlarge,
        TrainingInstanceType_Ml_c5_2xlarge,
        TrainingInstanceType_Ml_c5_4xlarge,
        TrainingInstanceType_Ml_c5_9xlarge,
        TrainingInstanceType_Ml_c5_xlarge,
        TrainingInstanceType_Ml_c5n_18xlarge,
        TrainingInstanceType_Ml_c5n_2xlarge,
        TrainingInstanceType_Ml_c5n_4xlarge,
        TrainingInstanceType_Ml_c5n_9xlarge,
        TrainingInstanceType_Ml_c5n_xlarge,
        TrainingInstanceType_Ml_g4dn_12xlarge,
        TrainingInstanceType_Ml_g4dn_16xlarge,
        TrainingInstanceType_Ml_g4dn_2xlarge,
        TrainingInstanceType_Ml_g4dn_4xlarge,
        TrainingInstanceType_Ml_g4dn_8xlarge,
        TrainingInstanceType_Ml_g4dn_xlarge,
        TrainingInstanceType_Ml_m4_10xlarge,
        TrainingInstanceType_Ml_m4_16xlarge,
        TrainingInstanceType_Ml_m4_2xlarge,
        TrainingInstanceType_Ml_m4_4xlarge,
        TrainingInstanceType_Ml_m4_xlarge,
        TrainingInstanceType_Ml_m5_12xlarge,
        TrainingInstanceType_Ml_m5_24xlarge,
        TrainingInstanceType_Ml_m5_2xlarge,
        TrainingInstanceType_Ml_m5_4xlarge,
        TrainingInstanceType_Ml_m5_large,
        TrainingInstanceType_Ml_m5_xlarge,
        TrainingInstanceType_Ml_p2_16xlarge,
        TrainingInstanceType_Ml_p2_8xlarge,
        TrainingInstanceType_Ml_p2_xlarge,
        TrainingInstanceType_Ml_p3_16xlarge,
        TrainingInstanceType_Ml_p3_2xlarge,
        TrainingInstanceType_Ml_p3_8xlarge,
        TrainingInstanceType_Ml_p3dn_24xlarge,
        TrainingInstanceType_Ml_p4d_24xlarge
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TrainingInstanceType = TrainingInstanceType'
  { fromTrainingInstanceType ::
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

pattern TrainingInstanceType_Ml_c4_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c4_2xlarge = TrainingInstanceType' "ml.c4.2xlarge"

pattern TrainingInstanceType_Ml_c4_4xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c4_4xlarge = TrainingInstanceType' "ml.c4.4xlarge"

pattern TrainingInstanceType_Ml_c4_8xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c4_8xlarge = TrainingInstanceType' "ml.c4.8xlarge"

pattern TrainingInstanceType_Ml_c4_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c4_xlarge = TrainingInstanceType' "ml.c4.xlarge"

pattern TrainingInstanceType_Ml_c5_18xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5_18xlarge = TrainingInstanceType' "ml.c5.18xlarge"

pattern TrainingInstanceType_Ml_c5_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5_2xlarge = TrainingInstanceType' "ml.c5.2xlarge"

pattern TrainingInstanceType_Ml_c5_4xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5_4xlarge = TrainingInstanceType' "ml.c5.4xlarge"

pattern TrainingInstanceType_Ml_c5_9xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5_9xlarge = TrainingInstanceType' "ml.c5.9xlarge"

pattern TrainingInstanceType_Ml_c5_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5_xlarge = TrainingInstanceType' "ml.c5.xlarge"

pattern TrainingInstanceType_Ml_c5n_18xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5n_18xlarge = TrainingInstanceType' "ml.c5n.18xlarge"

pattern TrainingInstanceType_Ml_c5n_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5n_2xlarge = TrainingInstanceType' "ml.c5n.2xlarge"

pattern TrainingInstanceType_Ml_c5n_4xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5n_4xlarge = TrainingInstanceType' "ml.c5n.4xlarge"

pattern TrainingInstanceType_Ml_c5n_9xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5n_9xlarge = TrainingInstanceType' "ml.c5n.9xlarge"

pattern TrainingInstanceType_Ml_c5n_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_c5n_xlarge = TrainingInstanceType' "ml.c5n.xlarge"

pattern TrainingInstanceType_Ml_g4dn_12xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_g4dn_12xlarge = TrainingInstanceType' "ml.g4dn.12xlarge"

pattern TrainingInstanceType_Ml_g4dn_16xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_g4dn_16xlarge = TrainingInstanceType' "ml.g4dn.16xlarge"

pattern TrainingInstanceType_Ml_g4dn_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_g4dn_2xlarge = TrainingInstanceType' "ml.g4dn.2xlarge"

pattern TrainingInstanceType_Ml_g4dn_4xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_g4dn_4xlarge = TrainingInstanceType' "ml.g4dn.4xlarge"

pattern TrainingInstanceType_Ml_g4dn_8xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_g4dn_8xlarge = TrainingInstanceType' "ml.g4dn.8xlarge"

pattern TrainingInstanceType_Ml_g4dn_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_g4dn_xlarge = TrainingInstanceType' "ml.g4dn.xlarge"

pattern TrainingInstanceType_Ml_m4_10xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m4_10xlarge = TrainingInstanceType' "ml.m4.10xlarge"

pattern TrainingInstanceType_Ml_m4_16xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m4_16xlarge = TrainingInstanceType' "ml.m4.16xlarge"

pattern TrainingInstanceType_Ml_m4_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m4_2xlarge = TrainingInstanceType' "ml.m4.2xlarge"

pattern TrainingInstanceType_Ml_m4_4xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m4_4xlarge = TrainingInstanceType' "ml.m4.4xlarge"

pattern TrainingInstanceType_Ml_m4_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m4_xlarge = TrainingInstanceType' "ml.m4.xlarge"

pattern TrainingInstanceType_Ml_m5_12xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m5_12xlarge = TrainingInstanceType' "ml.m5.12xlarge"

pattern TrainingInstanceType_Ml_m5_24xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m5_24xlarge = TrainingInstanceType' "ml.m5.24xlarge"

pattern TrainingInstanceType_Ml_m5_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m5_2xlarge = TrainingInstanceType' "ml.m5.2xlarge"

pattern TrainingInstanceType_Ml_m5_4xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m5_4xlarge = TrainingInstanceType' "ml.m5.4xlarge"

pattern TrainingInstanceType_Ml_m5_large :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m5_large = TrainingInstanceType' "ml.m5.large"

pattern TrainingInstanceType_Ml_m5_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_m5_xlarge = TrainingInstanceType' "ml.m5.xlarge"

pattern TrainingInstanceType_Ml_p2_16xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p2_16xlarge = TrainingInstanceType' "ml.p2.16xlarge"

pattern TrainingInstanceType_Ml_p2_8xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p2_8xlarge = TrainingInstanceType' "ml.p2.8xlarge"

pattern TrainingInstanceType_Ml_p2_xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p2_xlarge = TrainingInstanceType' "ml.p2.xlarge"

pattern TrainingInstanceType_Ml_p3_16xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p3_16xlarge = TrainingInstanceType' "ml.p3.16xlarge"

pattern TrainingInstanceType_Ml_p3_2xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p3_2xlarge = TrainingInstanceType' "ml.p3.2xlarge"

pattern TrainingInstanceType_Ml_p3_8xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p3_8xlarge = TrainingInstanceType' "ml.p3.8xlarge"

pattern TrainingInstanceType_Ml_p3dn_24xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p3dn_24xlarge = TrainingInstanceType' "ml.p3dn.24xlarge"

pattern TrainingInstanceType_Ml_p4d_24xlarge :: TrainingInstanceType
pattern TrainingInstanceType_Ml_p4d_24xlarge = TrainingInstanceType' "ml.p4d.24xlarge"

{-# COMPLETE
  TrainingInstanceType_Ml_c4_2xlarge,
  TrainingInstanceType_Ml_c4_4xlarge,
  TrainingInstanceType_Ml_c4_8xlarge,
  TrainingInstanceType_Ml_c4_xlarge,
  TrainingInstanceType_Ml_c5_18xlarge,
  TrainingInstanceType_Ml_c5_2xlarge,
  TrainingInstanceType_Ml_c5_4xlarge,
  TrainingInstanceType_Ml_c5_9xlarge,
  TrainingInstanceType_Ml_c5_xlarge,
  TrainingInstanceType_Ml_c5n_18xlarge,
  TrainingInstanceType_Ml_c5n_2xlarge,
  TrainingInstanceType_Ml_c5n_4xlarge,
  TrainingInstanceType_Ml_c5n_9xlarge,
  TrainingInstanceType_Ml_c5n_xlarge,
  TrainingInstanceType_Ml_g4dn_12xlarge,
  TrainingInstanceType_Ml_g4dn_16xlarge,
  TrainingInstanceType_Ml_g4dn_2xlarge,
  TrainingInstanceType_Ml_g4dn_4xlarge,
  TrainingInstanceType_Ml_g4dn_8xlarge,
  TrainingInstanceType_Ml_g4dn_xlarge,
  TrainingInstanceType_Ml_m4_10xlarge,
  TrainingInstanceType_Ml_m4_16xlarge,
  TrainingInstanceType_Ml_m4_2xlarge,
  TrainingInstanceType_Ml_m4_4xlarge,
  TrainingInstanceType_Ml_m4_xlarge,
  TrainingInstanceType_Ml_m5_12xlarge,
  TrainingInstanceType_Ml_m5_24xlarge,
  TrainingInstanceType_Ml_m5_2xlarge,
  TrainingInstanceType_Ml_m5_4xlarge,
  TrainingInstanceType_Ml_m5_large,
  TrainingInstanceType_Ml_m5_xlarge,
  TrainingInstanceType_Ml_p2_16xlarge,
  TrainingInstanceType_Ml_p2_8xlarge,
  TrainingInstanceType_Ml_p2_xlarge,
  TrainingInstanceType_Ml_p3_16xlarge,
  TrainingInstanceType_Ml_p3_2xlarge,
  TrainingInstanceType_Ml_p3_8xlarge,
  TrainingInstanceType_Ml_p3dn_24xlarge,
  TrainingInstanceType_Ml_p4d_24xlarge,
  TrainingInstanceType'
  #-}
