{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingInstanceType
  ( TrainingInstanceType
      ( TrainingInstanceType',
        TrainingInstanceTypeMl_M4_Xlarge,
        TrainingInstanceTypeMl_M4_2xlarge,
        TrainingInstanceTypeMl_M4_4xlarge,
        TrainingInstanceTypeMl_M4_10xlarge,
        TrainingInstanceTypeMl_M4_16xlarge,
        TrainingInstanceTypeMl_G4dn_Xlarge,
        TrainingInstanceTypeMl_G4dn_2xlarge,
        TrainingInstanceTypeMl_G4dn_4xlarge,
        TrainingInstanceTypeMl_G4dn_8xlarge,
        TrainingInstanceTypeMl_G4dn_12xlarge,
        TrainingInstanceTypeMl_G4dn_16xlarge,
        TrainingInstanceTypeMl_M5_Large,
        TrainingInstanceTypeMl_M5_Xlarge,
        TrainingInstanceTypeMl_M5_2xlarge,
        TrainingInstanceTypeMl_M5_4xlarge,
        TrainingInstanceTypeMl_M5_12xlarge,
        TrainingInstanceTypeMl_M5_24xlarge,
        TrainingInstanceTypeMl_C4_Xlarge,
        TrainingInstanceTypeMl_C4_2xlarge,
        TrainingInstanceTypeMl_C4_4xlarge,
        TrainingInstanceTypeMl_C4_8xlarge,
        TrainingInstanceTypeMl_P2_Xlarge,
        TrainingInstanceTypeMl_P2_8xlarge,
        TrainingInstanceTypeMl_P2_16xlarge,
        TrainingInstanceTypeMl_P3_2xlarge,
        TrainingInstanceTypeMl_P3_8xlarge,
        TrainingInstanceTypeMl_P3_16xlarge,
        TrainingInstanceTypeMl_P3dn_24xlarge,
        TrainingInstanceTypeMl_P4d_24xlarge,
        TrainingInstanceTypeMl_C5_Xlarge,
        TrainingInstanceTypeMl_C5_2xlarge,
        TrainingInstanceTypeMl_C5_4xlarge,
        TrainingInstanceTypeMl_C5_9xlarge,
        TrainingInstanceTypeMl_C5_18xlarge,
        TrainingInstanceTypeMl_C5n_Xlarge,
        TrainingInstanceTypeMl_C5n_2xlarge,
        TrainingInstanceTypeMl_C5n_4xlarge,
        TrainingInstanceTypeMl_C5n_9xlarge,
        TrainingInstanceTypeMl_C5n_18xlarge,
        fromTrainingInstanceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TrainingInstanceType = TrainingInstanceType'
  { fromTrainingInstanceType ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TrainingInstanceTypeMl_M4_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M4_Xlarge = TrainingInstanceType' "ml.m4.xlarge"

pattern TrainingInstanceTypeMl_M4_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M4_2xlarge = TrainingInstanceType' "ml.m4.2xlarge"

pattern TrainingInstanceTypeMl_M4_4xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M4_4xlarge = TrainingInstanceType' "ml.m4.4xlarge"

pattern TrainingInstanceTypeMl_M4_10xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M4_10xlarge = TrainingInstanceType' "ml.m4.10xlarge"

pattern TrainingInstanceTypeMl_M4_16xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M4_16xlarge = TrainingInstanceType' "ml.m4.16xlarge"

pattern TrainingInstanceTypeMl_G4dn_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_G4dn_Xlarge = TrainingInstanceType' "ml.g4dn.xlarge"

pattern TrainingInstanceTypeMl_G4dn_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_G4dn_2xlarge = TrainingInstanceType' "ml.g4dn.2xlarge"

pattern TrainingInstanceTypeMl_G4dn_4xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_G4dn_4xlarge = TrainingInstanceType' "ml.g4dn.4xlarge"

pattern TrainingInstanceTypeMl_G4dn_8xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_G4dn_8xlarge = TrainingInstanceType' "ml.g4dn.8xlarge"

pattern TrainingInstanceTypeMl_G4dn_12xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_G4dn_12xlarge = TrainingInstanceType' "ml.g4dn.12xlarge"

pattern TrainingInstanceTypeMl_G4dn_16xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_G4dn_16xlarge = TrainingInstanceType' "ml.g4dn.16xlarge"

pattern TrainingInstanceTypeMl_M5_Large :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M5_Large = TrainingInstanceType' "ml.m5.large"

pattern TrainingInstanceTypeMl_M5_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M5_Xlarge = TrainingInstanceType' "ml.m5.xlarge"

pattern TrainingInstanceTypeMl_M5_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M5_2xlarge = TrainingInstanceType' "ml.m5.2xlarge"

pattern TrainingInstanceTypeMl_M5_4xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M5_4xlarge = TrainingInstanceType' "ml.m5.4xlarge"

pattern TrainingInstanceTypeMl_M5_12xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M5_12xlarge = TrainingInstanceType' "ml.m5.12xlarge"

pattern TrainingInstanceTypeMl_M5_24xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_M5_24xlarge = TrainingInstanceType' "ml.m5.24xlarge"

pattern TrainingInstanceTypeMl_C4_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C4_Xlarge = TrainingInstanceType' "ml.c4.xlarge"

pattern TrainingInstanceTypeMl_C4_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C4_2xlarge = TrainingInstanceType' "ml.c4.2xlarge"

pattern TrainingInstanceTypeMl_C4_4xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C4_4xlarge = TrainingInstanceType' "ml.c4.4xlarge"

pattern TrainingInstanceTypeMl_C4_8xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C4_8xlarge = TrainingInstanceType' "ml.c4.8xlarge"

pattern TrainingInstanceTypeMl_P2_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P2_Xlarge = TrainingInstanceType' "ml.p2.xlarge"

pattern TrainingInstanceTypeMl_P2_8xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P2_8xlarge = TrainingInstanceType' "ml.p2.8xlarge"

pattern TrainingInstanceTypeMl_P2_16xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P2_16xlarge = TrainingInstanceType' "ml.p2.16xlarge"

pattern TrainingInstanceTypeMl_P3_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P3_2xlarge = TrainingInstanceType' "ml.p3.2xlarge"

pattern TrainingInstanceTypeMl_P3_8xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P3_8xlarge = TrainingInstanceType' "ml.p3.8xlarge"

pattern TrainingInstanceTypeMl_P3_16xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P3_16xlarge = TrainingInstanceType' "ml.p3.16xlarge"

pattern TrainingInstanceTypeMl_P3dn_24xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P3dn_24xlarge = TrainingInstanceType' "ml.p3dn.24xlarge"

pattern TrainingInstanceTypeMl_P4d_24xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_P4d_24xlarge = TrainingInstanceType' "ml.p4d.24xlarge"

pattern TrainingInstanceTypeMl_C5_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5_Xlarge = TrainingInstanceType' "ml.c5.xlarge"

pattern TrainingInstanceTypeMl_C5_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5_2xlarge = TrainingInstanceType' "ml.c5.2xlarge"

pattern TrainingInstanceTypeMl_C5_4xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5_4xlarge = TrainingInstanceType' "ml.c5.4xlarge"

pattern TrainingInstanceTypeMl_C5_9xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5_9xlarge = TrainingInstanceType' "ml.c5.9xlarge"

pattern TrainingInstanceTypeMl_C5_18xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5_18xlarge = TrainingInstanceType' "ml.c5.18xlarge"

pattern TrainingInstanceTypeMl_C5n_Xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5n_Xlarge = TrainingInstanceType' "ml.c5n.xlarge"

pattern TrainingInstanceTypeMl_C5n_2xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5n_2xlarge = TrainingInstanceType' "ml.c5n.2xlarge"

pattern TrainingInstanceTypeMl_C5n_4xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5n_4xlarge = TrainingInstanceType' "ml.c5n.4xlarge"

pattern TrainingInstanceTypeMl_C5n_9xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5n_9xlarge = TrainingInstanceType' "ml.c5n.9xlarge"

pattern TrainingInstanceTypeMl_C5n_18xlarge :: TrainingInstanceType
pattern TrainingInstanceTypeMl_C5n_18xlarge = TrainingInstanceType' "ml.c5n.18xlarge"

{-# COMPLETE
  TrainingInstanceTypeMl_M4_Xlarge,
  TrainingInstanceTypeMl_M4_2xlarge,
  TrainingInstanceTypeMl_M4_4xlarge,
  TrainingInstanceTypeMl_M4_10xlarge,
  TrainingInstanceTypeMl_M4_16xlarge,
  TrainingInstanceTypeMl_G4dn_Xlarge,
  TrainingInstanceTypeMl_G4dn_2xlarge,
  TrainingInstanceTypeMl_G4dn_4xlarge,
  TrainingInstanceTypeMl_G4dn_8xlarge,
  TrainingInstanceTypeMl_G4dn_12xlarge,
  TrainingInstanceTypeMl_G4dn_16xlarge,
  TrainingInstanceTypeMl_M5_Large,
  TrainingInstanceTypeMl_M5_Xlarge,
  TrainingInstanceTypeMl_M5_2xlarge,
  TrainingInstanceTypeMl_M5_4xlarge,
  TrainingInstanceTypeMl_M5_12xlarge,
  TrainingInstanceTypeMl_M5_24xlarge,
  TrainingInstanceTypeMl_C4_Xlarge,
  TrainingInstanceTypeMl_C4_2xlarge,
  TrainingInstanceTypeMl_C4_4xlarge,
  TrainingInstanceTypeMl_C4_8xlarge,
  TrainingInstanceTypeMl_P2_Xlarge,
  TrainingInstanceTypeMl_P2_8xlarge,
  TrainingInstanceTypeMl_P2_16xlarge,
  TrainingInstanceTypeMl_P3_2xlarge,
  TrainingInstanceTypeMl_P3_8xlarge,
  TrainingInstanceTypeMl_P3_16xlarge,
  TrainingInstanceTypeMl_P3dn_24xlarge,
  TrainingInstanceTypeMl_P4d_24xlarge,
  TrainingInstanceTypeMl_C5_Xlarge,
  TrainingInstanceTypeMl_C5_2xlarge,
  TrainingInstanceTypeMl_C5_4xlarge,
  TrainingInstanceTypeMl_C5_9xlarge,
  TrainingInstanceTypeMl_C5_18xlarge,
  TrainingInstanceTypeMl_C5n_Xlarge,
  TrainingInstanceTypeMl_C5n_2xlarge,
  TrainingInstanceTypeMl_C5n_4xlarge,
  TrainingInstanceTypeMl_C5n_9xlarge,
  TrainingInstanceTypeMl_C5n_18xlarge,
  TrainingInstanceType'
  #-}
