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
        TITMl_C4_2XLarge,
        TITMl_C4_4XLarge,
        TITMl_C4_8XLarge,
        TITMl_C4_XLarge,
        TITMl_C5_18XLarge,
        TITMl_C5_2XLarge,
        TITMl_C5_4XLarge,
        TITMl_C5_9XLarge,
        TITMl_C5_XLarge,
        TITMl_C5n_18XLarge,
        TITMl_C5n_2XLarge,
        TITMl_C5n_4XLarge,
        TITMl_C5n_9XLarge,
        TITMl_C5n_XLarge,
        TITMl_G4dn_12XLarge,
        TITMl_G4dn_16XLarge,
        TITMl_G4dn_2XLarge,
        TITMl_G4dn_4XLarge,
        TITMl_G4dn_8XLarge,
        TITMl_G4dn_XLarge,
        TITMl_M4_10XLarge,
        TITMl_M4_16XLarge,
        TITMl_M4_2XLarge,
        TITMl_M4_4XLarge,
        TITMl_M4_XLarge,
        TITMl_M5_12XLarge,
        TITMl_M5_24XLarge,
        TITMl_M5_2XLarge,
        TITMl_M5_4XLarge,
        TITMl_M5_Large,
        TITMl_M5_XLarge,
        TITMl_P2_16XLarge,
        TITMl_P2_8XLarge,
        TITMl_P2_XLarge,
        TITMl_P3_16XLarge,
        TITMl_P3_2XLarge,
        TITMl_P3_8XLarge,
        TITMl_P3dn_24XLarge,
        TITMl_P4d_24XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrainingInstanceType = TrainingInstanceType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern TITMl_C4_2XLarge :: TrainingInstanceType
pattern TITMl_C4_2XLarge = TrainingInstanceType' "ml.c4.2xlarge"

pattern TITMl_C4_4XLarge :: TrainingInstanceType
pattern TITMl_C4_4XLarge = TrainingInstanceType' "ml.c4.4xlarge"

pattern TITMl_C4_8XLarge :: TrainingInstanceType
pattern TITMl_C4_8XLarge = TrainingInstanceType' "ml.c4.8xlarge"

pattern TITMl_C4_XLarge :: TrainingInstanceType
pattern TITMl_C4_XLarge = TrainingInstanceType' "ml.c4.xlarge"

pattern TITMl_C5_18XLarge :: TrainingInstanceType
pattern TITMl_C5_18XLarge = TrainingInstanceType' "ml.c5.18xlarge"

pattern TITMl_C5_2XLarge :: TrainingInstanceType
pattern TITMl_C5_2XLarge = TrainingInstanceType' "ml.c5.2xlarge"

pattern TITMl_C5_4XLarge :: TrainingInstanceType
pattern TITMl_C5_4XLarge = TrainingInstanceType' "ml.c5.4xlarge"

pattern TITMl_C5_9XLarge :: TrainingInstanceType
pattern TITMl_C5_9XLarge = TrainingInstanceType' "ml.c5.9xlarge"

pattern TITMl_C5_XLarge :: TrainingInstanceType
pattern TITMl_C5_XLarge = TrainingInstanceType' "ml.c5.xlarge"

pattern TITMl_C5n_18XLarge :: TrainingInstanceType
pattern TITMl_C5n_18XLarge = TrainingInstanceType' "ml.c5n.18xlarge"

pattern TITMl_C5n_2XLarge :: TrainingInstanceType
pattern TITMl_C5n_2XLarge = TrainingInstanceType' "ml.c5n.2xlarge"

pattern TITMl_C5n_4XLarge :: TrainingInstanceType
pattern TITMl_C5n_4XLarge = TrainingInstanceType' "ml.c5n.4xlarge"

pattern TITMl_C5n_9XLarge :: TrainingInstanceType
pattern TITMl_C5n_9XLarge = TrainingInstanceType' "ml.c5n.9xlarge"

pattern TITMl_C5n_XLarge :: TrainingInstanceType
pattern TITMl_C5n_XLarge = TrainingInstanceType' "ml.c5n.xlarge"

pattern TITMl_G4dn_12XLarge :: TrainingInstanceType
pattern TITMl_G4dn_12XLarge = TrainingInstanceType' "ml.g4dn.12xlarge"

pattern TITMl_G4dn_16XLarge :: TrainingInstanceType
pattern TITMl_G4dn_16XLarge = TrainingInstanceType' "ml.g4dn.16xlarge"

pattern TITMl_G4dn_2XLarge :: TrainingInstanceType
pattern TITMl_G4dn_2XLarge = TrainingInstanceType' "ml.g4dn.2xlarge"

pattern TITMl_G4dn_4XLarge :: TrainingInstanceType
pattern TITMl_G4dn_4XLarge = TrainingInstanceType' "ml.g4dn.4xlarge"

pattern TITMl_G4dn_8XLarge :: TrainingInstanceType
pattern TITMl_G4dn_8XLarge = TrainingInstanceType' "ml.g4dn.8xlarge"

pattern TITMl_G4dn_XLarge :: TrainingInstanceType
pattern TITMl_G4dn_XLarge = TrainingInstanceType' "ml.g4dn.xlarge"

pattern TITMl_M4_10XLarge :: TrainingInstanceType
pattern TITMl_M4_10XLarge = TrainingInstanceType' "ml.m4.10xlarge"

pattern TITMl_M4_16XLarge :: TrainingInstanceType
pattern TITMl_M4_16XLarge = TrainingInstanceType' "ml.m4.16xlarge"

pattern TITMl_M4_2XLarge :: TrainingInstanceType
pattern TITMl_M4_2XLarge = TrainingInstanceType' "ml.m4.2xlarge"

pattern TITMl_M4_4XLarge :: TrainingInstanceType
pattern TITMl_M4_4XLarge = TrainingInstanceType' "ml.m4.4xlarge"

pattern TITMl_M4_XLarge :: TrainingInstanceType
pattern TITMl_M4_XLarge = TrainingInstanceType' "ml.m4.xlarge"

pattern TITMl_M5_12XLarge :: TrainingInstanceType
pattern TITMl_M5_12XLarge = TrainingInstanceType' "ml.m5.12xlarge"

pattern TITMl_M5_24XLarge :: TrainingInstanceType
pattern TITMl_M5_24XLarge = TrainingInstanceType' "ml.m5.24xlarge"

pattern TITMl_M5_2XLarge :: TrainingInstanceType
pattern TITMl_M5_2XLarge = TrainingInstanceType' "ml.m5.2xlarge"

pattern TITMl_M5_4XLarge :: TrainingInstanceType
pattern TITMl_M5_4XLarge = TrainingInstanceType' "ml.m5.4xlarge"

pattern TITMl_M5_Large :: TrainingInstanceType
pattern TITMl_M5_Large = TrainingInstanceType' "ml.m5.large"

pattern TITMl_M5_XLarge :: TrainingInstanceType
pattern TITMl_M5_XLarge = TrainingInstanceType' "ml.m5.xlarge"

pattern TITMl_P2_16XLarge :: TrainingInstanceType
pattern TITMl_P2_16XLarge = TrainingInstanceType' "ml.p2.16xlarge"

pattern TITMl_P2_8XLarge :: TrainingInstanceType
pattern TITMl_P2_8XLarge = TrainingInstanceType' "ml.p2.8xlarge"

pattern TITMl_P2_XLarge :: TrainingInstanceType
pattern TITMl_P2_XLarge = TrainingInstanceType' "ml.p2.xlarge"

pattern TITMl_P3_16XLarge :: TrainingInstanceType
pattern TITMl_P3_16XLarge = TrainingInstanceType' "ml.p3.16xlarge"

pattern TITMl_P3_2XLarge :: TrainingInstanceType
pattern TITMl_P3_2XLarge = TrainingInstanceType' "ml.p3.2xlarge"

pattern TITMl_P3_8XLarge :: TrainingInstanceType
pattern TITMl_P3_8XLarge = TrainingInstanceType' "ml.p3.8xlarge"

pattern TITMl_P3dn_24XLarge :: TrainingInstanceType
pattern TITMl_P3dn_24XLarge = TrainingInstanceType' "ml.p3dn.24xlarge"

pattern TITMl_P4d_24XLarge :: TrainingInstanceType
pattern TITMl_P4d_24XLarge = TrainingInstanceType' "ml.p4d.24xlarge"

{-# COMPLETE
  TITMl_C4_2XLarge,
  TITMl_C4_4XLarge,
  TITMl_C4_8XLarge,
  TITMl_C4_XLarge,
  TITMl_C5_18XLarge,
  TITMl_C5_2XLarge,
  TITMl_C5_4XLarge,
  TITMl_C5_9XLarge,
  TITMl_C5_XLarge,
  TITMl_C5n_18XLarge,
  TITMl_C5n_2XLarge,
  TITMl_C5n_4XLarge,
  TITMl_C5n_9XLarge,
  TITMl_C5n_XLarge,
  TITMl_G4dn_12XLarge,
  TITMl_G4dn_16XLarge,
  TITMl_G4dn_2XLarge,
  TITMl_G4dn_4XLarge,
  TITMl_G4dn_8XLarge,
  TITMl_G4dn_XLarge,
  TITMl_M4_10XLarge,
  TITMl_M4_16XLarge,
  TITMl_M4_2XLarge,
  TITMl_M4_4XLarge,
  TITMl_M4_XLarge,
  TITMl_M5_12XLarge,
  TITMl_M5_24XLarge,
  TITMl_M5_2XLarge,
  TITMl_M5_4XLarge,
  TITMl_M5_Large,
  TITMl_M5_XLarge,
  TITMl_P2_16XLarge,
  TITMl_P2_8XLarge,
  TITMl_P2_XLarge,
  TITMl_P3_16XLarge,
  TITMl_P3_2XLarge,
  TITMl_P3_8XLarge,
  TITMl_P3dn_24XLarge,
  TITMl_P4d_24XLarge,
  TrainingInstanceType'
  #-}
