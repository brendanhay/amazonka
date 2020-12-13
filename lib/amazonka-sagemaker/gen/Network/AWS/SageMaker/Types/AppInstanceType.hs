{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppInstanceType
  ( AppInstanceType
      ( AppInstanceType',
        System,
        Ml_T3_Micro,
        Ml_T3_Small,
        Ml_T3_Medium,
        Ml_T3_Large,
        Ml_T3_XLarge,
        Ml_T3_2XLarge,
        Ml_M5_Large,
        Ml_M5_XLarge,
        Ml_M5_2XLarge,
        Ml_M5_4XLarge,
        Ml_M5_8XLarge,
        Ml_M5_12XLarge,
        Ml_M5_16XLarge,
        Ml_M5_24XLarge,
        Ml_C5_Large,
        Ml_C5_XLarge,
        Ml_C5_2XLarge,
        Ml_C5_4XLarge,
        Ml_C5_9XLarge,
        Ml_C5_12XLarge,
        Ml_C5_18XLarge,
        Ml_C5_24XLarge,
        Ml_P3_2XLarge,
        Ml_P3_8XLarge,
        Ml_P3_16XLarge,
        Ml_G4dn_XLarge,
        Ml_G4dn_2XLarge,
        Ml_G4dn_4XLarge,
        Ml_G4dn_8XLarge,
        Ml_G4dn_12XLarge,
        Ml_G4dn_16XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AppInstanceType = AppInstanceType' Lude.Text
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

pattern System :: AppInstanceType
pattern System = AppInstanceType' "system"

pattern Ml_T3_Micro :: AppInstanceType
pattern Ml_T3_Micro = AppInstanceType' "ml.t3.micro"

pattern Ml_T3_Small :: AppInstanceType
pattern Ml_T3_Small = AppInstanceType' "ml.t3.small"

pattern Ml_T3_Medium :: AppInstanceType
pattern Ml_T3_Medium = AppInstanceType' "ml.t3.medium"

pattern Ml_T3_Large :: AppInstanceType
pattern Ml_T3_Large = AppInstanceType' "ml.t3.large"

pattern Ml_T3_XLarge :: AppInstanceType
pattern Ml_T3_XLarge = AppInstanceType' "ml.t3.xlarge"

pattern Ml_T3_2XLarge :: AppInstanceType
pattern Ml_T3_2XLarge = AppInstanceType' "ml.t3.2xlarge"

pattern Ml_M5_Large :: AppInstanceType
pattern Ml_M5_Large = AppInstanceType' "ml.m5.large"

pattern Ml_M5_XLarge :: AppInstanceType
pattern Ml_M5_XLarge = AppInstanceType' "ml.m5.xlarge"

pattern Ml_M5_2XLarge :: AppInstanceType
pattern Ml_M5_2XLarge = AppInstanceType' "ml.m5.2xlarge"

pattern Ml_M5_4XLarge :: AppInstanceType
pattern Ml_M5_4XLarge = AppInstanceType' "ml.m5.4xlarge"

pattern Ml_M5_8XLarge :: AppInstanceType
pattern Ml_M5_8XLarge = AppInstanceType' "ml.m5.8xlarge"

pattern Ml_M5_12XLarge :: AppInstanceType
pattern Ml_M5_12XLarge = AppInstanceType' "ml.m5.12xlarge"

pattern Ml_M5_16XLarge :: AppInstanceType
pattern Ml_M5_16XLarge = AppInstanceType' "ml.m5.16xlarge"

pattern Ml_M5_24XLarge :: AppInstanceType
pattern Ml_M5_24XLarge = AppInstanceType' "ml.m5.24xlarge"

pattern Ml_C5_Large :: AppInstanceType
pattern Ml_C5_Large = AppInstanceType' "ml.c5.large"

pattern Ml_C5_XLarge :: AppInstanceType
pattern Ml_C5_XLarge = AppInstanceType' "ml.c5.xlarge"

pattern Ml_C5_2XLarge :: AppInstanceType
pattern Ml_C5_2XLarge = AppInstanceType' "ml.c5.2xlarge"

pattern Ml_C5_4XLarge :: AppInstanceType
pattern Ml_C5_4XLarge = AppInstanceType' "ml.c5.4xlarge"

pattern Ml_C5_9XLarge :: AppInstanceType
pattern Ml_C5_9XLarge = AppInstanceType' "ml.c5.9xlarge"

pattern Ml_C5_12XLarge :: AppInstanceType
pattern Ml_C5_12XLarge = AppInstanceType' "ml.c5.12xlarge"

pattern Ml_C5_18XLarge :: AppInstanceType
pattern Ml_C5_18XLarge = AppInstanceType' "ml.c5.18xlarge"

pattern Ml_C5_24XLarge :: AppInstanceType
pattern Ml_C5_24XLarge = AppInstanceType' "ml.c5.24xlarge"

pattern Ml_P3_2XLarge :: AppInstanceType
pattern Ml_P3_2XLarge = AppInstanceType' "ml.p3.2xlarge"

pattern Ml_P3_8XLarge :: AppInstanceType
pattern Ml_P3_8XLarge = AppInstanceType' "ml.p3.8xlarge"

pattern Ml_P3_16XLarge :: AppInstanceType
pattern Ml_P3_16XLarge = AppInstanceType' "ml.p3.16xlarge"

pattern Ml_G4dn_XLarge :: AppInstanceType
pattern Ml_G4dn_XLarge = AppInstanceType' "ml.g4dn.xlarge"

pattern Ml_G4dn_2XLarge :: AppInstanceType
pattern Ml_G4dn_2XLarge = AppInstanceType' "ml.g4dn.2xlarge"

pattern Ml_G4dn_4XLarge :: AppInstanceType
pattern Ml_G4dn_4XLarge = AppInstanceType' "ml.g4dn.4xlarge"

pattern Ml_G4dn_8XLarge :: AppInstanceType
pattern Ml_G4dn_8XLarge = AppInstanceType' "ml.g4dn.8xlarge"

pattern Ml_G4dn_12XLarge :: AppInstanceType
pattern Ml_G4dn_12XLarge = AppInstanceType' "ml.g4dn.12xlarge"

pattern Ml_G4dn_16XLarge :: AppInstanceType
pattern Ml_G4dn_16XLarge = AppInstanceType' "ml.g4dn.16xlarge"

{-# COMPLETE
  System,
  Ml_T3_Micro,
  Ml_T3_Small,
  Ml_T3_Medium,
  Ml_T3_Large,
  Ml_T3_XLarge,
  Ml_T3_2XLarge,
  Ml_M5_Large,
  Ml_M5_XLarge,
  Ml_M5_2XLarge,
  Ml_M5_4XLarge,
  Ml_M5_8XLarge,
  Ml_M5_12XLarge,
  Ml_M5_16XLarge,
  Ml_M5_24XLarge,
  Ml_C5_Large,
  Ml_C5_XLarge,
  Ml_C5_2XLarge,
  Ml_C5_4XLarge,
  Ml_C5_9XLarge,
  Ml_C5_12XLarge,
  Ml_C5_18XLarge,
  Ml_C5_24XLarge,
  Ml_P3_2XLarge,
  Ml_P3_8XLarge,
  Ml_P3_16XLarge,
  Ml_G4dn_XLarge,
  Ml_G4dn_2XLarge,
  Ml_G4dn_4XLarge,
  Ml_G4dn_8XLarge,
  Ml_G4dn_12XLarge,
  Ml_G4dn_16XLarge,
  AppInstanceType'
  #-}
