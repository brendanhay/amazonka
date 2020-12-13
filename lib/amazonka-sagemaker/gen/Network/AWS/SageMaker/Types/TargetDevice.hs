{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetDevice
  ( TargetDevice
      ( TargetDevice',
        Lambda,
        MlM4,
        MlM5,
        MlC4,
        MlC5,
        MlP2,
        MlP3,
        MlG4dn,
        MlINF1,
        JetsonTX1,
        JetsonTX2,
        JetsonNano,
        JetsonXavier,
        Rasp3b,
        Imx8qm,
        Deeplens,
        RK3399,
        RK3288,
        Aisage,
        Sbec,
        QCS605,
        QCS603,
        SitaraAm57x,
        AmbaCV22,
        X86WIN32,
        X86WIN64,
        Coreml
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetDevice = TargetDevice' Lude.Text
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

pattern Lambda :: TargetDevice
pattern Lambda = TargetDevice' "lambda"

pattern MlM4 :: TargetDevice
pattern MlM4 = TargetDevice' "ml_m4"

pattern MlM5 :: TargetDevice
pattern MlM5 = TargetDevice' "ml_m5"

pattern MlC4 :: TargetDevice
pattern MlC4 = TargetDevice' "ml_c4"

pattern MlC5 :: TargetDevice
pattern MlC5 = TargetDevice' "ml_c5"

pattern MlP2 :: TargetDevice
pattern MlP2 = TargetDevice' "ml_p2"

pattern MlP3 :: TargetDevice
pattern MlP3 = TargetDevice' "ml_p3"

pattern MlG4dn :: TargetDevice
pattern MlG4dn = TargetDevice' "ml_g4dn"

pattern MlINF1 :: TargetDevice
pattern MlINF1 = TargetDevice' "ml_inf1"

pattern JetsonTX1 :: TargetDevice
pattern JetsonTX1 = TargetDevice' "jetson_tx1"

pattern JetsonTX2 :: TargetDevice
pattern JetsonTX2 = TargetDevice' "jetson_tx2"

pattern JetsonNano :: TargetDevice
pattern JetsonNano = TargetDevice' "jetson_nano"

pattern JetsonXavier :: TargetDevice
pattern JetsonXavier = TargetDevice' "jetson_xavier"

pattern Rasp3b :: TargetDevice
pattern Rasp3b = TargetDevice' "rasp3b"

pattern Imx8qm :: TargetDevice
pattern Imx8qm = TargetDevice' "imx8qm"

pattern Deeplens :: TargetDevice
pattern Deeplens = TargetDevice' "deeplens"

pattern RK3399 :: TargetDevice
pattern RK3399 = TargetDevice' "rk3399"

pattern RK3288 :: TargetDevice
pattern RK3288 = TargetDevice' "rk3288"

pattern Aisage :: TargetDevice
pattern Aisage = TargetDevice' "aisage"

pattern Sbec :: TargetDevice
pattern Sbec = TargetDevice' "sbe_c"

pattern QCS605 :: TargetDevice
pattern QCS605 = TargetDevice' "qcs605"

pattern QCS603 :: TargetDevice
pattern QCS603 = TargetDevice' "qcs603"

pattern SitaraAm57x :: TargetDevice
pattern SitaraAm57x = TargetDevice' "sitara_am57x"

pattern AmbaCV22 :: TargetDevice
pattern AmbaCV22 = TargetDevice' "amba_cv22"

pattern X86WIN32 :: TargetDevice
pattern X86WIN32 = TargetDevice' "x86_win32"

pattern X86WIN64 :: TargetDevice
pattern X86WIN64 = TargetDevice' "x86_win64"

pattern Coreml :: TargetDevice
pattern Coreml = TargetDevice' "coreml"

{-# COMPLETE
  Lambda,
  MlM4,
  MlM5,
  MlC4,
  MlC5,
  MlP2,
  MlP3,
  MlG4dn,
  MlINF1,
  JetsonTX1,
  JetsonTX2,
  JetsonNano,
  JetsonXavier,
  Rasp3b,
  Imx8qm,
  Deeplens,
  RK3399,
  RK3288,
  Aisage,
  Sbec,
  QCS605,
  QCS603,
  SitaraAm57x,
  AmbaCV22,
  X86WIN32,
  X86WIN64,
  Coreml,
  TargetDevice'
  #-}
