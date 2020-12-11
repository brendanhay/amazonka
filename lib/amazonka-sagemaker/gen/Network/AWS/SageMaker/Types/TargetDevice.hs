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
        Aisage,
        AmbaCV22,
        Coreml,
        Deeplens,
        Imx8qm,
        JetsonNano,
        JetsonTX1,
        JetsonTX2,
        JetsonXavier,
        Lambda,
        MlC4,
        MlC5,
        MlG4dn,
        MlINF1,
        MlM4,
        MlM5,
        MlP2,
        MlP3,
        QCS603,
        QCS605,
        RK3288,
        RK3399,
        Rasp3b,
        Sbec,
        SitaraAm57x,
        X86WIN32,
        X86WIN64
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

pattern Aisage :: TargetDevice
pattern Aisage = TargetDevice' "aisage"

pattern AmbaCV22 :: TargetDevice
pattern AmbaCV22 = TargetDevice' "amba_cv22"

pattern Coreml :: TargetDevice
pattern Coreml = TargetDevice' "coreml"

pattern Deeplens :: TargetDevice
pattern Deeplens = TargetDevice' "deeplens"

pattern Imx8qm :: TargetDevice
pattern Imx8qm = TargetDevice' "imx8qm"

pattern JetsonNano :: TargetDevice
pattern JetsonNano = TargetDevice' "jetson_nano"

pattern JetsonTX1 :: TargetDevice
pattern JetsonTX1 = TargetDevice' "jetson_tx1"

pattern JetsonTX2 :: TargetDevice
pattern JetsonTX2 = TargetDevice' "jetson_tx2"

pattern JetsonXavier :: TargetDevice
pattern JetsonXavier = TargetDevice' "jetson_xavier"

pattern Lambda :: TargetDevice
pattern Lambda = TargetDevice' "lambda"

pattern MlC4 :: TargetDevice
pattern MlC4 = TargetDevice' "ml_c4"

pattern MlC5 :: TargetDevice
pattern MlC5 = TargetDevice' "ml_c5"

pattern MlG4dn :: TargetDevice
pattern MlG4dn = TargetDevice' "ml_g4dn"

pattern MlINF1 :: TargetDevice
pattern MlINF1 = TargetDevice' "ml_inf1"

pattern MlM4 :: TargetDevice
pattern MlM4 = TargetDevice' "ml_m4"

pattern MlM5 :: TargetDevice
pattern MlM5 = TargetDevice' "ml_m5"

pattern MlP2 :: TargetDevice
pattern MlP2 = TargetDevice' "ml_p2"

pattern MlP3 :: TargetDevice
pattern MlP3 = TargetDevice' "ml_p3"

pattern QCS603 :: TargetDevice
pattern QCS603 = TargetDevice' "qcs603"

pattern QCS605 :: TargetDevice
pattern QCS605 = TargetDevice' "qcs605"

pattern RK3288 :: TargetDevice
pattern RK3288 = TargetDevice' "rk3288"

pattern RK3399 :: TargetDevice
pattern RK3399 = TargetDevice' "rk3399"

pattern Rasp3b :: TargetDevice
pattern Rasp3b = TargetDevice' "rasp3b"

pattern Sbec :: TargetDevice
pattern Sbec = TargetDevice' "sbe_c"

pattern SitaraAm57x :: TargetDevice
pattern SitaraAm57x = TargetDevice' "sitara_am57x"

pattern X86WIN32 :: TargetDevice
pattern X86WIN32 = TargetDevice' "x86_win32"

pattern X86WIN64 :: TargetDevice
pattern X86WIN64 = TargetDevice' "x86_win64"

{-# COMPLETE
  Aisage,
  AmbaCV22,
  Coreml,
  Deeplens,
  Imx8qm,
  JetsonNano,
  JetsonTX1,
  JetsonTX2,
  JetsonXavier,
  Lambda,
  MlC4,
  MlC5,
  MlG4dn,
  MlINF1,
  MlM4,
  MlM5,
  MlP2,
  MlP3,
  QCS603,
  QCS605,
  RK3288,
  RK3399,
  Rasp3b,
  Sbec,
  SitaraAm57x,
  X86WIN32,
  X86WIN64,
  TargetDevice'
  #-}
