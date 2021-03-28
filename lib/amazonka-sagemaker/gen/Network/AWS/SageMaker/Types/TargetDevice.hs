{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TargetDevice
  ( TargetDevice
    ( TargetDevice'
    , TargetDeviceLambda
    , TargetDeviceMlM4
    , TargetDeviceMlM5
    , TargetDeviceMlC4
    , TargetDeviceMlC5
    , TargetDeviceMlP2
    , TargetDeviceMlP3
    , TargetDeviceMlG4dn
    , TargetDeviceMlINF1
    , TargetDeviceJetsonTX1
    , TargetDeviceJetsonTX2
    , TargetDeviceJetsonNano
    , TargetDeviceJetsonXavier
    , TargetDeviceRasp3b
    , TargetDeviceImx8qm
    , TargetDeviceDeeplens
    , TargetDeviceRK3399
    , TargetDeviceRK3288
    , TargetDeviceAisage
    , TargetDeviceSbec
    , TargetDeviceQCS605
    , TargetDeviceQCS603
    , TargetDeviceSitaraAm57x
    , TargetDeviceAmbaCV22
    , TargetDeviceX86WIN32
    , TargetDeviceX86WIN64
    , TargetDeviceCoreml
    , fromTargetDevice
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TargetDevice = TargetDevice'{fromTargetDevice :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern TargetDeviceLambda :: TargetDevice
pattern TargetDeviceLambda = TargetDevice' "lambda"

pattern TargetDeviceMlM4 :: TargetDevice
pattern TargetDeviceMlM4 = TargetDevice' "ml_m4"

pattern TargetDeviceMlM5 :: TargetDevice
pattern TargetDeviceMlM5 = TargetDevice' "ml_m5"

pattern TargetDeviceMlC4 :: TargetDevice
pattern TargetDeviceMlC4 = TargetDevice' "ml_c4"

pattern TargetDeviceMlC5 :: TargetDevice
pattern TargetDeviceMlC5 = TargetDevice' "ml_c5"

pattern TargetDeviceMlP2 :: TargetDevice
pattern TargetDeviceMlP2 = TargetDevice' "ml_p2"

pattern TargetDeviceMlP3 :: TargetDevice
pattern TargetDeviceMlP3 = TargetDevice' "ml_p3"

pattern TargetDeviceMlG4dn :: TargetDevice
pattern TargetDeviceMlG4dn = TargetDevice' "ml_g4dn"

pattern TargetDeviceMlINF1 :: TargetDevice
pattern TargetDeviceMlINF1 = TargetDevice' "ml_inf1"

pattern TargetDeviceJetsonTX1 :: TargetDevice
pattern TargetDeviceJetsonTX1 = TargetDevice' "jetson_tx1"

pattern TargetDeviceJetsonTX2 :: TargetDevice
pattern TargetDeviceJetsonTX2 = TargetDevice' "jetson_tx2"

pattern TargetDeviceJetsonNano :: TargetDevice
pattern TargetDeviceJetsonNano = TargetDevice' "jetson_nano"

pattern TargetDeviceJetsonXavier :: TargetDevice
pattern TargetDeviceJetsonXavier = TargetDevice' "jetson_xavier"

pattern TargetDeviceRasp3b :: TargetDevice
pattern TargetDeviceRasp3b = TargetDevice' "rasp3b"

pattern TargetDeviceImx8qm :: TargetDevice
pattern TargetDeviceImx8qm = TargetDevice' "imx8qm"

pattern TargetDeviceDeeplens :: TargetDevice
pattern TargetDeviceDeeplens = TargetDevice' "deeplens"

pattern TargetDeviceRK3399 :: TargetDevice
pattern TargetDeviceRK3399 = TargetDevice' "rk3399"

pattern TargetDeviceRK3288 :: TargetDevice
pattern TargetDeviceRK3288 = TargetDevice' "rk3288"

pattern TargetDeviceAisage :: TargetDevice
pattern TargetDeviceAisage = TargetDevice' "aisage"

pattern TargetDeviceSbec :: TargetDevice
pattern TargetDeviceSbec = TargetDevice' "sbe_c"

pattern TargetDeviceQCS605 :: TargetDevice
pattern TargetDeviceQCS605 = TargetDevice' "qcs605"

pattern TargetDeviceQCS603 :: TargetDevice
pattern TargetDeviceQCS603 = TargetDevice' "qcs603"

pattern TargetDeviceSitaraAm57x :: TargetDevice
pattern TargetDeviceSitaraAm57x = TargetDevice' "sitara_am57x"

pattern TargetDeviceAmbaCV22 :: TargetDevice
pattern TargetDeviceAmbaCV22 = TargetDevice' "amba_cv22"

pattern TargetDeviceX86WIN32 :: TargetDevice
pattern TargetDeviceX86WIN32 = TargetDevice' "x86_win32"

pattern TargetDeviceX86WIN64 :: TargetDevice
pattern TargetDeviceX86WIN64 = TargetDevice' "x86_win64"

pattern TargetDeviceCoreml :: TargetDevice
pattern TargetDeviceCoreml = TargetDevice' "coreml"

{-# COMPLETE 
  TargetDeviceLambda,

  TargetDeviceMlM4,

  TargetDeviceMlM5,

  TargetDeviceMlC4,

  TargetDeviceMlC5,

  TargetDeviceMlP2,

  TargetDeviceMlP3,

  TargetDeviceMlG4dn,

  TargetDeviceMlINF1,

  TargetDeviceJetsonTX1,

  TargetDeviceJetsonTX2,

  TargetDeviceJetsonNano,

  TargetDeviceJetsonXavier,

  TargetDeviceRasp3b,

  TargetDeviceImx8qm,

  TargetDeviceDeeplens,

  TargetDeviceRK3399,

  TargetDeviceRK3288,

  TargetDeviceAisage,

  TargetDeviceSbec,

  TargetDeviceQCS605,

  TargetDeviceQCS603,

  TargetDeviceSitaraAm57x,

  TargetDeviceAmbaCV22,

  TargetDeviceX86WIN32,

  TargetDeviceX86WIN64,

  TargetDeviceCoreml,
  TargetDevice'
  #-}
