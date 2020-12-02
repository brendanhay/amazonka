{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TargetDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TargetDevice where

import Network.AWS.Prelude

data TargetDevice
  = Aisage
  | AmbaCV22
  | Coreml
  | Deeplens
  | Imx8qm
  | JetsonNano
  | JetsonTX1
  | JetsonTX2
  | JetsonXavier
  | Lambda
  | MlC4
  | MlC5
  | MlG4dn
  | MlINF1
  | MlM4
  | MlM5
  | MlP2
  | MlP3
  | QCS603
  | QCS605
  | RK3288
  | RK3399
  | Rasp3b
  | Sbec
  | SitaraAm57x
  | X86WIN32
  | X86WIN64
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText TargetDevice where
  parser =
    takeLowerText >>= \case
      "aisage" -> pure Aisage
      "amba_cv22" -> pure AmbaCV22
      "coreml" -> pure Coreml
      "deeplens" -> pure Deeplens
      "imx8qm" -> pure Imx8qm
      "jetson_nano" -> pure JetsonNano
      "jetson_tx1" -> pure JetsonTX1
      "jetson_tx2" -> pure JetsonTX2
      "jetson_xavier" -> pure JetsonXavier
      "lambda" -> pure Lambda
      "ml_c4" -> pure MlC4
      "ml_c5" -> pure MlC5
      "ml_g4dn" -> pure MlG4dn
      "ml_inf1" -> pure MlINF1
      "ml_m4" -> pure MlM4
      "ml_m5" -> pure MlM5
      "ml_p2" -> pure MlP2
      "ml_p3" -> pure MlP3
      "qcs603" -> pure QCS603
      "qcs605" -> pure QCS605
      "rk3288" -> pure RK3288
      "rk3399" -> pure RK3399
      "rasp3b" -> pure Rasp3b
      "sbe_c" -> pure Sbec
      "sitara_am57x" -> pure SitaraAm57x
      "x86_win32" -> pure X86WIN32
      "x86_win64" -> pure X86WIN64
      e ->
        fromTextError $
          "Failure parsing TargetDevice from value: '" <> e
            <> "'. Accepted values: aisage, amba_cv22, coreml, deeplens, imx8qm, jetson_nano, jetson_tx1, jetson_tx2, jetson_xavier, lambda, ml_c4, ml_c5, ml_g4dn, ml_inf1, ml_m4, ml_m5, ml_p2, ml_p3, qcs603, qcs605, rk3288, rk3399, rasp3b, sbe_c, sitara_am57x, x86_win32, x86_win64"

instance ToText TargetDevice where
  toText = \case
    Aisage -> "aisage"
    AmbaCV22 -> "amba_cv22"
    Coreml -> "coreml"
    Deeplens -> "deeplens"
    Imx8qm -> "imx8qm"
    JetsonNano -> "jetson_nano"
    JetsonTX1 -> "jetson_tx1"
    JetsonTX2 -> "jetson_tx2"
    JetsonXavier -> "jetson_xavier"
    Lambda -> "lambda"
    MlC4 -> "ml_c4"
    MlC5 -> "ml_c5"
    MlG4dn -> "ml_g4dn"
    MlINF1 -> "ml_inf1"
    MlM4 -> "ml_m4"
    MlM5 -> "ml_m5"
    MlP2 -> "ml_p2"
    MlP3 -> "ml_p3"
    QCS603 -> "qcs603"
    QCS605 -> "qcs605"
    RK3288 -> "rk3288"
    RK3399 -> "rk3399"
    Rasp3b -> "rasp3b"
    Sbec -> "sbe_c"
    SitaraAm57x -> "sitara_am57x"
    X86WIN32 -> "x86_win32"
    X86WIN64 -> "x86_win64"

instance Hashable TargetDevice

instance NFData TargetDevice

instance ToByteString TargetDevice

instance ToQuery TargetDevice

instance ToHeader TargetDevice

instance ToJSON TargetDevice where
  toJSON = toJSONText

instance FromJSON TargetDevice where
  parseJSON = parseJSONText "TargetDevice"
