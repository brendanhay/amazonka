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
-- Module      : Amazonka.SageMaker.Types.TargetDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TargetDevice
  ( TargetDevice
      ( ..,
        TargetDevice_Aisage,
        TargetDevice_Amba_cv2,
        TargetDevice_Amba_cv22,
        TargetDevice_Amba_cv25,
        TargetDevice_Coreml,
        TargetDevice_Deeplens,
        TargetDevice_Imx8mplus,
        TargetDevice_Imx8qm,
        TargetDevice_Jacinto_tda4vm,
        TargetDevice_Jetson_nano,
        TargetDevice_Jetson_tx1,
        TargetDevice_Jetson_tx2,
        TargetDevice_Jetson_xavier,
        TargetDevice_Lambda,
        TargetDevice_Ml_c4,
        TargetDevice_Ml_c5,
        TargetDevice_Ml_eia2,
        TargetDevice_Ml_g4dn,
        TargetDevice_Ml_inf1,
        TargetDevice_Ml_m4,
        TargetDevice_Ml_m5,
        TargetDevice_Ml_p2,
        TargetDevice_Ml_p3,
        TargetDevice_Qcs603,
        TargetDevice_Qcs605,
        TargetDevice_Rasp3b,
        TargetDevice_Rk3288,
        TargetDevice_Rk3399,
        TargetDevice_Sbe_c,
        TargetDevice_Sitara_am57x,
        TargetDevice_X86_win32,
        TargetDevice_X86_win64
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetDevice = TargetDevice'
  { fromTargetDevice ::
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

pattern TargetDevice_Aisage :: TargetDevice
pattern TargetDevice_Aisage = TargetDevice' "aisage"

pattern TargetDevice_Amba_cv2 :: TargetDevice
pattern TargetDevice_Amba_cv2 = TargetDevice' "amba_cv2"

pattern TargetDevice_Amba_cv22 :: TargetDevice
pattern TargetDevice_Amba_cv22 = TargetDevice' "amba_cv22"

pattern TargetDevice_Amba_cv25 :: TargetDevice
pattern TargetDevice_Amba_cv25 = TargetDevice' "amba_cv25"

pattern TargetDevice_Coreml :: TargetDevice
pattern TargetDevice_Coreml = TargetDevice' "coreml"

pattern TargetDevice_Deeplens :: TargetDevice
pattern TargetDevice_Deeplens = TargetDevice' "deeplens"

pattern TargetDevice_Imx8mplus :: TargetDevice
pattern TargetDevice_Imx8mplus = TargetDevice' "imx8mplus"

pattern TargetDevice_Imx8qm :: TargetDevice
pattern TargetDevice_Imx8qm = TargetDevice' "imx8qm"

pattern TargetDevice_Jacinto_tda4vm :: TargetDevice
pattern TargetDevice_Jacinto_tda4vm = TargetDevice' "jacinto_tda4vm"

pattern TargetDevice_Jetson_nano :: TargetDevice
pattern TargetDevice_Jetson_nano = TargetDevice' "jetson_nano"

pattern TargetDevice_Jetson_tx1 :: TargetDevice
pattern TargetDevice_Jetson_tx1 = TargetDevice' "jetson_tx1"

pattern TargetDevice_Jetson_tx2 :: TargetDevice
pattern TargetDevice_Jetson_tx2 = TargetDevice' "jetson_tx2"

pattern TargetDevice_Jetson_xavier :: TargetDevice
pattern TargetDevice_Jetson_xavier = TargetDevice' "jetson_xavier"

pattern TargetDevice_Lambda :: TargetDevice
pattern TargetDevice_Lambda = TargetDevice' "lambda"

pattern TargetDevice_Ml_c4 :: TargetDevice
pattern TargetDevice_Ml_c4 = TargetDevice' "ml_c4"

pattern TargetDevice_Ml_c5 :: TargetDevice
pattern TargetDevice_Ml_c5 = TargetDevice' "ml_c5"

pattern TargetDevice_Ml_eia2 :: TargetDevice
pattern TargetDevice_Ml_eia2 = TargetDevice' "ml_eia2"

pattern TargetDevice_Ml_g4dn :: TargetDevice
pattern TargetDevice_Ml_g4dn = TargetDevice' "ml_g4dn"

pattern TargetDevice_Ml_inf1 :: TargetDevice
pattern TargetDevice_Ml_inf1 = TargetDevice' "ml_inf1"

pattern TargetDevice_Ml_m4 :: TargetDevice
pattern TargetDevice_Ml_m4 = TargetDevice' "ml_m4"

pattern TargetDevice_Ml_m5 :: TargetDevice
pattern TargetDevice_Ml_m5 = TargetDevice' "ml_m5"

pattern TargetDevice_Ml_p2 :: TargetDevice
pattern TargetDevice_Ml_p2 = TargetDevice' "ml_p2"

pattern TargetDevice_Ml_p3 :: TargetDevice
pattern TargetDevice_Ml_p3 = TargetDevice' "ml_p3"

pattern TargetDevice_Qcs603 :: TargetDevice
pattern TargetDevice_Qcs603 = TargetDevice' "qcs603"

pattern TargetDevice_Qcs605 :: TargetDevice
pattern TargetDevice_Qcs605 = TargetDevice' "qcs605"

pattern TargetDevice_Rasp3b :: TargetDevice
pattern TargetDevice_Rasp3b = TargetDevice' "rasp3b"

pattern TargetDevice_Rk3288 :: TargetDevice
pattern TargetDevice_Rk3288 = TargetDevice' "rk3288"

pattern TargetDevice_Rk3399 :: TargetDevice
pattern TargetDevice_Rk3399 = TargetDevice' "rk3399"

pattern TargetDevice_Sbe_c :: TargetDevice
pattern TargetDevice_Sbe_c = TargetDevice' "sbe_c"

pattern TargetDevice_Sitara_am57x :: TargetDevice
pattern TargetDevice_Sitara_am57x = TargetDevice' "sitara_am57x"

pattern TargetDevice_X86_win32 :: TargetDevice
pattern TargetDevice_X86_win32 = TargetDevice' "x86_win32"

pattern TargetDevice_X86_win64 :: TargetDevice
pattern TargetDevice_X86_win64 = TargetDevice' "x86_win64"

{-# COMPLETE
  TargetDevice_Aisage,
  TargetDevice_Amba_cv2,
  TargetDevice_Amba_cv22,
  TargetDevice_Amba_cv25,
  TargetDevice_Coreml,
  TargetDevice_Deeplens,
  TargetDevice_Imx8mplus,
  TargetDevice_Imx8qm,
  TargetDevice_Jacinto_tda4vm,
  TargetDevice_Jetson_nano,
  TargetDevice_Jetson_tx1,
  TargetDevice_Jetson_tx2,
  TargetDevice_Jetson_xavier,
  TargetDevice_Lambda,
  TargetDevice_Ml_c4,
  TargetDevice_Ml_c5,
  TargetDevice_Ml_eia2,
  TargetDevice_Ml_g4dn,
  TargetDevice_Ml_inf1,
  TargetDevice_Ml_m4,
  TargetDevice_Ml_m5,
  TargetDevice_Ml_p2,
  TargetDevice_Ml_p3,
  TargetDevice_Qcs603,
  TargetDevice_Qcs605,
  TargetDevice_Rasp3b,
  TargetDevice_Rk3288,
  TargetDevice_Rk3399,
  TargetDevice_Sbe_c,
  TargetDevice_Sitara_am57x,
  TargetDevice_X86_win32,
  TargetDevice_X86_win64,
  TargetDevice'
  #-}
