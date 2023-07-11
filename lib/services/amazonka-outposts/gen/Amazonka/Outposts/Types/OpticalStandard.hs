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
-- Module      : Amazonka.Outposts.Types.OpticalStandard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.OpticalStandard
  ( OpticalStandard
      ( ..,
        OpticalStandard_OPTIC_1000BASE_LX,
        OpticalStandard_OPTIC_1000BASE_SX,
        OpticalStandard_OPTIC_100GBASE_CWDM4,
        OpticalStandard_OPTIC_100GBASE_LR4,
        OpticalStandard_OPTIC_100GBASE_SR4,
        OpticalStandard_OPTIC_100G_PSM4_MSA,
        OpticalStandard_OPTIC_10GBASE_IR,
        OpticalStandard_OPTIC_10GBASE_LR,
        OpticalStandard_OPTIC_10GBASE_SR,
        OpticalStandard_OPTIC_40GBASE_ESR,
        OpticalStandard_OPTIC_40GBASE_IR4_LR4L,
        OpticalStandard_OPTIC_40GBASE_LR4,
        OpticalStandard_OPTIC_40GBASE_SR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype OpticalStandard = OpticalStandard'
  { fromOpticalStandard ::
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

pattern OpticalStandard_OPTIC_1000BASE_LX :: OpticalStandard
pattern OpticalStandard_OPTIC_1000BASE_LX = OpticalStandard' "OPTIC_1000BASE_LX"

pattern OpticalStandard_OPTIC_1000BASE_SX :: OpticalStandard
pattern OpticalStandard_OPTIC_1000BASE_SX = OpticalStandard' "OPTIC_1000BASE_SX"

pattern OpticalStandard_OPTIC_100GBASE_CWDM4 :: OpticalStandard
pattern OpticalStandard_OPTIC_100GBASE_CWDM4 = OpticalStandard' "OPTIC_100GBASE_CWDM4"

pattern OpticalStandard_OPTIC_100GBASE_LR4 :: OpticalStandard
pattern OpticalStandard_OPTIC_100GBASE_LR4 = OpticalStandard' "OPTIC_100GBASE_LR4"

pattern OpticalStandard_OPTIC_100GBASE_SR4 :: OpticalStandard
pattern OpticalStandard_OPTIC_100GBASE_SR4 = OpticalStandard' "OPTIC_100GBASE_SR4"

pattern OpticalStandard_OPTIC_100G_PSM4_MSA :: OpticalStandard
pattern OpticalStandard_OPTIC_100G_PSM4_MSA = OpticalStandard' "OPTIC_100G_PSM4_MSA"

pattern OpticalStandard_OPTIC_10GBASE_IR :: OpticalStandard
pattern OpticalStandard_OPTIC_10GBASE_IR = OpticalStandard' "OPTIC_10GBASE_IR"

pattern OpticalStandard_OPTIC_10GBASE_LR :: OpticalStandard
pattern OpticalStandard_OPTIC_10GBASE_LR = OpticalStandard' "OPTIC_10GBASE_LR"

pattern OpticalStandard_OPTIC_10GBASE_SR :: OpticalStandard
pattern OpticalStandard_OPTIC_10GBASE_SR = OpticalStandard' "OPTIC_10GBASE_SR"

pattern OpticalStandard_OPTIC_40GBASE_ESR :: OpticalStandard
pattern OpticalStandard_OPTIC_40GBASE_ESR = OpticalStandard' "OPTIC_40GBASE_ESR"

pattern OpticalStandard_OPTIC_40GBASE_IR4_LR4L :: OpticalStandard
pattern OpticalStandard_OPTIC_40GBASE_IR4_LR4L = OpticalStandard' "OPTIC_40GBASE_IR4_LR4L"

pattern OpticalStandard_OPTIC_40GBASE_LR4 :: OpticalStandard
pattern OpticalStandard_OPTIC_40GBASE_LR4 = OpticalStandard' "OPTIC_40GBASE_LR4"

pattern OpticalStandard_OPTIC_40GBASE_SR :: OpticalStandard
pattern OpticalStandard_OPTIC_40GBASE_SR = OpticalStandard' "OPTIC_40GBASE_SR"

{-# COMPLETE
  OpticalStandard_OPTIC_1000BASE_LX,
  OpticalStandard_OPTIC_1000BASE_SX,
  OpticalStandard_OPTIC_100GBASE_CWDM4,
  OpticalStandard_OPTIC_100GBASE_LR4,
  OpticalStandard_OPTIC_100GBASE_SR4,
  OpticalStandard_OPTIC_100G_PSM4_MSA,
  OpticalStandard_OPTIC_10GBASE_IR,
  OpticalStandard_OPTIC_10GBASE_LR,
  OpticalStandard_OPTIC_10GBASE_SR,
  OpticalStandard_OPTIC_40GBASE_ESR,
  OpticalStandard_OPTIC_40GBASE_IR4_LR4L,
  OpticalStandard_OPTIC_40GBASE_LR4,
  OpticalStandard_OPTIC_40GBASE_SR,
  OpticalStandard'
  #-}
