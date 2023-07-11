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
-- Module      : Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpeningStrength
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.NoiseFilterPostTemporalSharpeningStrength
  ( NoiseFilterPostTemporalSharpeningStrength
      ( ..,
        NoiseFilterPostTemporalSharpeningStrength_HIGH,
        NoiseFilterPostTemporalSharpeningStrength_LOW,
        NoiseFilterPostTemporalSharpeningStrength_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use Post temporal sharpening strength (postTemporalSharpeningStrength)
-- to define the amount of sharpening the transcoder applies to your
-- output. Set Post temporal sharpening strength to Low (LOW), Medium
-- (MEDIUM), or High (HIGH) to indicate the amount of sharpening.
newtype NoiseFilterPostTemporalSharpeningStrength = NoiseFilterPostTemporalSharpeningStrength'
  { fromNoiseFilterPostTemporalSharpeningStrength ::
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

pattern NoiseFilterPostTemporalSharpeningStrength_HIGH :: NoiseFilterPostTemporalSharpeningStrength
pattern NoiseFilterPostTemporalSharpeningStrength_HIGH = NoiseFilterPostTemporalSharpeningStrength' "HIGH"

pattern NoiseFilterPostTemporalSharpeningStrength_LOW :: NoiseFilterPostTemporalSharpeningStrength
pattern NoiseFilterPostTemporalSharpeningStrength_LOW = NoiseFilterPostTemporalSharpeningStrength' "LOW"

pattern NoiseFilterPostTemporalSharpeningStrength_MEDIUM :: NoiseFilterPostTemporalSharpeningStrength
pattern NoiseFilterPostTemporalSharpeningStrength_MEDIUM = NoiseFilterPostTemporalSharpeningStrength' "MEDIUM"

{-# COMPLETE
  NoiseFilterPostTemporalSharpeningStrength_HIGH,
  NoiseFilterPostTemporalSharpeningStrength_LOW,
  NoiseFilterPostTemporalSharpeningStrength_MEDIUM,
  NoiseFilterPostTemporalSharpeningStrength'
  #-}
