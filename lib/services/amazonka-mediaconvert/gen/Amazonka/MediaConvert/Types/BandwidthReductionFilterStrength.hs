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
-- Module      : Amazonka.MediaConvert.Types.BandwidthReductionFilterStrength
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BandwidthReductionFilterStrength
  ( BandwidthReductionFilterStrength
      ( ..,
        BandwidthReductionFilterStrength_AUTO,
        BandwidthReductionFilterStrength_HIGH,
        BandwidthReductionFilterStrength_LOW,
        BandwidthReductionFilterStrength_MEDIUM,
        BandwidthReductionFilterStrength_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the strength of the Bandwidth reduction filter. For most
-- workflows, we recommend that you choose Auto to reduce the bandwidth of
-- your output with little to no perceptual decrease in video quality. For
-- high quality and high bitrate outputs, choose Low. For the most
-- bandwidth reduction, choose High. We recommend that you choose High for
-- low bitrate outputs. Note that High may incur a slight increase in the
-- softness of your output.
newtype BandwidthReductionFilterStrength = BandwidthReductionFilterStrength'
  { fromBandwidthReductionFilterStrength ::
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

pattern BandwidthReductionFilterStrength_AUTO :: BandwidthReductionFilterStrength
pattern BandwidthReductionFilterStrength_AUTO = BandwidthReductionFilterStrength' "AUTO"

pattern BandwidthReductionFilterStrength_HIGH :: BandwidthReductionFilterStrength
pattern BandwidthReductionFilterStrength_HIGH = BandwidthReductionFilterStrength' "HIGH"

pattern BandwidthReductionFilterStrength_LOW :: BandwidthReductionFilterStrength
pattern BandwidthReductionFilterStrength_LOW = BandwidthReductionFilterStrength' "LOW"

pattern BandwidthReductionFilterStrength_MEDIUM :: BandwidthReductionFilterStrength
pattern BandwidthReductionFilterStrength_MEDIUM = BandwidthReductionFilterStrength' "MEDIUM"

pattern BandwidthReductionFilterStrength_OFF :: BandwidthReductionFilterStrength
pattern BandwidthReductionFilterStrength_OFF = BandwidthReductionFilterStrength' "OFF"

{-# COMPLETE
  BandwidthReductionFilterStrength_AUTO,
  BandwidthReductionFilterStrength_HIGH,
  BandwidthReductionFilterStrength_LOW,
  BandwidthReductionFilterStrength_MEDIUM,
  BandwidthReductionFilterStrength_OFF,
  BandwidthReductionFilterStrength'
  #-}
