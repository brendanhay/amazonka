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
-- Module      : Amazonka.MediaConvert.Types.BandwidthReductionFilterSharpening
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.BandwidthReductionFilterSharpening
  ( BandwidthReductionFilterSharpening
      ( ..,
        BandwidthReductionFilterSharpening_HIGH,
        BandwidthReductionFilterSharpening_LOW,
        BandwidthReductionFilterSharpening_MEDIUM,
        BandwidthReductionFilterSharpening_OFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Optionally specify the level of sharpening to apply when you use the
-- Bandwidth reduction filter. Sharpening adds contrast to the edges of
-- your video content and can reduce softness. Keep the default value Off
-- to apply no sharpening. Set Sharpening strength to Low to apply a
-- minimal amount of sharpening, or High to apply a maximum amount of
-- sharpening.
newtype BandwidthReductionFilterSharpening = BandwidthReductionFilterSharpening'
  { fromBandwidthReductionFilterSharpening ::
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

pattern BandwidthReductionFilterSharpening_HIGH :: BandwidthReductionFilterSharpening
pattern BandwidthReductionFilterSharpening_HIGH = BandwidthReductionFilterSharpening' "HIGH"

pattern BandwidthReductionFilterSharpening_LOW :: BandwidthReductionFilterSharpening
pattern BandwidthReductionFilterSharpening_LOW = BandwidthReductionFilterSharpening' "LOW"

pattern BandwidthReductionFilterSharpening_MEDIUM :: BandwidthReductionFilterSharpening
pattern BandwidthReductionFilterSharpening_MEDIUM = BandwidthReductionFilterSharpening' "MEDIUM"

pattern BandwidthReductionFilterSharpening_OFF :: BandwidthReductionFilterSharpening
pattern BandwidthReductionFilterSharpening_OFF = BandwidthReductionFilterSharpening' "OFF"

{-# COMPLETE
  BandwidthReductionFilterSharpening_HIGH,
  BandwidthReductionFilterSharpening_LOW,
  BandwidthReductionFilterSharpening_MEDIUM,
  BandwidthReductionFilterSharpening_OFF,
  BandwidthReductionFilterSharpening'
  #-}
