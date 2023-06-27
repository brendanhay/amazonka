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
-- Module      : Amazonka.MediaConvert.Types.DeinterlaceAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DeinterlaceAlgorithm
  ( DeinterlaceAlgorithm
      ( ..,
        DeinterlaceAlgorithm_BLEND,
        DeinterlaceAlgorithm_BLEND_TICKER,
        DeinterlaceAlgorithm_INTERPOLATE,
        DeinterlaceAlgorithm_INTERPOLATE_TICKER,
        DeinterlaceAlgorithm_LINEAR_INTERPOLATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Only applies when you set Deinterlace mode to Deinterlace or Adaptive.
-- Interpolate produces sharper pictures, while blend produces smoother
-- motion. If your source file includes a ticker, such as a scrolling
-- headline at the bottom of the frame: Choose Interpolate ticker or Blend
-- ticker. To apply field doubling: Choose Linear interpolation. Note that
-- Linear interpolation may introduce video artifacts into your output.
newtype DeinterlaceAlgorithm = DeinterlaceAlgorithm'
  { fromDeinterlaceAlgorithm ::
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

pattern DeinterlaceAlgorithm_BLEND :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_BLEND = DeinterlaceAlgorithm' "BLEND"

pattern DeinterlaceAlgorithm_BLEND_TICKER :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_BLEND_TICKER = DeinterlaceAlgorithm' "BLEND_TICKER"

pattern DeinterlaceAlgorithm_INTERPOLATE :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_INTERPOLATE = DeinterlaceAlgorithm' "INTERPOLATE"

pattern DeinterlaceAlgorithm_INTERPOLATE_TICKER :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_INTERPOLATE_TICKER = DeinterlaceAlgorithm' "INTERPOLATE_TICKER"

pattern DeinterlaceAlgorithm_LINEAR_INTERPOLATION :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_LINEAR_INTERPOLATION = DeinterlaceAlgorithm' "LINEAR_INTERPOLATION"

{-# COMPLETE
  DeinterlaceAlgorithm_BLEND,
  DeinterlaceAlgorithm_BLEND_TICKER,
  DeinterlaceAlgorithm_INTERPOLATE,
  DeinterlaceAlgorithm_INTERPOLATE_TICKER,
  DeinterlaceAlgorithm_LINEAR_INTERPOLATION,
  DeinterlaceAlgorithm'
  #-}
