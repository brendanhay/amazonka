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
-- Module      : Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DeinterlaceAlgorithm
  ( DeinterlaceAlgorithm
      ( ..,
        DeinterlaceAlgorithm_BLEND,
        DeinterlaceAlgorithm_BLEND_TICKER,
        DeinterlaceAlgorithm_INTERPOLATE,
        DeinterlaceAlgorithm_INTERPOLATE_TICKER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Only applies when you set Deinterlacer (DeinterlaceMode) to Deinterlace
-- (DEINTERLACE) or Adaptive (ADAPTIVE). Motion adaptive interpolate
-- (INTERPOLATE) produces sharper pictures, while blend (BLEND) produces
-- smoother motion. Use (INTERPOLATE_TICKER) OR (BLEND_TICKER) if your
-- source file includes a ticker, such as a scrolling headline at the
-- bottom of the frame.
newtype DeinterlaceAlgorithm = DeinterlaceAlgorithm'
  { fromDeinterlaceAlgorithm ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DeinterlaceAlgorithm_BLEND :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_BLEND = DeinterlaceAlgorithm' "BLEND"

pattern DeinterlaceAlgorithm_BLEND_TICKER :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_BLEND_TICKER = DeinterlaceAlgorithm' "BLEND_TICKER"

pattern DeinterlaceAlgorithm_INTERPOLATE :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_INTERPOLATE = DeinterlaceAlgorithm' "INTERPOLATE"

pattern DeinterlaceAlgorithm_INTERPOLATE_TICKER :: DeinterlaceAlgorithm
pattern DeinterlaceAlgorithm_INTERPOLATE_TICKER = DeinterlaceAlgorithm' "INTERPOLATE_TICKER"

{-# COMPLETE
  DeinterlaceAlgorithm_BLEND,
  DeinterlaceAlgorithm_BLEND_TICKER,
  DeinterlaceAlgorithm_INTERPOLATE,
  DeinterlaceAlgorithm_INTERPOLATE_TICKER,
  DeinterlaceAlgorithm'
  #-}
