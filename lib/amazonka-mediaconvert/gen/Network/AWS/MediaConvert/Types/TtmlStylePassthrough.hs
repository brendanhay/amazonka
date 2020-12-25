{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.TtmlStylePassthrough
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TtmlStylePassthrough
  ( TtmlStylePassthrough
      ( TtmlStylePassthrough',
        TtmlStylePassthroughEnabled,
        TtmlStylePassthroughDisabled,
        fromTtmlStylePassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Pass through style and position information from a TTML-like input source (TTML, SMPTE-TT) to the TTML output.
newtype TtmlStylePassthrough = TtmlStylePassthrough'
  { fromTtmlStylePassthrough ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TtmlStylePassthroughEnabled :: TtmlStylePassthrough
pattern TtmlStylePassthroughEnabled = TtmlStylePassthrough' "ENABLED"

pattern TtmlStylePassthroughDisabled :: TtmlStylePassthrough
pattern TtmlStylePassthroughDisabled = TtmlStylePassthrough' "DISABLED"

{-# COMPLETE
  TtmlStylePassthroughEnabled,
  TtmlStylePassthroughDisabled,
  TtmlStylePassthrough'
  #-}
