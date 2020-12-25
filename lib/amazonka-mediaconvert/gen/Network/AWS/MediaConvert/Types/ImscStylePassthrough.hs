{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ImscStylePassthrough
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ImscStylePassthrough
  ( ImscStylePassthrough
      ( ImscStylePassthrough',
        ImscStylePassthroughEnabled,
        ImscStylePassthroughDisabled,
        fromImscStylePassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Keep this setting enabled to have MediaConvert use the font style and position information from the captions source in the output. This option is available only when your input captions are IMSC, SMPTE-TT, or TTML. Disable this setting for simplified output captions.
newtype ImscStylePassthrough = ImscStylePassthrough'
  { fromImscStylePassthrough ::
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

pattern ImscStylePassthroughEnabled :: ImscStylePassthrough
pattern ImscStylePassthroughEnabled = ImscStylePassthrough' "ENABLED"

pattern ImscStylePassthroughDisabled :: ImscStylePassthrough
pattern ImscStylePassthroughDisabled = ImscStylePassthrough' "DISABLED"

{-# COMPLETE
  ImscStylePassthroughEnabled,
  ImscStylePassthroughDisabled,
  ImscStylePassthrough'
  #-}
