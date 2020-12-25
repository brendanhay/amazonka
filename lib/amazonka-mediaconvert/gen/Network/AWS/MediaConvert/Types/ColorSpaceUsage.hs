{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorSpaceUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ColorSpaceUsage
  ( ColorSpaceUsage
      ( ColorSpaceUsage',
        ColorSpaceUsageForce,
        ColorSpaceUsageFallback,
        fromColorSpaceUsage
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | There are two sources for color metadata, the input file and the job input settings Color space (ColorSpace) and HDR master display information settings(Hdr10Metadata). The Color space usage setting determines which takes precedence. Choose Force (FORCE) to use color metadata from the input job settings. If you don't specify values for those settings, the service defaults to using metadata from your input. FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the source when it is present. If there's no color metadata in your input file, the service defaults to using values you specify in the input settings.
newtype ColorSpaceUsage = ColorSpaceUsage'
  { fromColorSpaceUsage ::
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

pattern ColorSpaceUsageForce :: ColorSpaceUsage
pattern ColorSpaceUsageForce = ColorSpaceUsage' "FORCE"

pattern ColorSpaceUsageFallback :: ColorSpaceUsage
pattern ColorSpaceUsageFallback = ColorSpaceUsage' "FALLBACK"

{-# COMPLETE
  ColorSpaceUsageForce,
  ColorSpaceUsageFallback,
  ColorSpaceUsage'
  #-}
