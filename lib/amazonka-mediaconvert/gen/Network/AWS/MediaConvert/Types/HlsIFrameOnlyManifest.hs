{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
  ( HlsIFrameOnlyManifest
      ( HlsIFrameOnlyManifest',
        HlsIFrameOnlyManifestInclude,
        HlsIFrameOnlyManifestExclude,
        fromHlsIFrameOnlyManifest
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
newtype HlsIFrameOnlyManifest = HlsIFrameOnlyManifest'
  { fromHlsIFrameOnlyManifest ::
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

pattern HlsIFrameOnlyManifestInclude :: HlsIFrameOnlyManifest
pattern HlsIFrameOnlyManifestInclude = HlsIFrameOnlyManifest' "INCLUDE"

pattern HlsIFrameOnlyManifestExclude :: HlsIFrameOnlyManifest
pattern HlsIFrameOnlyManifestExclude = HlsIFrameOnlyManifest' "EXCLUDE"

{-# COMPLETE
  HlsIFrameOnlyManifestInclude,
  HlsIFrameOnlyManifestExclude,
  HlsIFrameOnlyManifest'
  #-}
