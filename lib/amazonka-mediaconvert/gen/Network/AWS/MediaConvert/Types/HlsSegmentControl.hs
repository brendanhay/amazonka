{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsSegmentControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsSegmentControl
  ( HlsSegmentControl
      ( HlsSegmentControl',
        HlsSegmentControlSingleFile,
        HlsSegmentControlSegmentedFiles,
        fromHlsSegmentControl
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
newtype HlsSegmentControl = HlsSegmentControl'
  { fromHlsSegmentControl ::
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

pattern HlsSegmentControlSingleFile :: HlsSegmentControl
pattern HlsSegmentControlSingleFile = HlsSegmentControl' "SINGLE_FILE"

pattern HlsSegmentControlSegmentedFiles :: HlsSegmentControl
pattern HlsSegmentControlSegmentedFiles = HlsSegmentControl' "SEGMENTED_FILES"

{-# COMPLETE
  HlsSegmentControlSingleFile,
  HlsSegmentControlSegmentedFiles,
  HlsSegmentControl'
  #-}
