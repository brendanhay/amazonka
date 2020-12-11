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
        HSCSegmentedFiles,
        HSCSingleFile
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to SINGLE_FILE, emits program as a single media resource (.ts) file, uses #EXT-X-BYTERANGE tags to index segment for playback.
newtype HlsSegmentControl = HlsSegmentControl' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern HSCSegmentedFiles :: HlsSegmentControl
pattern HSCSegmentedFiles = HlsSegmentControl' "SEGMENTED_FILES"

pattern HSCSingleFile :: HlsSegmentControl
pattern HSCSingleFile = HlsSegmentControl' "SINGLE_FILE"

{-# COMPLETE
  HSCSegmentedFiles,
  HSCSingleFile,
  HlsSegmentControl'
  #-}
