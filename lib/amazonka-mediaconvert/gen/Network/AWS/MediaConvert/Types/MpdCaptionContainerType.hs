{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdCaptionContainerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdCaptionContainerType
  ( MpdCaptionContainerType
      ( MpdCaptionContainerType',
        MpdCaptionContainerTypeRaw,
        MpdCaptionContainerTypeFragmentedMP4,
        fromMpdCaptionContainerType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
newtype MpdCaptionContainerType = MpdCaptionContainerType'
  { fromMpdCaptionContainerType ::
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

pattern MpdCaptionContainerTypeRaw :: MpdCaptionContainerType
pattern MpdCaptionContainerTypeRaw = MpdCaptionContainerType' "RAW"

pattern MpdCaptionContainerTypeFragmentedMP4 :: MpdCaptionContainerType
pattern MpdCaptionContainerTypeFragmentedMP4 = MpdCaptionContainerType' "FRAGMENTED_MP4"

{-# COMPLETE
  MpdCaptionContainerTypeRaw,
  MpdCaptionContainerTypeFragmentedMP4,
  MpdCaptionContainerType'
  #-}
