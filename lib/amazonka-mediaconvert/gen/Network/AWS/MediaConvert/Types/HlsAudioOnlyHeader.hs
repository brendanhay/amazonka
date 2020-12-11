-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioOnlyHeader
  ( HlsAudioOnlyHeader
      ( HlsAudioOnlyHeader',
        HAOHExclude,
        HAOHInclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Ignore this setting unless you are using FairPlay DRM with Verimatrix and you encounter playback issues. Keep the default value, Include (INCLUDE), to output audio-only headers. Choose Exclude (EXCLUDE) to remove the audio-only headers from your audio segments.
newtype HlsAudioOnlyHeader = HlsAudioOnlyHeader' Lude.Text
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

pattern HAOHExclude :: HlsAudioOnlyHeader
pattern HAOHExclude = HlsAudioOnlyHeader' "EXCLUDE"

pattern HAOHInclude :: HlsAudioOnlyHeader
pattern HAOHInclude = HlsAudioOnlyHeader' "INCLUDE"

{-# COMPLETE
  HAOHExclude,
  HAOHInclude,
  HlsAudioOnlyHeader'
  #-}
