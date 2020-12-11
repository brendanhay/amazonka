-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
  ( HlsAudioOnlyContainer
      ( HlsAudioOnlyContainer',
        HAOCAutomatic,
        HAOCM2TS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create a raw audio-only file with no container. Regardless of the value that you specify here, if this output has video, the service will place outputs into an MPEG2-TS container.
newtype HlsAudioOnlyContainer = HlsAudioOnlyContainer' Lude.Text
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

pattern HAOCAutomatic :: HlsAudioOnlyContainer
pattern HAOCAutomatic = HlsAudioOnlyContainer' "AUTOMATIC"

pattern HAOCM2TS :: HlsAudioOnlyContainer
pattern HAOCM2TS = HlsAudioOnlyContainer' "M2TS"

{-# COMPLETE
  HAOCAutomatic,
  HAOCM2TS,
  HlsAudioOnlyContainer'
  #-}
