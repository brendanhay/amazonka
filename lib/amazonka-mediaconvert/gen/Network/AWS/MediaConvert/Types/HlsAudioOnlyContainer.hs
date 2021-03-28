{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
  ( HlsAudioOnlyContainer
    ( HlsAudioOnlyContainer'
    , HlsAudioOnlyContainerAutomatic
    , HlsAudioOnlyContainerM2TS
    , fromHlsAudioOnlyContainer
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create a raw audio-only file with no container. Regardless of the value that you specify here, if this output has video, the service will place outputs into an MPEG2-TS container.
newtype HlsAudioOnlyContainer = HlsAudioOnlyContainer'{fromHlsAudioOnlyContainer
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern HlsAudioOnlyContainerAutomatic :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainerAutomatic = HlsAudioOnlyContainer' "AUTOMATIC"

pattern HlsAudioOnlyContainerM2TS :: HlsAudioOnlyContainer
pattern HlsAudioOnlyContainerM2TS = HlsAudioOnlyContainer' "M2TS"

{-# COMPLETE 
  HlsAudioOnlyContainerAutomatic,

  HlsAudioOnlyContainerM2TS,
  HlsAudioOnlyContainer'
  #-}
