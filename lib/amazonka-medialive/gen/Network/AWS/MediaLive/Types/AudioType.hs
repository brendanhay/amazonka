{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioType
  ( AudioType
    ( AudioType'
    , AudioTypeCleanEffects
    , AudioTypeHearingImpaired
    , AudioTypeUndefined
    , AudioTypeVisualImpairedCommentary
    , fromAudioType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Audio Type
newtype AudioType = AudioType'{fromAudioType :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern AudioTypeCleanEffects :: AudioType
pattern AudioTypeCleanEffects = AudioType' "CLEAN_EFFECTS"

pattern AudioTypeHearingImpaired :: AudioType
pattern AudioTypeHearingImpaired = AudioType' "HEARING_IMPAIRED"

pattern AudioTypeUndefined :: AudioType
pattern AudioTypeUndefined = AudioType' "UNDEFINED"

pattern AudioTypeVisualImpairedCommentary :: AudioType
pattern AudioTypeVisualImpairedCommentary = AudioType' "VISUAL_IMPAIRED_COMMENTARY"

{-# COMPLETE 
  AudioTypeCleanEffects,

  AudioTypeHearingImpaired,

  AudioTypeUndefined,

  AudioTypeVisualImpairedCommentary,
  AudioType'
  #-}
