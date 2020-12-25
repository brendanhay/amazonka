{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioChannelTag
  ( AudioChannelTag
      ( AudioChannelTag',
        AudioChannelTagL,
        AudioChannelTagR,
        AudioChannelTagC,
        AudioChannelTagLfe,
        AudioChannelTagLS,
        AudioChannelTagRS,
        AudioChannelTagLC,
        AudioChannelTagRC,
        AudioChannelTagCS,
        AudioChannelTagLsd,
        AudioChannelTagRsd,
        AudioChannelTagTcs,
        AudioChannelTagVhl,
        AudioChannelTagVhc,
        AudioChannelTagVhr,
        fromAudioChannelTag
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | You can add a tag for this mono-channel audio track to mimic its placement in a multi-channel layout.  For example, if this track is the left surround channel, choose Left surround (LS).
newtype AudioChannelTag = AudioChannelTag'
  { fromAudioChannelTag ::
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

pattern AudioChannelTagL :: AudioChannelTag
pattern AudioChannelTagL = AudioChannelTag' "L"

pattern AudioChannelTagR :: AudioChannelTag
pattern AudioChannelTagR = AudioChannelTag' "R"

pattern AudioChannelTagC :: AudioChannelTag
pattern AudioChannelTagC = AudioChannelTag' "C"

pattern AudioChannelTagLfe :: AudioChannelTag
pattern AudioChannelTagLfe = AudioChannelTag' "LFE"

pattern AudioChannelTagLS :: AudioChannelTag
pattern AudioChannelTagLS = AudioChannelTag' "LS"

pattern AudioChannelTagRS :: AudioChannelTag
pattern AudioChannelTagRS = AudioChannelTag' "RS"

pattern AudioChannelTagLC :: AudioChannelTag
pattern AudioChannelTagLC = AudioChannelTag' "LC"

pattern AudioChannelTagRC :: AudioChannelTag
pattern AudioChannelTagRC = AudioChannelTag' "RC"

pattern AudioChannelTagCS :: AudioChannelTag
pattern AudioChannelTagCS = AudioChannelTag' "CS"

pattern AudioChannelTagLsd :: AudioChannelTag
pattern AudioChannelTagLsd = AudioChannelTag' "LSD"

pattern AudioChannelTagRsd :: AudioChannelTag
pattern AudioChannelTagRsd = AudioChannelTag' "RSD"

pattern AudioChannelTagTcs :: AudioChannelTag
pattern AudioChannelTagTcs = AudioChannelTag' "TCS"

pattern AudioChannelTagVhl :: AudioChannelTag
pattern AudioChannelTagVhl = AudioChannelTag' "VHL"

pattern AudioChannelTagVhc :: AudioChannelTag
pattern AudioChannelTagVhc = AudioChannelTag' "VHC"

pattern AudioChannelTagVhr :: AudioChannelTag
pattern AudioChannelTagVhr = AudioChannelTag' "VHR"

{-# COMPLETE
  AudioChannelTagL,
  AudioChannelTagR,
  AudioChannelTagC,
  AudioChannelTagLfe,
  AudioChannelTagLS,
  AudioChannelTagRS,
  AudioChannelTagLC,
  AudioChannelTagRC,
  AudioChannelTagCS,
  AudioChannelTagLsd,
  AudioChannelTagRsd,
  AudioChannelTagTcs,
  AudioChannelTagVhl,
  AudioChannelTagVhc,
  AudioChannelTagVhr,
  AudioChannelTag'
  #-}
