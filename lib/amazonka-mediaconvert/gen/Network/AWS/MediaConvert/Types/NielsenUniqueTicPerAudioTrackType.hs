{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NielsenUniqueTicPerAudioTrackType
  ( NielsenUniqueTicPerAudioTrackType
      ( NielsenUniqueTicPerAudioTrackType',
        NielsenUniqueTicPerAudioTrackTypeReserveUniqueTicsPerTrack,
        NielsenUniqueTicPerAudioTrackTypeSameTicsPerTrack,
        fromNielsenUniqueTicPerAudioTrackType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | To create assets that have the same TIC values in each audio track, keep the default value Share TICs (SAME_TICS_PER_TRACK). To create assets that have unique TIC values for each audio track, choose Use unique TICs (RESERVE_UNIQUE_TICS_PER_TRACK).
newtype NielsenUniqueTicPerAudioTrackType = NielsenUniqueTicPerAudioTrackType'
  { fromNielsenUniqueTicPerAudioTrackType ::
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

pattern NielsenUniqueTicPerAudioTrackTypeReserveUniqueTicsPerTrack :: NielsenUniqueTicPerAudioTrackType
pattern NielsenUniqueTicPerAudioTrackTypeReserveUniqueTicsPerTrack = NielsenUniqueTicPerAudioTrackType' "RESERVE_UNIQUE_TICS_PER_TRACK"

pattern NielsenUniqueTicPerAudioTrackTypeSameTicsPerTrack :: NielsenUniqueTicPerAudioTrackType
pattern NielsenUniqueTicPerAudioTrackTypeSameTicsPerTrack = NielsenUniqueTicPerAudioTrackType' "SAME_TICS_PER_TRACK"

{-# COMPLETE
  NielsenUniqueTicPerAudioTrackTypeReserveUniqueTicsPerTrack,
  NielsenUniqueTicPerAudioTrackTypeSameTicsPerTrack,
  NielsenUniqueTicPerAudioTrackType'
  #-}
