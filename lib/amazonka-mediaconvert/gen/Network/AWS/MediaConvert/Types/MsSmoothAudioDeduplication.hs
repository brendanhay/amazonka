{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothAudioDeduplication
  ( MsSmoothAudioDeduplication
      ( MsSmoothAudioDeduplication',
        MsSmoothAudioDeduplicationCombineDuplicateStreams,
        MsSmoothAudioDeduplicationNone,
        fromMsSmoothAudioDeduplication
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
newtype MsSmoothAudioDeduplication = MsSmoothAudioDeduplication'
  { fromMsSmoothAudioDeduplication ::
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

pattern MsSmoothAudioDeduplicationCombineDuplicateStreams :: MsSmoothAudioDeduplication
pattern MsSmoothAudioDeduplicationCombineDuplicateStreams = MsSmoothAudioDeduplication' "COMBINE_DUPLICATE_STREAMS"

pattern MsSmoothAudioDeduplicationNone :: MsSmoothAudioDeduplication
pattern MsSmoothAudioDeduplicationNone = MsSmoothAudioDeduplication' "NONE"

{-# COMPLETE
  MsSmoothAudioDeduplicationCombineDuplicateStreams,
  MsSmoothAudioDeduplicationNone,
  MsSmoothAudioDeduplication'
  #-}
