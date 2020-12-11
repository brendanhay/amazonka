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
        MSADCombineDuplicateStreams,
        MSADNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | COMBINE_DUPLICATE_STREAMS combines identical audio encoding settings across a Microsoft Smooth output group into a single audio stream.
newtype MsSmoothAudioDeduplication = MsSmoothAudioDeduplication' Lude.Text
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

pattern MSADCombineDuplicateStreams :: MsSmoothAudioDeduplication
pattern MSADCombineDuplicateStreams = MsSmoothAudioDeduplication' "COMBINE_DUPLICATE_STREAMS"

pattern MSADNone :: MsSmoothAudioDeduplication
pattern MSADNone = MsSmoothAudioDeduplication' "NONE"

{-# COMPLETE
  MSADCombineDuplicateStreams,
  MSADNone,
  MsSmoothAudioDeduplication'
  #-}
