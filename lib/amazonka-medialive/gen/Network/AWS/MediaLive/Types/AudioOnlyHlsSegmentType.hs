-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
  ( AudioOnlyHlsSegmentType
      ( AudioOnlyHlsSegmentType',
        Aac,
        FMP4
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Audio Only Hls Segment Type
newtype AudioOnlyHlsSegmentType = AudioOnlyHlsSegmentType' Lude.Text
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

pattern Aac :: AudioOnlyHlsSegmentType
pattern Aac = AudioOnlyHlsSegmentType' "AAC"

pattern FMP4 :: AudioOnlyHlsSegmentType
pattern FMP4 = AudioOnlyHlsSegmentType' "FMP4"

{-# COMPLETE
  Aac,
  FMP4,
  AudioOnlyHlsSegmentType'
  #-}
