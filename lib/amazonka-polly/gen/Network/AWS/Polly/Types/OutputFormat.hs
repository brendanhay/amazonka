-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.OutputFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.OutputFormat
  ( OutputFormat
      ( OutputFormat',
        JSON,
        MP3,
        OggVorbis,
        Pcm
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OutputFormat = OutputFormat' Lude.Text
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

pattern JSON :: OutputFormat
pattern JSON = OutputFormat' "json"

pattern MP3 :: OutputFormat
pattern MP3 = OutputFormat' "mp3"

pattern OggVorbis :: OutputFormat
pattern OggVorbis = OutputFormat' "ogg_vorbis"

pattern Pcm :: OutputFormat
pattern Pcm = OutputFormat' "pcm"

{-# COMPLETE
  JSON,
  MP3,
  OggVorbis,
  Pcm,
  OutputFormat'
  #-}
