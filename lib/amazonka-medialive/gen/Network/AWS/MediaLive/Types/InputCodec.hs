-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputCodec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputCodec
  ( InputCodec
      ( InputCodec',
        Avc,
        Hevc,
        MPEG2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | codec in increasing order of complexity
newtype InputCodec = InputCodec' Lude.Text
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

pattern Avc :: InputCodec
pattern Avc = InputCodec' "AVC"

pattern Hevc :: InputCodec
pattern Hevc = InputCodec' "HEVC"

pattern MPEG2 :: InputCodec
pattern MPEG2 = InputCodec' "MPEG2"

{-# COMPLETE
  Avc,
  Hevc,
  MPEG2,
  InputCodec'
  #-}
