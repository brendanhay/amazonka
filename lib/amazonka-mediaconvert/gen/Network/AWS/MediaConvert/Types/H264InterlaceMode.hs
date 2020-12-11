-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264InterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264InterlaceMode
  ( H264InterlaceMode
      ( H264InterlaceMode',
        HIMBottomField,
        HIMFollowBottomField,
        HIMFollowTopField,
        HIMProgressive,
        HIMTopField
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
newtype H264InterlaceMode = H264InterlaceMode' Lude.Text
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

pattern HIMBottomField :: H264InterlaceMode
pattern HIMBottomField = H264InterlaceMode' "BOTTOM_FIELD"

pattern HIMFollowBottomField :: H264InterlaceMode
pattern HIMFollowBottomField = H264InterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern HIMFollowTopField :: H264InterlaceMode
pattern HIMFollowTopField = H264InterlaceMode' "FOLLOW_TOP_FIELD"

pattern HIMProgressive :: H264InterlaceMode
pattern HIMProgressive = H264InterlaceMode' "PROGRESSIVE"

pattern HIMTopField :: H264InterlaceMode
pattern HIMTopField = H264InterlaceMode' "TOP_FIELD"

{-# COMPLETE
  HIMBottomField,
  HIMFollowBottomField,
  HIMFollowTopField,
  HIMProgressive,
  HIMTopField,
  H264InterlaceMode'
  #-}
