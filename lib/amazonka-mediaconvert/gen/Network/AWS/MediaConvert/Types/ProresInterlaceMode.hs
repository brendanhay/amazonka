-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresInterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresInterlaceMode
  ( ProresInterlaceMode
      ( ProresInterlaceMode',
        PIMBottomField,
        PIMFollowBottomField,
        PIMFollowTopField,
        PIMProgressive,
        PIMTopField
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
newtype ProresInterlaceMode = ProresInterlaceMode' Lude.Text
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

pattern PIMBottomField :: ProresInterlaceMode
pattern PIMBottomField = ProresInterlaceMode' "BOTTOM_FIELD"

pattern PIMFollowBottomField :: ProresInterlaceMode
pattern PIMFollowBottomField = ProresInterlaceMode' "FOLLOW_BOTTOM_FIELD"

pattern PIMFollowTopField :: ProresInterlaceMode
pattern PIMFollowTopField = ProresInterlaceMode' "FOLLOW_TOP_FIELD"

pattern PIMProgressive :: ProresInterlaceMode
pattern PIMProgressive = ProresInterlaceMode' "PROGRESSIVE"

pattern PIMTopField :: ProresInterlaceMode
pattern PIMTopField = ProresInterlaceMode' "TOP_FIELD"

{-# COMPLETE
  PIMBottomField,
  PIMFollowBottomField,
  PIMFollowTopField,
  PIMProgressive,
  PIMTopField,
  ProresInterlaceMode'
  #-}
