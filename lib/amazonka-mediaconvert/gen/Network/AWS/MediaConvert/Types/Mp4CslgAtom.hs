-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4CslgAtom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4CslgAtom
  ( Mp4CslgAtom
      ( Mp4CslgAtom',
        Mp4Exclude,
        Mp4Include
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype Mp4CslgAtom = Mp4CslgAtom' Lude.Text
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

pattern Mp4Exclude :: Mp4CslgAtom
pattern Mp4Exclude = Mp4CslgAtom' "EXCLUDE"

pattern Mp4Include :: Mp4CslgAtom
pattern Mp4Include = Mp4CslgAtom' "INCLUDE"

{-# COMPLETE
  Mp4Exclude,
  Mp4Include,
  Mp4CslgAtom'
  #-}
