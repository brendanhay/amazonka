-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovCslgAtom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MovCslgAtom
  ( MovCslgAtom
      ( MovCslgAtom',
        MCAExclude,
        MCAInclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype MovCslgAtom = MovCslgAtom' Lude.Text
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

pattern MCAExclude :: MovCslgAtom
pattern MCAExclude = MovCslgAtom' "EXCLUDE"

pattern MCAInclude :: MovCslgAtom
pattern MCAInclude = MovCslgAtom' "INCLUDE"

{-# COMPLETE
  MCAExclude,
  MCAInclude,
  MovCslgAtom'
  #-}
