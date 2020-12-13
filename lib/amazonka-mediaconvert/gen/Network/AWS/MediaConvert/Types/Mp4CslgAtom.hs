{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        MCAfInclude,
        MCAfExclude
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

pattern MCAfInclude :: Mp4CslgAtom
pattern MCAfInclude = Mp4CslgAtom' "INCLUDE"

pattern MCAfExclude :: Mp4CslgAtom
pattern MCAfExclude = Mp4CslgAtom' "EXCLUDE"

{-# COMPLETE
  MCAfInclude,
  MCAfExclude,
  Mp4CslgAtom'
  #-}
