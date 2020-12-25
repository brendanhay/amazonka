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
        Mp4CslgAtomInclude,
        Mp4CslgAtomExclude,
        fromMp4CslgAtom
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype Mp4CslgAtom = Mp4CslgAtom' {fromMp4CslgAtom :: Core.Text}
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

pattern Mp4CslgAtomInclude :: Mp4CslgAtom
pattern Mp4CslgAtomInclude = Mp4CslgAtom' "INCLUDE"

pattern Mp4CslgAtomExclude :: Mp4CslgAtom
pattern Mp4CslgAtomExclude = Mp4CslgAtom' "EXCLUDE"

{-# COMPLETE
  Mp4CslgAtomInclude,
  Mp4CslgAtomExclude,
  Mp4CslgAtom'
  #-}
