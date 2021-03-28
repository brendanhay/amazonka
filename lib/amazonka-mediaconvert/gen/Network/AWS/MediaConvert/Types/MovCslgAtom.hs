{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovCslgAtom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovCslgAtom
  ( MovCslgAtom
    ( MovCslgAtom'
    , MovCslgAtomInclude
    , MovCslgAtomExclude
    , fromMovCslgAtom
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
newtype MovCslgAtom = MovCslgAtom'{fromMovCslgAtom :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern MovCslgAtomInclude :: MovCslgAtom
pattern MovCslgAtomInclude = MovCslgAtom' "INCLUDE"

pattern MovCslgAtomExclude :: MovCslgAtom
pattern MovCslgAtomExclude = MovCslgAtom' "EXCLUDE"

{-# COMPLETE 
  MovCslgAtomInclude,

  MovCslgAtomExclude,
  MovCslgAtom'
  #-}
