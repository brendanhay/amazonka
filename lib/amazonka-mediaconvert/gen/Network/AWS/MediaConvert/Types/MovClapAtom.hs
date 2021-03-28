{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovClapAtom
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovClapAtom
  ( MovClapAtom
    ( MovClapAtom'
    , MovClapAtomInclude
    , MovClapAtomExclude
    , fromMovClapAtom
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When enabled, include 'clap' atom if appropriate for the video output settings.
newtype MovClapAtom = MovClapAtom'{fromMovClapAtom :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern MovClapAtomInclude :: MovClapAtom
pattern MovClapAtomInclude = MovClapAtom' "INCLUDE"

pattern MovClapAtomExclude :: MovClapAtom
pattern MovClapAtomExclude = MovClapAtom' "EXCLUDE"

{-# COMPLETE 
  MovClapAtomInclude,

  MovClapAtomExclude,
  MovClapAtom'
  #-}
