{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3DrcProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3DrcProfile
  ( Ac3DrcProfile
      ( Ac3DrcProfile',
        Ac3DrcProfileFilmStandard,
        Ac3DrcProfileNone,
        fromAc3DrcProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Ac3 Drc Profile
newtype Ac3DrcProfile = Ac3DrcProfile'
  { fromAc3DrcProfile ::
      Core.Text
  }
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

pattern Ac3DrcProfileFilmStandard :: Ac3DrcProfile
pattern Ac3DrcProfileFilmStandard = Ac3DrcProfile' "FILM_STANDARD"

pattern Ac3DrcProfileNone :: Ac3DrcProfile
pattern Ac3DrcProfileNone = Ac3DrcProfile' "NONE"

{-# COMPLETE
  Ac3DrcProfileFilmStandard,
  Ac3DrcProfileNone,
  Ac3DrcProfile'
  #-}
