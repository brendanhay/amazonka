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
        ADPFilmStandard,
        ADPNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Ac3 Drc Profile
newtype Ac3DrcProfile = Ac3DrcProfile' Lude.Text
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

pattern ADPFilmStandard :: Ac3DrcProfile
pattern ADPFilmStandard = Ac3DrcProfile' "FILM_STANDARD"

pattern ADPNone :: Ac3DrcProfile
pattern ADPNone = Ac3DrcProfile' "NONE"

{-# COMPLETE
  ADPFilmStandard,
  ADPNone,
  Ac3DrcProfile'
  #-}
