-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3DynamicRangeCompressionProfile
  ( Ac3DynamicRangeCompressionProfile
      ( Ac3DynamicRangeCompressionProfile',
        ADRCPFilmStandard,
        ADRCPNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile' Lude.Text
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

pattern ADRCPFilmStandard :: Ac3DynamicRangeCompressionProfile
pattern ADRCPFilmStandard = Ac3DynamicRangeCompressionProfile' "FILM_STANDARD"

pattern ADRCPNone :: Ac3DynamicRangeCompressionProfile
pattern ADRCPNone = Ac3DynamicRangeCompressionProfile' "NONE"

{-# COMPLETE
  ADRCPFilmStandard,
  ADRCPNone,
  Ac3DynamicRangeCompressionProfile'
  #-}
