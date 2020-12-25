{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Ac3DynamicRangeCompressionProfileFilmStandard,
        Ac3DynamicRangeCompressionProfileNone,
        fromAc3DynamicRangeCompressionProfile
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | If set to FILM_STANDARD, adds dynamic range compression signaling to the output bitstream as defined in the Dolby Digital specification.
newtype Ac3DynamicRangeCompressionProfile = Ac3DynamicRangeCompressionProfile'
  { fromAc3DynamicRangeCompressionProfile ::
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

pattern Ac3DynamicRangeCompressionProfileFilmStandard :: Ac3DynamicRangeCompressionProfile
pattern Ac3DynamicRangeCompressionProfileFilmStandard = Ac3DynamicRangeCompressionProfile' "FILM_STANDARD"

pattern Ac3DynamicRangeCompressionProfileNone :: Ac3DynamicRangeCompressionProfile
pattern Ac3DynamicRangeCompressionProfileNone = Ac3DynamicRangeCompressionProfile' "NONE"

{-# COMPLETE
  Ac3DynamicRangeCompressionProfileFilmStandard,
  Ac3DynamicRangeCompressionProfileNone,
  Ac3DynamicRangeCompressionProfile'
  #-}
