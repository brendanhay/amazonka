{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsCodecSpecification
  ( HlsCodecSpecification
      ( HlsCodecSpecification',
        HlsCodecSpecificationRfc6381,
        HlsCodecSpecificationRfc4281,
        fromHlsCodecSpecification
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
newtype HlsCodecSpecification = HlsCodecSpecification'
  { fromHlsCodecSpecification ::
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

pattern HlsCodecSpecificationRfc6381 :: HlsCodecSpecification
pattern HlsCodecSpecificationRfc6381 = HlsCodecSpecification' "RFC_6381"

pattern HlsCodecSpecificationRfc4281 :: HlsCodecSpecification
pattern HlsCodecSpecificationRfc4281 = HlsCodecSpecification' "RFC_4281"

{-# COMPLETE
  HlsCodecSpecificationRfc6381,
  HlsCodecSpecificationRfc4281,
  HlsCodecSpecification'
  #-}
