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
        HCSRfc6381,
        HCSRfc4281
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
newtype HlsCodecSpecification = HlsCodecSpecification' Lude.Text
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

pattern HCSRfc6381 :: HlsCodecSpecification
pattern HCSRfc6381 = HlsCodecSpecification' "RFC_6381"

pattern HCSRfc4281 :: HlsCodecSpecification
pattern HCSRfc4281 = HlsCodecSpecification' "RFC_4281"

{-# COMPLETE
  HCSRfc6381,
  HCSRfc4281,
  HlsCodecSpecification'
  #-}
