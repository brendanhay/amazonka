{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafCodecSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafCodecSpecification
  ( CmafCodecSpecification
      ( CmafCodecSpecification',
        Rfc4281,
        Rfc6381
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
newtype CmafCodecSpecification = CmafCodecSpecification' Lude.Text
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

pattern Rfc4281 :: CmafCodecSpecification
pattern Rfc4281 = CmafCodecSpecification' "RFC_4281"

pattern Rfc6381 :: CmafCodecSpecification
pattern Rfc6381 = CmafCodecSpecification' "RFC_6381"

{-# COMPLETE
  Rfc4281,
  Rfc6381,
  CmafCodecSpecification'
  #-}
