{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsCodecSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsCodecSpecification
  ( HlsCodecSpecification
    ( HlsCodecSpecification'
    , HlsCodecSpecificationRfc4281
    , HlsCodecSpecificationRfc6381
    , fromHlsCodecSpecification
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Hls Codec Specification
newtype HlsCodecSpecification = HlsCodecSpecification'{fromHlsCodecSpecification
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern HlsCodecSpecificationRfc4281 :: HlsCodecSpecification
pattern HlsCodecSpecificationRfc4281 = HlsCodecSpecification' "RFC_4281"

pattern HlsCodecSpecificationRfc6381 :: HlsCodecSpecification
pattern HlsCodecSpecificationRfc6381 = HlsCodecSpecification' "RFC_6381"

{-# COMPLETE 
  HlsCodecSpecificationRfc4281,

  HlsCodecSpecificationRfc6381,
  HlsCodecSpecification'
  #-}
