{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsH265PackagingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsH265PackagingType
  ( HlsH265PackagingType
    ( HlsH265PackagingType'
    , HlsH265PackagingTypeHEV1
    , HlsH265PackagingTypeHVC1
    , fromHlsH265PackagingType
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Hls H265 Packaging Type
newtype HlsH265PackagingType = HlsH265PackagingType'{fromHlsH265PackagingType
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern HlsH265PackagingTypeHEV1 :: HlsH265PackagingType
pattern HlsH265PackagingTypeHEV1 = HlsH265PackagingType' "HEV1"

pattern HlsH265PackagingTypeHVC1 :: HlsH265PackagingType
pattern HlsH265PackagingTypeHVC1 = HlsH265PackagingType' "HVC1"

{-# COMPLETE 
  HlsH265PackagingTypeHEV1,

  HlsH265PackagingTypeHVC1,
  HlsH265PackagingType'
  #-}
