{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacCodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.AacCodecProfile
  ( AacCodecProfile
    ( AacCodecProfile'
    , AacCodecProfileLC
    , AacCodecProfileHEV1
    , AacCodecProfileHEV2
    , fromAacCodecProfile
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | AAC Profile.
newtype AacCodecProfile = AacCodecProfile'{fromAacCodecProfile ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern AacCodecProfileLC :: AacCodecProfile
pattern AacCodecProfileLC = AacCodecProfile' "LC"

pattern AacCodecProfileHEV1 :: AacCodecProfile
pattern AacCodecProfileHEV1 = AacCodecProfile' "HEV1"

pattern AacCodecProfileHEV2 :: AacCodecProfile
pattern AacCodecProfileHEV2 = AacCodecProfile' "HEV2"

{-# COMPLETE 
  AacCodecProfileLC,

  AacCodecProfileHEV1,

  AacCodecProfileHEV2,
  AacCodecProfile'
  #-}
