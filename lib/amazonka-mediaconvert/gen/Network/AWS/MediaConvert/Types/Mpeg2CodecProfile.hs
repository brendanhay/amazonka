{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2CodecProfile
  ( Mpeg2CodecProfile
    ( Mpeg2CodecProfile'
    , Mpeg2CodecProfileMain
    , Mpeg2CodecProfileProfile422
    , fromMpeg2CodecProfile
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Profile (Mpeg2CodecProfile) to set the MPEG-2 profile for the video output.
newtype Mpeg2CodecProfile = Mpeg2CodecProfile'{fromMpeg2CodecProfile
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern Mpeg2CodecProfileMain :: Mpeg2CodecProfile
pattern Mpeg2CodecProfileMain = Mpeg2CodecProfile' "MAIN"

pattern Mpeg2CodecProfileProfile422 :: Mpeg2CodecProfile
pattern Mpeg2CodecProfileProfile422 = Mpeg2CodecProfile' "PROFILE_422"

{-# COMPLETE 
  Mpeg2CodecProfileMain,

  Mpeg2CodecProfileProfile422,
  Mpeg2CodecProfile'
  #-}
