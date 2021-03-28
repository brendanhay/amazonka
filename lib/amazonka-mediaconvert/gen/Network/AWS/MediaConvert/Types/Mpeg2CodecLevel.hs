{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
  ( Mpeg2CodecLevel
    ( Mpeg2CodecLevel'
    , Mpeg2CodecLevelAuto
    , Mpeg2CodecLevelLow
    , Mpeg2CodecLevelMain
    , Mpeg2CodecLevelHIGH1440
    , Mpeg2CodecLevelHigh
    , fromMpeg2CodecLevel
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
newtype Mpeg2CodecLevel = Mpeg2CodecLevel'{fromMpeg2CodecLevel ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern Mpeg2CodecLevelAuto :: Mpeg2CodecLevel
pattern Mpeg2CodecLevelAuto = Mpeg2CodecLevel' "AUTO"

pattern Mpeg2CodecLevelLow :: Mpeg2CodecLevel
pattern Mpeg2CodecLevelLow = Mpeg2CodecLevel' "LOW"

pattern Mpeg2CodecLevelMain :: Mpeg2CodecLevel
pattern Mpeg2CodecLevelMain = Mpeg2CodecLevel' "MAIN"

pattern Mpeg2CodecLevelHIGH1440 :: Mpeg2CodecLevel
pattern Mpeg2CodecLevelHIGH1440 = Mpeg2CodecLevel' "HIGH1440"

pattern Mpeg2CodecLevelHigh :: Mpeg2CodecLevel
pattern Mpeg2CodecLevelHigh = Mpeg2CodecLevel' "HIGH"

{-# COMPLETE 
  Mpeg2CodecLevelAuto,

  Mpeg2CodecLevelLow,

  Mpeg2CodecLevelMain,

  Mpeg2CodecLevelHIGH1440,

  Mpeg2CodecLevelHigh,
  Mpeg2CodecLevel'
  #-}
