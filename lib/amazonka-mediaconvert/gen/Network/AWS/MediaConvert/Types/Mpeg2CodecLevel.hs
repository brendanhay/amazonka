{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2CodecLevel
  ( Mpeg2CodecLevel
      ( Mpeg2CodecLevel',
        MCLAuto,
        MCLHIGH1440,
        MCLHigh,
        MCLLow,
        MCLMain
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Level (Mpeg2CodecLevel) to set the MPEG-2 level for the video output.
newtype Mpeg2CodecLevel = Mpeg2CodecLevel' Lude.Text
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

pattern MCLAuto :: Mpeg2CodecLevel
pattern MCLAuto = Mpeg2CodecLevel' "AUTO"

pattern MCLHIGH1440 :: Mpeg2CodecLevel
pattern MCLHIGH1440 = Mpeg2CodecLevel' "HIGH1440"

pattern MCLHigh :: Mpeg2CodecLevel
pattern MCLHigh = Mpeg2CodecLevel' "HIGH"

pattern MCLLow :: Mpeg2CodecLevel
pattern MCLLow = Mpeg2CodecLevel' "LOW"

pattern MCLMain :: Mpeg2CodecLevel
pattern MCLMain = Mpeg2CodecLevel' "MAIN"

{-# COMPLETE
  MCLAuto,
  MCLHIGH1440,
  MCLHigh,
  MCLLow,
  MCLMain,
  Mpeg2CodecLevel'
  #-}
