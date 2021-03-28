{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Mpeg2RateControlMode
  ( Mpeg2RateControlMode
    ( Mpeg2RateControlMode'
    , Mpeg2RateControlModeVbr
    , Mpeg2RateControlModeCbr
    , fromMpeg2RateControlMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Rate control mode (Mpeg2RateControlMode) to specifiy whether the bitrate is variable (vbr) or constant (cbr).
newtype Mpeg2RateControlMode = Mpeg2RateControlMode'{fromMpeg2RateControlMode
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern Mpeg2RateControlModeVbr :: Mpeg2RateControlMode
pattern Mpeg2RateControlModeVbr = Mpeg2RateControlMode' "VBR"

pattern Mpeg2RateControlModeCbr :: Mpeg2RateControlMode
pattern Mpeg2RateControlModeCbr = Mpeg2RateControlMode' "CBR"

{-# COMPLETE 
  Mpeg2RateControlModeVbr,

  Mpeg2RateControlModeCbr,
  Mpeg2RateControlMode'
  #-}
