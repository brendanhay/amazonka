{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9RateControlMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Vp9RateControlMode
  ( Vp9RateControlMode
    ( Vp9RateControlMode'
    , Vp9RateControlModeVbr
    , fromVp9RateControlMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | With the VP9 codec, you can use only the variable bitrate (VBR) rate control mode.
newtype Vp9RateControlMode = Vp9RateControlMode'{fromVp9RateControlMode
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern Vp9RateControlModeVbr :: Vp9RateControlMode
pattern Vp9RateControlModeVbr = Vp9RateControlMode' "VBR"

{-# COMPLETE 
  Vp9RateControlModeVbr,
  Vp9RateControlMode'
  #-}
