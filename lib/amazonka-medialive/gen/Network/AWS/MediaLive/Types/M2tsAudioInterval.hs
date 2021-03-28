{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsAudioInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.M2tsAudioInterval
  ( M2tsAudioInterval
    ( M2tsAudioInterval'
    , M2tsAudioIntervalVideoAndFixedIntervals
    , M2tsAudioIntervalVideoInterval
    , fromM2tsAudioInterval
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | M2ts Audio Interval
newtype M2tsAudioInterval = M2tsAudioInterval'{fromM2tsAudioInterval
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern M2tsAudioIntervalVideoAndFixedIntervals :: M2tsAudioInterval
pattern M2tsAudioIntervalVideoAndFixedIntervals = M2tsAudioInterval' "VIDEO_AND_FIXED_INTERVALS"

pattern M2tsAudioIntervalVideoInterval :: M2tsAudioInterval
pattern M2tsAudioIntervalVideoInterval = M2tsAudioInterval' "VIDEO_INTERVAL"

{-# COMPLETE 
  M2tsAudioIntervalVideoAndFixedIntervals,

  M2tsAudioIntervalVideoInterval,
  M2tsAudioInterval'
  #-}
