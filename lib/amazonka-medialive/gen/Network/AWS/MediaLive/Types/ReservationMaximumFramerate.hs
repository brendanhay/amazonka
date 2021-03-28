{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.ReservationMaximumFramerate
  ( ReservationMaximumFramerate
    ( ReservationMaximumFramerate'
    , ReservationMaximumFramerateMax30Fps
    , ReservationMaximumFramerateMax60Fps
    , fromReservationMaximumFramerate
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Maximum framerate in frames per second (Outputs only)
newtype ReservationMaximumFramerate = ReservationMaximumFramerate'{fromReservationMaximumFramerate
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern ReservationMaximumFramerateMax30Fps :: ReservationMaximumFramerate
pattern ReservationMaximumFramerateMax30Fps = ReservationMaximumFramerate' "MAX_30_FPS"

pattern ReservationMaximumFramerateMax60Fps :: ReservationMaximumFramerate
pattern ReservationMaximumFramerateMax60Fps = ReservationMaximumFramerate' "MAX_60_FPS"

{-# COMPLETE 
  ReservationMaximumFramerateMax30Fps,

  ReservationMaximumFramerateMax60Fps,
  ReservationMaximumFramerate'
  #-}
