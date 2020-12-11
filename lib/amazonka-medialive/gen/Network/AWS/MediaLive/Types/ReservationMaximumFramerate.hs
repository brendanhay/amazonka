-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumFramerate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationMaximumFramerate
  ( ReservationMaximumFramerate
      ( ReservationMaximumFramerate',
        Max30Fps,
        Max60Fps
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Maximum framerate in frames per second (Outputs only)
newtype ReservationMaximumFramerate = ReservationMaximumFramerate' Lude.Text
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

pattern Max30Fps :: ReservationMaximumFramerate
pattern Max30Fps = ReservationMaximumFramerate' "MAX_30_FPS"

pattern Max60Fps :: ReservationMaximumFramerate
pattern Max60Fps = ReservationMaximumFramerate' "MAX_60_FPS"

{-# COMPLETE
  Max30Fps,
  Max60Fps,
  ReservationMaximumFramerate'
  #-}
