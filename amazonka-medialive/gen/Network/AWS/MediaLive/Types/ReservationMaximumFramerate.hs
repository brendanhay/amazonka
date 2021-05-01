{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ReservationMaximumFramerate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationMaximumFramerate
  ( ReservationMaximumFramerate
      ( ..,
        ReservationMaximumFramerate_MAX_30_FPS,
        ReservationMaximumFramerate_MAX_60_FPS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Maximum framerate in frames per second (Outputs only)
newtype ReservationMaximumFramerate = ReservationMaximumFramerate'
  { fromReservationMaximumFramerate ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ReservationMaximumFramerate_MAX_30_FPS :: ReservationMaximumFramerate
pattern ReservationMaximumFramerate_MAX_30_FPS = ReservationMaximumFramerate' "MAX_30_FPS"

pattern ReservationMaximumFramerate_MAX_60_FPS :: ReservationMaximumFramerate
pattern ReservationMaximumFramerate_MAX_60_FPS = ReservationMaximumFramerate' "MAX_60_FPS"

{-# COMPLETE
  ReservationMaximumFramerate_MAX_30_FPS,
  ReservationMaximumFramerate_MAX_60_FPS,
  ReservationMaximumFramerate'
  #-}
