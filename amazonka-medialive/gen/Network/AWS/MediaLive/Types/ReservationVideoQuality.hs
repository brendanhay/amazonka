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
-- Module      : Network.AWS.MediaLive.Types.ReservationVideoQuality
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationVideoQuality
  ( ReservationVideoQuality
      ( ..,
        ReservationVideoQuality_ENHANCED,
        ReservationVideoQuality_PREMIUM,
        ReservationVideoQuality_STANDARD
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Video quality, e.g. \'STANDARD\' (Outputs only)
newtype ReservationVideoQuality = ReservationVideoQuality'
  { fromReservationVideoQuality ::
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

pattern ReservationVideoQuality_ENHANCED :: ReservationVideoQuality
pattern ReservationVideoQuality_ENHANCED = ReservationVideoQuality' "ENHANCED"

pattern ReservationVideoQuality_PREMIUM :: ReservationVideoQuality
pattern ReservationVideoQuality_PREMIUM = ReservationVideoQuality' "PREMIUM"

pattern ReservationVideoQuality_STANDARD :: ReservationVideoQuality
pattern ReservationVideoQuality_STANDARD = ReservationVideoQuality' "STANDARD"

{-# COMPLETE
  ReservationVideoQuality_ENHANCED,
  ReservationVideoQuality_PREMIUM,
  ReservationVideoQuality_STANDARD,
  ReservationVideoQuality'
  #-}
