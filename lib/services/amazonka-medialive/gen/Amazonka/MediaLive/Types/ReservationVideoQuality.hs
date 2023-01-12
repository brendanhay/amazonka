{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.ReservationVideoQuality
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationVideoQuality
  ( ReservationVideoQuality
      ( ..,
        ReservationVideoQuality_ENHANCED,
        ReservationVideoQuality_PREMIUM,
        ReservationVideoQuality_STANDARD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Video quality, e.g. \'STANDARD\' (Outputs only)
newtype ReservationVideoQuality = ReservationVideoQuality'
  { fromReservationVideoQuality ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
