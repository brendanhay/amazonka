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
-- Module      : Amazonka.MediaLive.Types.ReservationSpecialFeature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationSpecialFeature
  ( ReservationSpecialFeature
      ( ..,
        ReservationSpecialFeature_ADVANCED_AUDIO,
        ReservationSpecialFeature_AUDIO_NORMALIZATION,
        ReservationSpecialFeature_MGHD,
        ReservationSpecialFeature_MGUHD
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Special features, \'ADVANCED_AUDIO\' \'AUDIO_NORMALIZATION\' \'MGHD\' or
-- \'MGUHD\'
newtype ReservationSpecialFeature = ReservationSpecialFeature'
  { fromReservationSpecialFeature ::
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

pattern ReservationSpecialFeature_ADVANCED_AUDIO :: ReservationSpecialFeature
pattern ReservationSpecialFeature_ADVANCED_AUDIO = ReservationSpecialFeature' "ADVANCED_AUDIO"

pattern ReservationSpecialFeature_AUDIO_NORMALIZATION :: ReservationSpecialFeature
pattern ReservationSpecialFeature_AUDIO_NORMALIZATION = ReservationSpecialFeature' "AUDIO_NORMALIZATION"

pattern ReservationSpecialFeature_MGHD :: ReservationSpecialFeature
pattern ReservationSpecialFeature_MGHD = ReservationSpecialFeature' "MGHD"

pattern ReservationSpecialFeature_MGUHD :: ReservationSpecialFeature
pattern ReservationSpecialFeature_MGUHD = ReservationSpecialFeature' "MGUHD"

{-# COMPLETE
  ReservationSpecialFeature_ADVANCED_AUDIO,
  ReservationSpecialFeature_AUDIO_NORMALIZATION,
  ReservationSpecialFeature_MGHD,
  ReservationSpecialFeature_MGUHD,
  ReservationSpecialFeature'
  #-}
