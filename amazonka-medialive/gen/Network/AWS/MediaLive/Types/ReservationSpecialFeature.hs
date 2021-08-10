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
-- Module      : Network.AWS.MediaLive.Types.ReservationSpecialFeature
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationSpecialFeature
  ( ReservationSpecialFeature
      ( ..,
        ReservationSpecialFeature_ADVANCED_AUDIO,
        ReservationSpecialFeature_AUDIO_NORMALIZATION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Special features, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
newtype ReservationSpecialFeature = ReservationSpecialFeature'
  { fromReservationSpecialFeature ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern ReservationSpecialFeature_ADVANCED_AUDIO :: ReservationSpecialFeature
pattern ReservationSpecialFeature_ADVANCED_AUDIO = ReservationSpecialFeature' "ADVANCED_AUDIO"

pattern ReservationSpecialFeature_AUDIO_NORMALIZATION :: ReservationSpecialFeature
pattern ReservationSpecialFeature_AUDIO_NORMALIZATION = ReservationSpecialFeature' "AUDIO_NORMALIZATION"

{-# COMPLETE
  ReservationSpecialFeature_ADVANCED_AUDIO,
  ReservationSpecialFeature_AUDIO_NORMALIZATION,
  ReservationSpecialFeature'
  #-}
