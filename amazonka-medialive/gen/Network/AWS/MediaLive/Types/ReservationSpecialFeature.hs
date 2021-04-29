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

import qualified Network.AWS.Prelude as Prelude

-- | Special features, \'ADVANCED_AUDIO\' or \'AUDIO_NORMALIZATION\'
newtype ReservationSpecialFeature = ReservationSpecialFeature'
  { fromReservationSpecialFeature ::
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

pattern ReservationSpecialFeature_ADVANCED_AUDIO :: ReservationSpecialFeature
pattern ReservationSpecialFeature_ADVANCED_AUDIO = ReservationSpecialFeature' "ADVANCED_AUDIO"

pattern ReservationSpecialFeature_AUDIO_NORMALIZATION :: ReservationSpecialFeature
pattern ReservationSpecialFeature_AUDIO_NORMALIZATION = ReservationSpecialFeature' "AUDIO_NORMALIZATION"

{-# COMPLETE
  ReservationSpecialFeature_ADVANCED_AUDIO,
  ReservationSpecialFeature_AUDIO_NORMALIZATION,
  ReservationSpecialFeature'
  #-}
