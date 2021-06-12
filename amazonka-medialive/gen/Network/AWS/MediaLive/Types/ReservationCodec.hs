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
-- Module      : Network.AWS.MediaLive.Types.ReservationCodec
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationCodec
  ( ReservationCodec
      ( ..,
        ReservationCodec_AUDIO,
        ReservationCodec_AVC,
        ReservationCodec_HEVC,
        ReservationCodec_LINK,
        ReservationCodec_MPEG2
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Codec, \'MPEG2\', \'AVC\', \'HEVC\', or \'AUDIO\'
newtype ReservationCodec = ReservationCodec'
  { fromReservationCodec ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ReservationCodec_AUDIO :: ReservationCodec
pattern ReservationCodec_AUDIO = ReservationCodec' "AUDIO"

pattern ReservationCodec_AVC :: ReservationCodec
pattern ReservationCodec_AVC = ReservationCodec' "AVC"

pattern ReservationCodec_HEVC :: ReservationCodec
pattern ReservationCodec_HEVC = ReservationCodec' "HEVC"

pattern ReservationCodec_LINK :: ReservationCodec
pattern ReservationCodec_LINK = ReservationCodec' "LINK"

pattern ReservationCodec_MPEG2 :: ReservationCodec
pattern ReservationCodec_MPEG2 = ReservationCodec' "MPEG2"

{-# COMPLETE
  ReservationCodec_AUDIO,
  ReservationCodec_AVC,
  ReservationCodec_HEVC,
  ReservationCodec_LINK,
  ReservationCodec_MPEG2,
  ReservationCodec'
  #-}
