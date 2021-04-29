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

import qualified Network.AWS.Prelude as Prelude

-- | Codec, \'MPEG2\', \'AVC\', \'HEVC\', or \'AUDIO\'
newtype ReservationCodec = ReservationCodec'
  { fromReservationCodec ::
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
