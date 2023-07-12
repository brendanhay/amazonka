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
-- Module      : Amazonka.MediaLive.Types.ReservationCodec
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.ReservationCodec
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Codec, \'MPEG2\', \'AVC\', \'HEVC\', or \'AUDIO\'
newtype ReservationCodec = ReservationCodec'
  { fromReservationCodec ::
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
