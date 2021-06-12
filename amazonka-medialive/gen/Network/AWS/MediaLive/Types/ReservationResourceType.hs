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
-- Module      : Network.AWS.MediaLive.Types.ReservationResourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ReservationResourceType
  ( ReservationResourceType
      ( ..,
        ReservationResourceType_CHANNEL,
        ReservationResourceType_INPUT,
        ReservationResourceType_MULTIPLEX,
        ReservationResourceType_OUTPUT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
newtype ReservationResourceType = ReservationResourceType'
  { fromReservationResourceType ::
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

pattern ReservationResourceType_CHANNEL :: ReservationResourceType
pattern ReservationResourceType_CHANNEL = ReservationResourceType' "CHANNEL"

pattern ReservationResourceType_INPUT :: ReservationResourceType
pattern ReservationResourceType_INPUT = ReservationResourceType' "INPUT"

pattern ReservationResourceType_MULTIPLEX :: ReservationResourceType
pattern ReservationResourceType_MULTIPLEX = ReservationResourceType' "MULTIPLEX"

pattern ReservationResourceType_OUTPUT :: ReservationResourceType
pattern ReservationResourceType_OUTPUT = ReservationResourceType' "OUTPUT"

{-# COMPLETE
  ReservationResourceType_CHANNEL,
  ReservationResourceType_INPUT,
  ReservationResourceType_MULTIPLEX,
  ReservationResourceType_OUTPUT,
  ReservationResourceType'
  #-}
