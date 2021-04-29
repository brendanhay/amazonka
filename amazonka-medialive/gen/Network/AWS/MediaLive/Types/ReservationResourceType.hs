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

import qualified Network.AWS.Prelude as Prelude

-- | Resource type, \'INPUT\', \'OUTPUT\', \'MULTIPLEX\', or \'CHANNEL\'
newtype ReservationResourceType = ReservationResourceType'
  { fromReservationResourceType ::
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
