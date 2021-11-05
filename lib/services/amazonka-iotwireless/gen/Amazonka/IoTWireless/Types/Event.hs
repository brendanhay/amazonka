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
-- Module      : Amazonka.IoTWireless.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Event
  ( Event
      ( ..,
        Event_Ack,
        Event_Discovered,
        Event_Lost,
        Event_Nack,
        Event_Passthrough
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk device status notification.
newtype Event = Event' {fromEvent :: Core.Text}
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

pattern Event_Ack :: Event
pattern Event_Ack = Event' "ack"

pattern Event_Discovered :: Event
pattern Event_Discovered = Event' "discovered"

pattern Event_Lost :: Event
pattern Event_Lost = Event' "lost"

pattern Event_Nack :: Event
pattern Event_Nack = Event' "nack"

pattern Event_Passthrough :: Event
pattern Event_Passthrough = Event' "passthrough"

{-# COMPLETE
  Event_Ack,
  Event_Discovered,
  Event_Lost,
  Event_Nack,
  Event_Passthrough,
  Event'
  #-}
