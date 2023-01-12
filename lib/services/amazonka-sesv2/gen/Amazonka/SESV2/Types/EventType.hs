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
-- Module      : Amazonka.SESV2.Types.EventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.EventType
  ( EventType
      ( ..,
        EventType_BOUNCE,
        EventType_CLICK,
        EventType_COMPLAINT,
        EventType_DELIVERY,
        EventType_DELIVERY_DELAY,
        EventType_OPEN,
        EventType_REJECT,
        EventType_RENDERING_FAILURE,
        EventType_SEND,
        EventType_SUBSCRIPTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An email sending event type. For example, email sends, opens, and
-- bounces are all email events.
newtype EventType = EventType'
  { fromEventType ::
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

pattern EventType_BOUNCE :: EventType
pattern EventType_BOUNCE = EventType' "BOUNCE"

pattern EventType_CLICK :: EventType
pattern EventType_CLICK = EventType' "CLICK"

pattern EventType_COMPLAINT :: EventType
pattern EventType_COMPLAINT = EventType' "COMPLAINT"

pattern EventType_DELIVERY :: EventType
pattern EventType_DELIVERY = EventType' "DELIVERY"

pattern EventType_DELIVERY_DELAY :: EventType
pattern EventType_DELIVERY_DELAY = EventType' "DELIVERY_DELAY"

pattern EventType_OPEN :: EventType
pattern EventType_OPEN = EventType' "OPEN"

pattern EventType_REJECT :: EventType
pattern EventType_REJECT = EventType' "REJECT"

pattern EventType_RENDERING_FAILURE :: EventType
pattern EventType_RENDERING_FAILURE = EventType' "RENDERING_FAILURE"

pattern EventType_SEND :: EventType
pattern EventType_SEND = EventType' "SEND"

pattern EventType_SUBSCRIPTION :: EventType
pattern EventType_SUBSCRIPTION = EventType' "SUBSCRIPTION"

{-# COMPLETE
  EventType_BOUNCE,
  EventType_CLICK,
  EventType_COMPLAINT,
  EventType_DELIVERY,
  EventType_DELIVERY_DELAY,
  EventType_OPEN,
  EventType_REJECT,
  EventType_RENDERING_FAILURE,
  EventType_SEND,
  EventType_SUBSCRIPTION,
  EventType'
  #-}
