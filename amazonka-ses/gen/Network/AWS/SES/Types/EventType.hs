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
-- Module      : Network.AWS.SES.Types.EventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.EventType
  ( EventType
      ( ..,
        EventType_Bounce,
        EventType_Click,
        EventType_Complaint,
        EventType_Delivery,
        EventType_Open,
        EventType_Reject,
        EventType_RenderingFailure,
        EventType_Send
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EventType = EventType'
  { fromEventType ::
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

pattern EventType_Bounce :: EventType
pattern EventType_Bounce = EventType' "bounce"

pattern EventType_Click :: EventType
pattern EventType_Click = EventType' "click"

pattern EventType_Complaint :: EventType
pattern EventType_Complaint = EventType' "complaint"

pattern EventType_Delivery :: EventType
pattern EventType_Delivery = EventType' "delivery"

pattern EventType_Open :: EventType
pattern EventType_Open = EventType' "open"

pattern EventType_Reject :: EventType
pattern EventType_Reject = EventType' "reject"

pattern EventType_RenderingFailure :: EventType
pattern EventType_RenderingFailure = EventType' "renderingFailure"

pattern EventType_Send :: EventType
pattern EventType_Send = EventType' "send"

{-# COMPLETE
  EventType_Bounce,
  EventType_Click,
  EventType_Complaint,
  EventType_Delivery,
  EventType_Open,
  EventType_Reject,
  EventType_RenderingFailure,
  EventType_Send,
  EventType'
  #-}
