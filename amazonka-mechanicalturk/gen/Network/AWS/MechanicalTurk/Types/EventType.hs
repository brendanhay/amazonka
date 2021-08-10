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
-- Module      : Network.AWS.MechanicalTurk.Types.EventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.EventType
  ( EventType
      ( ..,
        EventType_AssignmentAbandoned,
        EventType_AssignmentAccepted,
        EventType_AssignmentApproved,
        EventType_AssignmentRejected,
        EventType_AssignmentReturned,
        EventType_AssignmentSubmitted,
        EventType_HITCreated,
        EventType_HITDisposed,
        EventType_HITExpired,
        EventType_HITExtended,
        EventType_HITReviewable,
        EventType_Ping
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

pattern EventType_AssignmentAbandoned :: EventType
pattern EventType_AssignmentAbandoned = EventType' "AssignmentAbandoned"

pattern EventType_AssignmentAccepted :: EventType
pattern EventType_AssignmentAccepted = EventType' "AssignmentAccepted"

pattern EventType_AssignmentApproved :: EventType
pattern EventType_AssignmentApproved = EventType' "AssignmentApproved"

pattern EventType_AssignmentRejected :: EventType
pattern EventType_AssignmentRejected = EventType' "AssignmentRejected"

pattern EventType_AssignmentReturned :: EventType
pattern EventType_AssignmentReturned = EventType' "AssignmentReturned"

pattern EventType_AssignmentSubmitted :: EventType
pattern EventType_AssignmentSubmitted = EventType' "AssignmentSubmitted"

pattern EventType_HITCreated :: EventType
pattern EventType_HITCreated = EventType' "HITCreated"

pattern EventType_HITDisposed :: EventType
pattern EventType_HITDisposed = EventType' "HITDisposed"

pattern EventType_HITExpired :: EventType
pattern EventType_HITExpired = EventType' "HITExpired"

pattern EventType_HITExtended :: EventType
pattern EventType_HITExtended = EventType' "HITExtended"

pattern EventType_HITReviewable :: EventType
pattern EventType_HITReviewable = EventType' "HITReviewable"

pattern EventType_Ping :: EventType
pattern EventType_Ping = EventType' "Ping"

{-# COMPLETE
  EventType_AssignmentAbandoned,
  EventType_AssignmentAccepted,
  EventType_AssignmentApproved,
  EventType_AssignmentRejected,
  EventType_AssignmentReturned,
  EventType_AssignmentSubmitted,
  EventType_HITCreated,
  EventType_HITDisposed,
  EventType_HITExpired,
  EventType_HITExtended,
  EventType_HITReviewable,
  EventType_Ping,
  EventType'
  #-}
