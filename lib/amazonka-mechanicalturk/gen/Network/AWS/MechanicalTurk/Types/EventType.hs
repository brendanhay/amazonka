{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MechanicalTurk.Types.EventType
  ( EventType
    ( EventType'
    , EventTypeAssignmentAccepted
    , EventTypeAssignmentAbandoned
    , EventTypeAssignmentReturned
    , EventTypeAssignmentSubmitted
    , EventTypeAssignmentRejected
    , EventTypeAssignmentApproved
    , EventTypeHITCreated
    , EventTypeHITExpired
    , EventTypeHITReviewable
    , EventTypeHITExtended
    , EventTypeHITDisposed
    , EventTypePing
    , fromEventType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype EventType = EventType'{fromEventType :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern EventTypeAssignmentAccepted :: EventType
pattern EventTypeAssignmentAccepted = EventType' "AssignmentAccepted"

pattern EventTypeAssignmentAbandoned :: EventType
pattern EventTypeAssignmentAbandoned = EventType' "AssignmentAbandoned"

pattern EventTypeAssignmentReturned :: EventType
pattern EventTypeAssignmentReturned = EventType' "AssignmentReturned"

pattern EventTypeAssignmentSubmitted :: EventType
pattern EventTypeAssignmentSubmitted = EventType' "AssignmentSubmitted"

pattern EventTypeAssignmentRejected :: EventType
pattern EventTypeAssignmentRejected = EventType' "AssignmentRejected"

pattern EventTypeAssignmentApproved :: EventType
pattern EventTypeAssignmentApproved = EventType' "AssignmentApproved"

pattern EventTypeHITCreated :: EventType
pattern EventTypeHITCreated = EventType' "HITCreated"

pattern EventTypeHITExpired :: EventType
pattern EventTypeHITExpired = EventType' "HITExpired"

pattern EventTypeHITReviewable :: EventType
pattern EventTypeHITReviewable = EventType' "HITReviewable"

pattern EventTypeHITExtended :: EventType
pattern EventTypeHITExtended = EventType' "HITExtended"

pattern EventTypeHITDisposed :: EventType
pattern EventTypeHITDisposed = EventType' "HITDisposed"

pattern EventTypePing :: EventType
pattern EventTypePing = EventType' "Ping"

{-# COMPLETE 
  EventTypeAssignmentAccepted,

  EventTypeAssignmentAbandoned,

  EventTypeAssignmentReturned,

  EventTypeAssignmentSubmitted,

  EventTypeAssignmentRejected,

  EventTypeAssignmentApproved,

  EventTypeHITCreated,

  EventTypeHITExpired,

  EventTypeHITReviewable,

  EventTypeHITExtended,

  EventTypeHITDisposed,

  EventTypePing,
  EventType'
  #-}
