{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.EventType
  ( EventType
      ( EventType',
        AssignmentAccepted,
        AssignmentAbandoned,
        AssignmentReturned,
        AssignmentSubmitted,
        AssignmentRejected,
        AssignmentApproved,
        HITCreated,
        HITExpired,
        HITReviewable,
        HITExtended,
        HITDisposed,
        Ping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventType = EventType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AssignmentAccepted :: EventType
pattern AssignmentAccepted = EventType' "AssignmentAccepted"

pattern AssignmentAbandoned :: EventType
pattern AssignmentAbandoned = EventType' "AssignmentAbandoned"

pattern AssignmentReturned :: EventType
pattern AssignmentReturned = EventType' "AssignmentReturned"

pattern AssignmentSubmitted :: EventType
pattern AssignmentSubmitted = EventType' "AssignmentSubmitted"

pattern AssignmentRejected :: EventType
pattern AssignmentRejected = EventType' "AssignmentRejected"

pattern AssignmentApproved :: EventType
pattern AssignmentApproved = EventType' "AssignmentApproved"

pattern HITCreated :: EventType
pattern HITCreated = EventType' "HITCreated"

pattern HITExpired :: EventType
pattern HITExpired = EventType' "HITExpired"

pattern HITReviewable :: EventType
pattern HITReviewable = EventType' "HITReviewable"

pattern HITExtended :: EventType
pattern HITExtended = EventType' "HITExtended"

pattern HITDisposed :: EventType
pattern HITDisposed = EventType' "HITDisposed"

pattern Ping :: EventType
pattern Ping = EventType' "Ping"

{-# COMPLETE
  AssignmentAccepted,
  AssignmentAbandoned,
  AssignmentReturned,
  AssignmentSubmitted,
  AssignmentRejected,
  AssignmentApproved,
  HITCreated,
  HITExpired,
  HITReviewable,
  HITExtended,
  HITDisposed,
  Ping,
  EventType'
  #-}
