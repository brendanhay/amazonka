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
        AssignmentAbandoned,
        AssignmentAccepted,
        AssignmentApproved,
        AssignmentRejected,
        AssignmentReturned,
        AssignmentSubmitted,
        HITCreated,
        HITDisposed,
        HITExpired,
        HITExtended,
        HITReviewable,
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

pattern AssignmentAbandoned :: EventType
pattern AssignmentAbandoned = EventType' "AssignmentAbandoned"

pattern AssignmentAccepted :: EventType
pattern AssignmentAccepted = EventType' "AssignmentAccepted"

pattern AssignmentApproved :: EventType
pattern AssignmentApproved = EventType' "AssignmentApproved"

pattern AssignmentRejected :: EventType
pattern AssignmentRejected = EventType' "AssignmentRejected"

pattern AssignmentReturned :: EventType
pattern AssignmentReturned = EventType' "AssignmentReturned"

pattern AssignmentSubmitted :: EventType
pattern AssignmentSubmitted = EventType' "AssignmentSubmitted"

pattern HITCreated :: EventType
pattern HITCreated = EventType' "HITCreated"

pattern HITDisposed :: EventType
pattern HITDisposed = EventType' "HITDisposed"

pattern HITExpired :: EventType
pattern HITExpired = EventType' "HITExpired"

pattern HITExtended :: EventType
pattern HITExtended = EventType' "HITExtended"

pattern HITReviewable :: EventType
pattern HITReviewable = EventType' "HITReviewable"

pattern Ping :: EventType
pattern Ping = EventType' "Ping"

{-# COMPLETE
  AssignmentAbandoned,
  AssignmentAccepted,
  AssignmentApproved,
  AssignmentRejected,
  AssignmentReturned,
  AssignmentSubmitted,
  HITCreated,
  HITDisposed,
  HITExpired,
  HITExtended,
  HITReviewable,
  Ping,
  EventType'
  #-}
