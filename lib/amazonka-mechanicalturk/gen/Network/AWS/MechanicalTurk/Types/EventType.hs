{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.EventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.EventType where

import Network.AWS.Prelude

data EventType
  = AssignmentAbandoned
  | AssignmentAccepted
  | AssignmentApproved
  | AssignmentRejected
  | AssignmentReturned
  | AssignmentSubmitted
  | HITCreated
  | HITDisposed
  | HITExpired
  | HITExtended
  | HITReviewable
  | Ping
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText EventType where
  parser =
    takeLowerText >>= \case
      "assignmentabandoned" -> pure AssignmentAbandoned
      "assignmentaccepted" -> pure AssignmentAccepted
      "assignmentapproved" -> pure AssignmentApproved
      "assignmentrejected" -> pure AssignmentRejected
      "assignmentreturned" -> pure AssignmentReturned
      "assignmentsubmitted" -> pure AssignmentSubmitted
      "hitcreated" -> pure HITCreated
      "hitdisposed" -> pure HITDisposed
      "hitexpired" -> pure HITExpired
      "hitextended" -> pure HITExtended
      "hitreviewable" -> pure HITReviewable
      "ping" -> pure Ping
      e ->
        fromTextError $
          "Failure parsing EventType from value: '" <> e
            <> "'. Accepted values: assignmentabandoned, assignmentaccepted, assignmentapproved, assignmentrejected, assignmentreturned, assignmentsubmitted, hitcreated, hitdisposed, hitexpired, hitextended, hitreviewable, ping"

instance ToText EventType where
  toText = \case
    AssignmentAbandoned -> "AssignmentAbandoned"
    AssignmentAccepted -> "AssignmentAccepted"
    AssignmentApproved -> "AssignmentApproved"
    AssignmentRejected -> "AssignmentRejected"
    AssignmentReturned -> "AssignmentReturned"
    AssignmentSubmitted -> "AssignmentSubmitted"
    HITCreated -> "HITCreated"
    HITDisposed -> "HITDisposed"
    HITExpired -> "HITExpired"
    HITExtended -> "HITExtended"
    HITReviewable -> "HITReviewable"
    Ping -> "Ping"

instance Hashable EventType

instance NFData EventType

instance ToByteString EventType

instance ToQuery EventType

instance ToHeader EventType

instance ToJSON EventType where
  toJSON = toJSONText
