{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Event where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | The bucket event for which to send notifications.
data Event
  = S3ObjectCreated
  | S3ObjectCreatedCompleteMultipartUpload
  | S3ObjectCreatedCopy
  | S3ObjectCreatedPost
  | S3ObjectCreatedPut
  | S3ObjectRemoved
  | S3ObjectRemovedDelete
  | S3ObjectRemovedDeleteMarkerCreated
  | S3ObjectRestore
  | S3ObjectRestoreCompleted
  | S3ObjectRestorePost
  | S3ReducedRedundancyLostObject
  | S3Replication
  | S3ReplicationOperationFailedReplication
  | S3ReplicationOperationMissedThreshold
  | S3ReplicationOperationNotTracked
  | S3ReplicationOperationReplicatedAfterThreshold
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

instance FromText Event where
  parser =
    takeLowerText >>= \case
      "s3:objectcreated:*" -> pure S3ObjectCreated
      "s3:objectcreated:completemultipartupload" -> pure S3ObjectCreatedCompleteMultipartUpload
      "s3:objectcreated:copy" -> pure S3ObjectCreatedCopy
      "s3:objectcreated:post" -> pure S3ObjectCreatedPost
      "s3:objectcreated:put" -> pure S3ObjectCreatedPut
      "s3:objectremoved:*" -> pure S3ObjectRemoved
      "s3:objectremoved:delete" -> pure S3ObjectRemovedDelete
      "s3:objectremoved:deletemarkercreated" -> pure S3ObjectRemovedDeleteMarkerCreated
      "s3:objectrestore:*" -> pure S3ObjectRestore
      "s3:objectrestore:completed" -> pure S3ObjectRestoreCompleted
      "s3:objectrestore:post" -> pure S3ObjectRestorePost
      "s3:reducedredundancylostobject" -> pure S3ReducedRedundancyLostObject
      "s3:replication:*" -> pure S3Replication
      "s3:replication:operationfailedreplication" -> pure S3ReplicationOperationFailedReplication
      "s3:replication:operationmissedthreshold" -> pure S3ReplicationOperationMissedThreshold
      "s3:replication:operationnottracked" -> pure S3ReplicationOperationNotTracked
      "s3:replication:operationreplicatedafterthreshold" -> pure S3ReplicationOperationReplicatedAfterThreshold
      e ->
        fromTextError $
          "Failure parsing Event from value: '" <> e
            <> "'. Accepted values: s3:objectcreated:*, s3:objectcreated:completemultipartupload, s3:objectcreated:copy, s3:objectcreated:post, s3:objectcreated:put, s3:objectremoved:*, s3:objectremoved:delete, s3:objectremoved:deletemarkercreated, s3:objectrestore:*, s3:objectrestore:completed, s3:objectrestore:post, s3:reducedredundancylostobject, s3:replication:*, s3:replication:operationfailedreplication, s3:replication:operationmissedthreshold, s3:replication:operationnottracked, s3:replication:operationreplicatedafterthreshold"

instance ToText Event where
  toText = \case
    S3ObjectCreated -> "s3:ObjectCreated:*"
    S3ObjectCreatedCompleteMultipartUpload -> "s3:ObjectCreated:CompleteMultipartUpload"
    S3ObjectCreatedCopy -> "s3:ObjectCreated:Copy"
    S3ObjectCreatedPost -> "s3:ObjectCreated:Post"
    S3ObjectCreatedPut -> "s3:ObjectCreated:Put"
    S3ObjectRemoved -> "s3:ObjectRemoved:*"
    S3ObjectRemovedDelete -> "s3:ObjectRemoved:Delete"
    S3ObjectRemovedDeleteMarkerCreated -> "s3:ObjectRemoved:DeleteMarkerCreated"
    S3ObjectRestore -> "s3:ObjectRestore:*"
    S3ObjectRestoreCompleted -> "s3:ObjectRestore:Completed"
    S3ObjectRestorePost -> "s3:ObjectRestore:Post"
    S3ReducedRedundancyLostObject -> "s3:ReducedRedundancyLostObject"
    S3Replication -> "s3:Replication:*"
    S3ReplicationOperationFailedReplication -> "s3:Replication:OperationFailedReplication"
    S3ReplicationOperationMissedThreshold -> "s3:Replication:OperationMissedThreshold"
    S3ReplicationOperationNotTracked -> "s3:Replication:OperationNotTracked"
    S3ReplicationOperationReplicatedAfterThreshold -> "s3:Replication:OperationReplicatedAfterThreshold"

instance Hashable Event

instance NFData Event

instance ToByteString Event

instance ToQuery Event

instance ToHeader Event

instance FromXML Event where
  parseXML = parseXMLText "Event"

instance ToXML Event where
  toXML = toXMLText
