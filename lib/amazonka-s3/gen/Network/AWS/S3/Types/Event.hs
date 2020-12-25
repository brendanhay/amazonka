{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Event
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Event
  ( Event
      ( Event',
        EventS3ReducedRedundancyLostObject,
        EventS3ObjectCreated,
        EventS3ObjectCreatedPut,
        EventS3ObjectCreatedPost,
        EventS3ObjectCreatedCopy,
        EventS3ObjectCreatedCompleteMultipartUpload,
        EventS3ObjectRemoved,
        EventS3ObjectRemovedDelete,
        EventS3ObjectRemovedDeleteMarkerCreated,
        EventS3ObjectRestore,
        EventS3ObjectRestorePost,
        EventS3ObjectRestoreCompleted,
        EventS3Replication,
        EventS3ReplicationOperationFailedReplication,
        EventS3ReplicationOperationNotTracked,
        EventS3ReplicationOperationMissedThreshold,
        EventS3ReplicationOperationReplicatedAfterThreshold,
        fromEvent
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | The bucket event for which to send notifications.
newtype Event = Event' {fromEvent :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern EventS3ReducedRedundancyLostObject :: Event
pattern EventS3ReducedRedundancyLostObject = Event' "s3:ReducedRedundancyLostObject"

pattern EventS3ObjectCreated :: Event
pattern EventS3ObjectCreated = Event' "s3:ObjectCreated:*"

pattern EventS3ObjectCreatedPut :: Event
pattern EventS3ObjectCreatedPut = Event' "s3:ObjectCreated:Put"

pattern EventS3ObjectCreatedPost :: Event
pattern EventS3ObjectCreatedPost = Event' "s3:ObjectCreated:Post"

pattern EventS3ObjectCreatedCopy :: Event
pattern EventS3ObjectCreatedCopy = Event' "s3:ObjectCreated:Copy"

pattern EventS3ObjectCreatedCompleteMultipartUpload :: Event
pattern EventS3ObjectCreatedCompleteMultipartUpload = Event' "s3:ObjectCreated:CompleteMultipartUpload"

pattern EventS3ObjectRemoved :: Event
pattern EventS3ObjectRemoved = Event' "s3:ObjectRemoved:*"

pattern EventS3ObjectRemovedDelete :: Event
pattern EventS3ObjectRemovedDelete = Event' "s3:ObjectRemoved:Delete"

pattern EventS3ObjectRemovedDeleteMarkerCreated :: Event
pattern EventS3ObjectRemovedDeleteMarkerCreated = Event' "s3:ObjectRemoved:DeleteMarkerCreated"

pattern EventS3ObjectRestore :: Event
pattern EventS3ObjectRestore = Event' "s3:ObjectRestore:*"

pattern EventS3ObjectRestorePost :: Event
pattern EventS3ObjectRestorePost = Event' "s3:ObjectRestore:Post"

pattern EventS3ObjectRestoreCompleted :: Event
pattern EventS3ObjectRestoreCompleted = Event' "s3:ObjectRestore:Completed"

pattern EventS3Replication :: Event
pattern EventS3Replication = Event' "s3:Replication:*"

pattern EventS3ReplicationOperationFailedReplication :: Event
pattern EventS3ReplicationOperationFailedReplication = Event' "s3:Replication:OperationFailedReplication"

pattern EventS3ReplicationOperationNotTracked :: Event
pattern EventS3ReplicationOperationNotTracked = Event' "s3:Replication:OperationNotTracked"

pattern EventS3ReplicationOperationMissedThreshold :: Event
pattern EventS3ReplicationOperationMissedThreshold = Event' "s3:Replication:OperationMissedThreshold"

pattern EventS3ReplicationOperationReplicatedAfterThreshold :: Event
pattern EventS3ReplicationOperationReplicatedAfterThreshold = Event' "s3:Replication:OperationReplicatedAfterThreshold"

{-# COMPLETE
  EventS3ReducedRedundancyLostObject,
  EventS3ObjectCreated,
  EventS3ObjectCreatedPut,
  EventS3ObjectCreatedPost,
  EventS3ObjectCreatedCopy,
  EventS3ObjectCreatedCompleteMultipartUpload,
  EventS3ObjectRemoved,
  EventS3ObjectRemovedDelete,
  EventS3ObjectRemovedDeleteMarkerCreated,
  EventS3ObjectRestore,
  EventS3ObjectRestorePost,
  EventS3ObjectRestoreCompleted,
  EventS3Replication,
  EventS3ReplicationOperationFailedReplication,
  EventS3ReplicationOperationNotTracked,
  EventS3ReplicationOperationMissedThreshold,
  EventS3ReplicationOperationReplicatedAfterThreshold,
  Event'
  #-}
