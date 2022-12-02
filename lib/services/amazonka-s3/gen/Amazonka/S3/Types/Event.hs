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
-- Module      : Amazonka.S3.Types.Event
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.Event
  ( Event
      ( ..,
        Event_S3_IntelligentTiering,
        Event_S3_LifecycleExpiration_Delete,
        Event_S3_LifecycleExpiration_DeleteMarkerCreated,
        Event_S3_LifecycleExpiration__,
        Event_S3_LifecycleTransition,
        Event_S3_ObjectAcl_Put,
        Event_S3_ObjectCreated_CompleteMultipartUpload,
        Event_S3_ObjectCreated_Copy,
        Event_S3_ObjectCreated_Post,
        Event_S3_ObjectCreated_Put,
        Event_S3_ObjectCreated__,
        Event_S3_ObjectRemoved_Delete,
        Event_S3_ObjectRemoved_DeleteMarkerCreated,
        Event_S3_ObjectRemoved__,
        Event_S3_ObjectRestore_Completed,
        Event_S3_ObjectRestore_Delete,
        Event_S3_ObjectRestore_Post,
        Event_S3_ObjectRestore__,
        Event_S3_ObjectTagging_Delete,
        Event_S3_ObjectTagging_Put,
        Event_S3_ObjectTagging__,
        Event_S3_ReducedRedundancyLostObject,
        Event_S3_Replication_OperationFailedReplication,
        Event_S3_Replication_OperationMissedThreshold,
        Event_S3_Replication_OperationNotTracked,
        Event_S3_Replication_OperationReplicatedAfterThreshold,
        Event_S3_Replication__
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | The bucket event for which to send notifications.
newtype Event = Event' {fromEvent :: Data.Text}
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

pattern Event_S3_IntelligentTiering :: Event
pattern Event_S3_IntelligentTiering = Event' "s3:IntelligentTiering"

pattern Event_S3_LifecycleExpiration_Delete :: Event
pattern Event_S3_LifecycleExpiration_Delete = Event' "s3:LifecycleExpiration:Delete"

pattern Event_S3_LifecycleExpiration_DeleteMarkerCreated :: Event
pattern Event_S3_LifecycleExpiration_DeleteMarkerCreated = Event' "s3:LifecycleExpiration:DeleteMarkerCreated"

pattern Event_S3_LifecycleExpiration__ :: Event
pattern Event_S3_LifecycleExpiration__ = Event' "s3:LifecycleExpiration:*"

pattern Event_S3_LifecycleTransition :: Event
pattern Event_S3_LifecycleTransition = Event' "s3:LifecycleTransition"

pattern Event_S3_ObjectAcl_Put :: Event
pattern Event_S3_ObjectAcl_Put = Event' "s3:ObjectAcl:Put"

pattern Event_S3_ObjectCreated_CompleteMultipartUpload :: Event
pattern Event_S3_ObjectCreated_CompleteMultipartUpload = Event' "s3:ObjectCreated:CompleteMultipartUpload"

pattern Event_S3_ObjectCreated_Copy :: Event
pattern Event_S3_ObjectCreated_Copy = Event' "s3:ObjectCreated:Copy"

pattern Event_S3_ObjectCreated_Post :: Event
pattern Event_S3_ObjectCreated_Post = Event' "s3:ObjectCreated:Post"

pattern Event_S3_ObjectCreated_Put :: Event
pattern Event_S3_ObjectCreated_Put = Event' "s3:ObjectCreated:Put"

pattern Event_S3_ObjectCreated__ :: Event
pattern Event_S3_ObjectCreated__ = Event' "s3:ObjectCreated:*"

pattern Event_S3_ObjectRemoved_Delete :: Event
pattern Event_S3_ObjectRemoved_Delete = Event' "s3:ObjectRemoved:Delete"

pattern Event_S3_ObjectRemoved_DeleteMarkerCreated :: Event
pattern Event_S3_ObjectRemoved_DeleteMarkerCreated = Event' "s3:ObjectRemoved:DeleteMarkerCreated"

pattern Event_S3_ObjectRemoved__ :: Event
pattern Event_S3_ObjectRemoved__ = Event' "s3:ObjectRemoved:*"

pattern Event_S3_ObjectRestore_Completed :: Event
pattern Event_S3_ObjectRestore_Completed = Event' "s3:ObjectRestore:Completed"

pattern Event_S3_ObjectRestore_Delete :: Event
pattern Event_S3_ObjectRestore_Delete = Event' "s3:ObjectRestore:Delete"

pattern Event_S3_ObjectRestore_Post :: Event
pattern Event_S3_ObjectRestore_Post = Event' "s3:ObjectRestore:Post"

pattern Event_S3_ObjectRestore__ :: Event
pattern Event_S3_ObjectRestore__ = Event' "s3:ObjectRestore:*"

pattern Event_S3_ObjectTagging_Delete :: Event
pattern Event_S3_ObjectTagging_Delete = Event' "s3:ObjectTagging:Delete"

pattern Event_S3_ObjectTagging_Put :: Event
pattern Event_S3_ObjectTagging_Put = Event' "s3:ObjectTagging:Put"

pattern Event_S3_ObjectTagging__ :: Event
pattern Event_S3_ObjectTagging__ = Event' "s3:ObjectTagging:*"

pattern Event_S3_ReducedRedundancyLostObject :: Event
pattern Event_S3_ReducedRedundancyLostObject = Event' "s3:ReducedRedundancyLostObject"

pattern Event_S3_Replication_OperationFailedReplication :: Event
pattern Event_S3_Replication_OperationFailedReplication = Event' "s3:Replication:OperationFailedReplication"

pattern Event_S3_Replication_OperationMissedThreshold :: Event
pattern Event_S3_Replication_OperationMissedThreshold = Event' "s3:Replication:OperationMissedThreshold"

pattern Event_S3_Replication_OperationNotTracked :: Event
pattern Event_S3_Replication_OperationNotTracked = Event' "s3:Replication:OperationNotTracked"

pattern Event_S3_Replication_OperationReplicatedAfterThreshold :: Event
pattern Event_S3_Replication_OperationReplicatedAfterThreshold = Event' "s3:Replication:OperationReplicatedAfterThreshold"

pattern Event_S3_Replication__ :: Event
pattern Event_S3_Replication__ = Event' "s3:Replication:*"

{-# COMPLETE
  Event_S3_IntelligentTiering,
  Event_S3_LifecycleExpiration_Delete,
  Event_S3_LifecycleExpiration_DeleteMarkerCreated,
  Event_S3_LifecycleExpiration__,
  Event_S3_LifecycleTransition,
  Event_S3_ObjectAcl_Put,
  Event_S3_ObjectCreated_CompleteMultipartUpload,
  Event_S3_ObjectCreated_Copy,
  Event_S3_ObjectCreated_Post,
  Event_S3_ObjectCreated_Put,
  Event_S3_ObjectCreated__,
  Event_S3_ObjectRemoved_Delete,
  Event_S3_ObjectRemoved_DeleteMarkerCreated,
  Event_S3_ObjectRemoved__,
  Event_S3_ObjectRestore_Completed,
  Event_S3_ObjectRestore_Delete,
  Event_S3_ObjectRestore_Post,
  Event_S3_ObjectRestore__,
  Event_S3_ObjectTagging_Delete,
  Event_S3_ObjectTagging_Put,
  Event_S3_ObjectTagging__,
  Event_S3_ReducedRedundancyLostObject,
  Event_S3_Replication_OperationFailedReplication,
  Event_S3_Replication_OperationMissedThreshold,
  Event_S3_Replication_OperationNotTracked,
  Event_S3_Replication_OperationReplicatedAfterThreshold,
  Event_S3_Replication__,
  Event'
  #-}
