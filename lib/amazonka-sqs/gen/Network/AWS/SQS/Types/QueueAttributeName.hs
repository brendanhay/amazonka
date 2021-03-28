{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.QueueAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SQS.Types.QueueAttributeName
  ( QueueAttributeName
    ( QueueAttributeName'
    , QueueAttributeNameAll
    , QueueAttributeNamePolicy
    , QueueAttributeNameVisibilityTimeout
    , QueueAttributeNameMaximumMessageSize
    , QueueAttributeNameMessageRetentionPeriod
    , QueueAttributeNameApproximateNumberOfMessages
    , QueueAttributeNameApproximateNumberOfMessagesNotVisible
    , QueueAttributeNameCreatedTimestamp
    , QueueAttributeNameLastModifiedTimestamp
    , QueueAttributeNameQueueArn
    , QueueAttributeNameApproximateNumberOfMessagesDelayed
    , QueueAttributeNameDelaySeconds
    , QueueAttributeNameReceiveMessageWaitTimeSeconds
    , QueueAttributeNameRedrivePolicy
    , QueueAttributeNameFifoQueue
    , QueueAttributeNameContentBasedDeduplication
    , QueueAttributeNameKmsMasterKeyId
    , QueueAttributeNameKmsDataKeyReusePeriodSeconds
    , fromQueueAttributeName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype QueueAttributeName = QueueAttributeName'{fromQueueAttributeName
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern QueueAttributeNameAll :: QueueAttributeName
pattern QueueAttributeNameAll = QueueAttributeName' "All"

pattern QueueAttributeNamePolicy :: QueueAttributeName
pattern QueueAttributeNamePolicy = QueueAttributeName' "Policy"

pattern QueueAttributeNameVisibilityTimeout :: QueueAttributeName
pattern QueueAttributeNameVisibilityTimeout = QueueAttributeName' "VisibilityTimeout"

pattern QueueAttributeNameMaximumMessageSize :: QueueAttributeName
pattern QueueAttributeNameMaximumMessageSize = QueueAttributeName' "MaximumMessageSize"

pattern QueueAttributeNameMessageRetentionPeriod :: QueueAttributeName
pattern QueueAttributeNameMessageRetentionPeriod = QueueAttributeName' "MessageRetentionPeriod"

pattern QueueAttributeNameApproximateNumberOfMessages :: QueueAttributeName
pattern QueueAttributeNameApproximateNumberOfMessages = QueueAttributeName' "ApproximateNumberOfMessages"

pattern QueueAttributeNameApproximateNumberOfMessagesNotVisible :: QueueAttributeName
pattern QueueAttributeNameApproximateNumberOfMessagesNotVisible = QueueAttributeName' "ApproximateNumberOfMessagesNotVisible"

pattern QueueAttributeNameCreatedTimestamp :: QueueAttributeName
pattern QueueAttributeNameCreatedTimestamp = QueueAttributeName' "CreatedTimestamp"

pattern QueueAttributeNameLastModifiedTimestamp :: QueueAttributeName
pattern QueueAttributeNameLastModifiedTimestamp = QueueAttributeName' "LastModifiedTimestamp"

pattern QueueAttributeNameQueueArn :: QueueAttributeName
pattern QueueAttributeNameQueueArn = QueueAttributeName' "QueueArn"

pattern QueueAttributeNameApproximateNumberOfMessagesDelayed :: QueueAttributeName
pattern QueueAttributeNameApproximateNumberOfMessagesDelayed = QueueAttributeName' "ApproximateNumberOfMessagesDelayed"

pattern QueueAttributeNameDelaySeconds :: QueueAttributeName
pattern QueueAttributeNameDelaySeconds = QueueAttributeName' "DelaySeconds"

pattern QueueAttributeNameReceiveMessageWaitTimeSeconds :: QueueAttributeName
pattern QueueAttributeNameReceiveMessageWaitTimeSeconds = QueueAttributeName' "ReceiveMessageWaitTimeSeconds"

pattern QueueAttributeNameRedrivePolicy :: QueueAttributeName
pattern QueueAttributeNameRedrivePolicy = QueueAttributeName' "RedrivePolicy"

pattern QueueAttributeNameFifoQueue :: QueueAttributeName
pattern QueueAttributeNameFifoQueue = QueueAttributeName' "FifoQueue"

pattern QueueAttributeNameContentBasedDeduplication :: QueueAttributeName
pattern QueueAttributeNameContentBasedDeduplication = QueueAttributeName' "ContentBasedDeduplication"

pattern QueueAttributeNameKmsMasterKeyId :: QueueAttributeName
pattern QueueAttributeNameKmsMasterKeyId = QueueAttributeName' "KmsMasterKeyId"

pattern QueueAttributeNameKmsDataKeyReusePeriodSeconds :: QueueAttributeName
pattern QueueAttributeNameKmsDataKeyReusePeriodSeconds = QueueAttributeName' "KmsDataKeyReusePeriodSeconds"

{-# COMPLETE 
  QueueAttributeNameAll,

  QueueAttributeNamePolicy,

  QueueAttributeNameVisibilityTimeout,

  QueueAttributeNameMaximumMessageSize,

  QueueAttributeNameMessageRetentionPeriod,

  QueueAttributeNameApproximateNumberOfMessages,

  QueueAttributeNameApproximateNumberOfMessagesNotVisible,

  QueueAttributeNameCreatedTimestamp,

  QueueAttributeNameLastModifiedTimestamp,

  QueueAttributeNameQueueArn,

  QueueAttributeNameApproximateNumberOfMessagesDelayed,

  QueueAttributeNameDelaySeconds,

  QueueAttributeNameReceiveMessageWaitTimeSeconds,

  QueueAttributeNameRedrivePolicy,

  QueueAttributeNameFifoQueue,

  QueueAttributeNameContentBasedDeduplication,

  QueueAttributeNameKmsMasterKeyId,

  QueueAttributeNameKmsDataKeyReusePeriodSeconds,
  QueueAttributeName'
  #-}
