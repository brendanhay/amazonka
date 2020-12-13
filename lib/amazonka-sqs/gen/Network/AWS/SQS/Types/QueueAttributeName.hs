{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.QueueAttributeName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.QueueAttributeName
  ( QueueAttributeName
      ( QueueAttributeName',
        QANAll,
        QANPolicy,
        QANVisibilityTimeout,
        QANMaximumMessageSize,
        QANMessageRetentionPeriod,
        QANApproximateNumberOfMessages,
        QANApproximateNumberOfMessagesNotVisible,
        QANCreatedTimestamp,
        QANLastModifiedTimestamp,
        QANQueueARN,
        QANApproximateNumberOfMessagesDelayed,
        QANDelaySeconds,
        QANReceiveMessageWaitTimeSeconds,
        QANRedrivePolicy,
        QANFifoQueue,
        QANContentBasedDeduplication,
        QANKMSMasterKeyId,
        QANKMSDataKeyReusePeriodSeconds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype QueueAttributeName = QueueAttributeName' Lude.Text
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

pattern QANAll :: QueueAttributeName
pattern QANAll = QueueAttributeName' "All"

pattern QANPolicy :: QueueAttributeName
pattern QANPolicy = QueueAttributeName' "Policy"

pattern QANVisibilityTimeout :: QueueAttributeName
pattern QANVisibilityTimeout = QueueAttributeName' "VisibilityTimeout"

pattern QANMaximumMessageSize :: QueueAttributeName
pattern QANMaximumMessageSize = QueueAttributeName' "MaximumMessageSize"

pattern QANMessageRetentionPeriod :: QueueAttributeName
pattern QANMessageRetentionPeriod = QueueAttributeName' "MessageRetentionPeriod"

pattern QANApproximateNumberOfMessages :: QueueAttributeName
pattern QANApproximateNumberOfMessages = QueueAttributeName' "ApproximateNumberOfMessages"

pattern QANApproximateNumberOfMessagesNotVisible :: QueueAttributeName
pattern QANApproximateNumberOfMessagesNotVisible = QueueAttributeName' "ApproximateNumberOfMessagesNotVisible"

pattern QANCreatedTimestamp :: QueueAttributeName
pattern QANCreatedTimestamp = QueueAttributeName' "CreatedTimestamp"

pattern QANLastModifiedTimestamp :: QueueAttributeName
pattern QANLastModifiedTimestamp = QueueAttributeName' "LastModifiedTimestamp"

pattern QANQueueARN :: QueueAttributeName
pattern QANQueueARN = QueueAttributeName' "QueueArn"

pattern QANApproximateNumberOfMessagesDelayed :: QueueAttributeName
pattern QANApproximateNumberOfMessagesDelayed = QueueAttributeName' "ApproximateNumberOfMessagesDelayed"

pattern QANDelaySeconds :: QueueAttributeName
pattern QANDelaySeconds = QueueAttributeName' "DelaySeconds"

pattern QANReceiveMessageWaitTimeSeconds :: QueueAttributeName
pattern QANReceiveMessageWaitTimeSeconds = QueueAttributeName' "ReceiveMessageWaitTimeSeconds"

pattern QANRedrivePolicy :: QueueAttributeName
pattern QANRedrivePolicy = QueueAttributeName' "RedrivePolicy"

pattern QANFifoQueue :: QueueAttributeName
pattern QANFifoQueue = QueueAttributeName' "FifoQueue"

pattern QANContentBasedDeduplication :: QueueAttributeName
pattern QANContentBasedDeduplication = QueueAttributeName' "ContentBasedDeduplication"

pattern QANKMSMasterKeyId :: QueueAttributeName
pattern QANKMSMasterKeyId = QueueAttributeName' "KmsMasterKeyId"

pattern QANKMSDataKeyReusePeriodSeconds :: QueueAttributeName
pattern QANKMSDataKeyReusePeriodSeconds = QueueAttributeName' "KmsDataKeyReusePeriodSeconds"

{-# COMPLETE
  QANAll,
  QANPolicy,
  QANVisibilityTimeout,
  QANMaximumMessageSize,
  QANMessageRetentionPeriod,
  QANApproximateNumberOfMessages,
  QANApproximateNumberOfMessagesNotVisible,
  QANCreatedTimestamp,
  QANLastModifiedTimestamp,
  QANQueueARN,
  QANApproximateNumberOfMessagesDelayed,
  QANDelaySeconds,
  QANReceiveMessageWaitTimeSeconds,
  QANRedrivePolicy,
  QANFifoQueue,
  QANContentBasedDeduplication,
  QANKMSMasterKeyId,
  QANKMSDataKeyReusePeriodSeconds,
  QueueAttributeName'
  #-}
