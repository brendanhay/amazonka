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
        QANApproximateNumberOfMessages,
        QANApproximateNumberOfMessagesDelayed,
        QANApproximateNumberOfMessagesNotVisible,
        QANContentBasedDeduplication,
        QANCreatedTimestamp,
        QANDelaySeconds,
        QANFifoQueue,
        QANKMSDataKeyReusePeriodSeconds,
        QANKMSMasterKeyId,
        QANLastModifiedTimestamp,
        QANMaximumMessageSize,
        QANMessageRetentionPeriod,
        QANPolicy,
        QANQueueARN,
        QANReceiveMessageWaitTimeSeconds,
        QANRedrivePolicy,
        QANVisibilityTimeout
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

pattern QANApproximateNumberOfMessages :: QueueAttributeName
pattern QANApproximateNumberOfMessages = QueueAttributeName' "ApproximateNumberOfMessages"

pattern QANApproximateNumberOfMessagesDelayed :: QueueAttributeName
pattern QANApproximateNumberOfMessagesDelayed = QueueAttributeName' "ApproximateNumberOfMessagesDelayed"

pattern QANApproximateNumberOfMessagesNotVisible :: QueueAttributeName
pattern QANApproximateNumberOfMessagesNotVisible = QueueAttributeName' "ApproximateNumberOfMessagesNotVisible"

pattern QANContentBasedDeduplication :: QueueAttributeName
pattern QANContentBasedDeduplication = QueueAttributeName' "ContentBasedDeduplication"

pattern QANCreatedTimestamp :: QueueAttributeName
pattern QANCreatedTimestamp = QueueAttributeName' "CreatedTimestamp"

pattern QANDelaySeconds :: QueueAttributeName
pattern QANDelaySeconds = QueueAttributeName' "DelaySeconds"

pattern QANFifoQueue :: QueueAttributeName
pattern QANFifoQueue = QueueAttributeName' "FifoQueue"

pattern QANKMSDataKeyReusePeriodSeconds :: QueueAttributeName
pattern QANKMSDataKeyReusePeriodSeconds = QueueAttributeName' "KmsDataKeyReusePeriodSeconds"

pattern QANKMSMasterKeyId :: QueueAttributeName
pattern QANKMSMasterKeyId = QueueAttributeName' "KmsMasterKeyId"

pattern QANLastModifiedTimestamp :: QueueAttributeName
pattern QANLastModifiedTimestamp = QueueAttributeName' "LastModifiedTimestamp"

pattern QANMaximumMessageSize :: QueueAttributeName
pattern QANMaximumMessageSize = QueueAttributeName' "MaximumMessageSize"

pattern QANMessageRetentionPeriod :: QueueAttributeName
pattern QANMessageRetentionPeriod = QueueAttributeName' "MessageRetentionPeriod"

pattern QANPolicy :: QueueAttributeName
pattern QANPolicy = QueueAttributeName' "Policy"

pattern QANQueueARN :: QueueAttributeName
pattern QANQueueARN = QueueAttributeName' "QueueArn"

pattern QANReceiveMessageWaitTimeSeconds :: QueueAttributeName
pattern QANReceiveMessageWaitTimeSeconds = QueueAttributeName' "ReceiveMessageWaitTimeSeconds"

pattern QANRedrivePolicy :: QueueAttributeName
pattern QANRedrivePolicy = QueueAttributeName' "RedrivePolicy"

pattern QANVisibilityTimeout :: QueueAttributeName
pattern QANVisibilityTimeout = QueueAttributeName' "VisibilityTimeout"

{-# COMPLETE
  QANAll,
  QANApproximateNumberOfMessages,
  QANApproximateNumberOfMessagesDelayed,
  QANApproximateNumberOfMessagesNotVisible,
  QANContentBasedDeduplication,
  QANCreatedTimestamp,
  QANDelaySeconds,
  QANFifoQueue,
  QANKMSDataKeyReusePeriodSeconds,
  QANKMSMasterKeyId,
  QANLastModifiedTimestamp,
  QANMaximumMessageSize,
  QANMessageRetentionPeriod,
  QANPolicy,
  QANQueueARN,
  QANReceiveMessageWaitTimeSeconds,
  QANRedrivePolicy,
  QANVisibilityTimeout,
  QueueAttributeName'
  #-}
