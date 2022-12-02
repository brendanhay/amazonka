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
-- Module      : Amazonka.SQS.Types.QueueAttributeName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.QueueAttributeName
  ( QueueAttributeName
      ( ..,
        QueueAttributeName_All,
        QueueAttributeName_ApproximateNumberOfMessages,
        QueueAttributeName_ApproximateNumberOfMessagesDelayed,
        QueueAttributeName_ApproximateNumberOfMessagesNotVisible,
        QueueAttributeName_ContentBasedDeduplication,
        QueueAttributeName_CreatedTimestamp,
        QueueAttributeName_DeduplicationScope,
        QueueAttributeName_DelaySeconds,
        QueueAttributeName_FifoQueue,
        QueueAttributeName_FifoThroughputLimit,
        QueueAttributeName_KmsDataKeyReusePeriodSeconds,
        QueueAttributeName_KmsMasterKeyId,
        QueueAttributeName_LastModifiedTimestamp,
        QueueAttributeName_MaximumMessageSize,
        QueueAttributeName_MessageRetentionPeriod,
        QueueAttributeName_Policy,
        QueueAttributeName_QueueArn,
        QueueAttributeName_ReceiveMessageWaitTimeSeconds,
        QueueAttributeName_RedriveAllowPolicy,
        QueueAttributeName_RedrivePolicy,
        QueueAttributeName_SqsManagedSseEnabled,
        QueueAttributeName_VisibilityTimeout
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QueueAttributeName = QueueAttributeName'
  { fromQueueAttributeName ::
      Data.Text
  }
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

pattern QueueAttributeName_All :: QueueAttributeName
pattern QueueAttributeName_All = QueueAttributeName' "All"

pattern QueueAttributeName_ApproximateNumberOfMessages :: QueueAttributeName
pattern QueueAttributeName_ApproximateNumberOfMessages = QueueAttributeName' "ApproximateNumberOfMessages"

pattern QueueAttributeName_ApproximateNumberOfMessagesDelayed :: QueueAttributeName
pattern QueueAttributeName_ApproximateNumberOfMessagesDelayed = QueueAttributeName' "ApproximateNumberOfMessagesDelayed"

pattern QueueAttributeName_ApproximateNumberOfMessagesNotVisible :: QueueAttributeName
pattern QueueAttributeName_ApproximateNumberOfMessagesNotVisible = QueueAttributeName' "ApproximateNumberOfMessagesNotVisible"

pattern QueueAttributeName_ContentBasedDeduplication :: QueueAttributeName
pattern QueueAttributeName_ContentBasedDeduplication = QueueAttributeName' "ContentBasedDeduplication"

pattern QueueAttributeName_CreatedTimestamp :: QueueAttributeName
pattern QueueAttributeName_CreatedTimestamp = QueueAttributeName' "CreatedTimestamp"

pattern QueueAttributeName_DeduplicationScope :: QueueAttributeName
pattern QueueAttributeName_DeduplicationScope = QueueAttributeName' "DeduplicationScope"

pattern QueueAttributeName_DelaySeconds :: QueueAttributeName
pattern QueueAttributeName_DelaySeconds = QueueAttributeName' "DelaySeconds"

pattern QueueAttributeName_FifoQueue :: QueueAttributeName
pattern QueueAttributeName_FifoQueue = QueueAttributeName' "FifoQueue"

pattern QueueAttributeName_FifoThroughputLimit :: QueueAttributeName
pattern QueueAttributeName_FifoThroughputLimit = QueueAttributeName' "FifoThroughputLimit"

pattern QueueAttributeName_KmsDataKeyReusePeriodSeconds :: QueueAttributeName
pattern QueueAttributeName_KmsDataKeyReusePeriodSeconds = QueueAttributeName' "KmsDataKeyReusePeriodSeconds"

pattern QueueAttributeName_KmsMasterKeyId :: QueueAttributeName
pattern QueueAttributeName_KmsMasterKeyId = QueueAttributeName' "KmsMasterKeyId"

pattern QueueAttributeName_LastModifiedTimestamp :: QueueAttributeName
pattern QueueAttributeName_LastModifiedTimestamp = QueueAttributeName' "LastModifiedTimestamp"

pattern QueueAttributeName_MaximumMessageSize :: QueueAttributeName
pattern QueueAttributeName_MaximumMessageSize = QueueAttributeName' "MaximumMessageSize"

pattern QueueAttributeName_MessageRetentionPeriod :: QueueAttributeName
pattern QueueAttributeName_MessageRetentionPeriod = QueueAttributeName' "MessageRetentionPeriod"

pattern QueueAttributeName_Policy :: QueueAttributeName
pattern QueueAttributeName_Policy = QueueAttributeName' "Policy"

pattern QueueAttributeName_QueueArn :: QueueAttributeName
pattern QueueAttributeName_QueueArn = QueueAttributeName' "QueueArn"

pattern QueueAttributeName_ReceiveMessageWaitTimeSeconds :: QueueAttributeName
pattern QueueAttributeName_ReceiveMessageWaitTimeSeconds = QueueAttributeName' "ReceiveMessageWaitTimeSeconds"

pattern QueueAttributeName_RedriveAllowPolicy :: QueueAttributeName
pattern QueueAttributeName_RedriveAllowPolicy = QueueAttributeName' "RedriveAllowPolicy"

pattern QueueAttributeName_RedrivePolicy :: QueueAttributeName
pattern QueueAttributeName_RedrivePolicy = QueueAttributeName' "RedrivePolicy"

pattern QueueAttributeName_SqsManagedSseEnabled :: QueueAttributeName
pattern QueueAttributeName_SqsManagedSseEnabled = QueueAttributeName' "SqsManagedSseEnabled"

pattern QueueAttributeName_VisibilityTimeout :: QueueAttributeName
pattern QueueAttributeName_VisibilityTimeout = QueueAttributeName' "VisibilityTimeout"

{-# COMPLETE
  QueueAttributeName_All,
  QueueAttributeName_ApproximateNumberOfMessages,
  QueueAttributeName_ApproximateNumberOfMessagesDelayed,
  QueueAttributeName_ApproximateNumberOfMessagesNotVisible,
  QueueAttributeName_ContentBasedDeduplication,
  QueueAttributeName_CreatedTimestamp,
  QueueAttributeName_DeduplicationScope,
  QueueAttributeName_DelaySeconds,
  QueueAttributeName_FifoQueue,
  QueueAttributeName_FifoThroughputLimit,
  QueueAttributeName_KmsDataKeyReusePeriodSeconds,
  QueueAttributeName_KmsMasterKeyId,
  QueueAttributeName_LastModifiedTimestamp,
  QueueAttributeName_MaximumMessageSize,
  QueueAttributeName_MessageRetentionPeriod,
  QueueAttributeName_Policy,
  QueueAttributeName_QueueArn,
  QueueAttributeName_ReceiveMessageWaitTimeSeconds,
  QueueAttributeName_RedriveAllowPolicy,
  QueueAttributeName_RedrivePolicy,
  QueueAttributeName_SqsManagedSseEnabled,
  QueueAttributeName_VisibilityTimeout,
  QueueAttributeName'
  #-}
