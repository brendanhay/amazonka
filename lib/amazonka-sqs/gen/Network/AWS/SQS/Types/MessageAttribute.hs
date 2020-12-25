{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageAttribute
  ( MessageAttribute
      ( MessageAttribute',
        MessageAttributeAll,
        MessageAttributeApproximateFirstReceiveTimestamp,
        MessageAttributeApproximateReceiveCount,
        MessageAttributeSenderId,
        MessageAttributeSentTimestamp,
        fromMessageAttribute
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype MessageAttribute = MessageAttribute'
  { fromMessageAttribute ::
      Core.Text
  }
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

pattern MessageAttributeAll :: MessageAttribute
pattern MessageAttributeAll = MessageAttribute' "All"

pattern MessageAttributeApproximateFirstReceiveTimestamp :: MessageAttribute
pattern MessageAttributeApproximateFirstReceiveTimestamp = MessageAttribute' "ApproximateFirstReceiveTimestamp"

pattern MessageAttributeApproximateReceiveCount :: MessageAttribute
pattern MessageAttributeApproximateReceiveCount = MessageAttribute' "ApproximateReceiveCount"

pattern MessageAttributeSenderId :: MessageAttribute
pattern MessageAttributeSenderId = MessageAttribute' "SenderId"

pattern MessageAttributeSentTimestamp :: MessageAttribute
pattern MessageAttributeSentTimestamp = MessageAttribute' "SentTimestamp"

{-# COMPLETE
  MessageAttributeAll,
  MessageAttributeApproximateFirstReceiveTimestamp,
  MessageAttributeApproximateReceiveCount,
  MessageAttributeSenderId,
  MessageAttributeSentTimestamp,
  MessageAttribute'
  #-}
