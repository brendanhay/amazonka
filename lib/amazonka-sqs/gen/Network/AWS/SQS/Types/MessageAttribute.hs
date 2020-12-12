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
        All,
        ApproximateFirstReceiveTimestamp,
        ApproximateReceiveCount,
        SenderId,
        SentTimestamp
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MessageAttribute = MessageAttribute' Lude.Text
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

pattern All :: MessageAttribute
pattern All = MessageAttribute' "All"

pattern ApproximateFirstReceiveTimestamp :: MessageAttribute
pattern ApproximateFirstReceiveTimestamp = MessageAttribute' "ApproximateFirstReceiveTimestamp"

pattern ApproximateReceiveCount :: MessageAttribute
pattern ApproximateReceiveCount = MessageAttribute' "ApproximateReceiveCount"

pattern SenderId :: MessageAttribute
pattern SenderId = MessageAttribute' "SenderId"

pattern SentTimestamp :: MessageAttribute
pattern SentTimestamp = MessageAttribute' "SentTimestamp"

{-# COMPLETE
  All,
  ApproximateFirstReceiveTimestamp,
  ApproximateReceiveCount,
  SenderId,
  SentTimestamp,
  MessageAttribute'
  #-}
