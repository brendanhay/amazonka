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
-- Module      : Network.AWS.SQS.Types.MessageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageAttribute
  ( MessageAttribute
      ( ..,
        MessageAttribute_All,
        MessageAttribute_ApproximateFirstReceiveTimestamp,
        MessageAttribute_ApproximateReceiveCount,
        MessageAttribute_SenderId,
        MessageAttribute_SentTimestamp
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MessageAttribute = MessageAttribute'
  { fromMessageAttribute ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MessageAttribute_All :: MessageAttribute
pattern MessageAttribute_All = MessageAttribute' "All"

pattern MessageAttribute_ApproximateFirstReceiveTimestamp :: MessageAttribute
pattern MessageAttribute_ApproximateFirstReceiveTimestamp = MessageAttribute' "ApproximateFirstReceiveTimestamp"

pattern MessageAttribute_ApproximateReceiveCount :: MessageAttribute
pattern MessageAttribute_ApproximateReceiveCount = MessageAttribute' "ApproximateReceiveCount"

pattern MessageAttribute_SenderId :: MessageAttribute
pattern MessageAttribute_SenderId = MessageAttribute' "SenderId"

pattern MessageAttribute_SentTimestamp :: MessageAttribute
pattern MessageAttribute_SentTimestamp = MessageAttribute' "SentTimestamp"

{-# COMPLETE
  MessageAttribute_All,
  MessageAttribute_ApproximateFirstReceiveTimestamp,
  MessageAttribute_ApproximateReceiveCount,
  MessageAttribute_SenderId,
  MessageAttribute_SentTimestamp,
  MessageAttribute'
  #-}
