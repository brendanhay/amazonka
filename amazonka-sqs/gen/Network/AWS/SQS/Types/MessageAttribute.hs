{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype MessageAttribute = MessageAttribute'
  { fromMessageAttribute ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
