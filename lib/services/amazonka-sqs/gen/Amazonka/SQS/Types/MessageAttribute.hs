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
-- Module      : Amazonka.SQS.Types.MessageAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.MessageAttribute
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype MessageAttribute = MessageAttribute'
  { fromMessageAttribute ::
      Core.Text
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
