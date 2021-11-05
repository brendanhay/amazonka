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
-- Module      : Amazonka.MediaConvert.Types.QueueStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.QueueStatus
  ( QueueStatus
      ( ..,
        QueueStatus_ACTIVE,
        QueueStatus_PAUSED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Queues can be ACTIVE or PAUSED. If you pause a queue, jobs in that queue
-- won\'t begin. Jobs that are running when you pause a queue continue to
-- run until they finish or result in an error.
newtype QueueStatus = QueueStatus'
  { fromQueueStatus ::
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

pattern QueueStatus_ACTIVE :: QueueStatus
pattern QueueStatus_ACTIVE = QueueStatus' "ACTIVE"

pattern QueueStatus_PAUSED :: QueueStatus
pattern QueueStatus_PAUSED = QueueStatus' "PAUSED"

{-# COMPLETE
  QueueStatus_ACTIVE,
  QueueStatus_PAUSED,
  QueueStatus'
  #-}
