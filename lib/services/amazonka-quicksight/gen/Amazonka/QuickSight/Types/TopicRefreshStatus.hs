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
-- Module      : Amazonka.QuickSight.Types.TopicRefreshStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRefreshStatus
  ( TopicRefreshStatus
      ( ..,
        TopicRefreshStatus_CANCELLED,
        TopicRefreshStatus_COMPLETED,
        TopicRefreshStatus_FAILED,
        TopicRefreshStatus_INITIALIZED,
        TopicRefreshStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TopicRefreshStatus = TopicRefreshStatus'
  { fromTopicRefreshStatus ::
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

pattern TopicRefreshStatus_CANCELLED :: TopicRefreshStatus
pattern TopicRefreshStatus_CANCELLED = TopicRefreshStatus' "CANCELLED"

pattern TopicRefreshStatus_COMPLETED :: TopicRefreshStatus
pattern TopicRefreshStatus_COMPLETED = TopicRefreshStatus' "COMPLETED"

pattern TopicRefreshStatus_FAILED :: TopicRefreshStatus
pattern TopicRefreshStatus_FAILED = TopicRefreshStatus' "FAILED"

pattern TopicRefreshStatus_INITIALIZED :: TopicRefreshStatus
pattern TopicRefreshStatus_INITIALIZED = TopicRefreshStatus' "INITIALIZED"

pattern TopicRefreshStatus_RUNNING :: TopicRefreshStatus
pattern TopicRefreshStatus_RUNNING = TopicRefreshStatus' "RUNNING"

{-# COMPLETE
  TopicRefreshStatus_CANCELLED,
  TopicRefreshStatus_COMPLETED,
  TopicRefreshStatus_FAILED,
  TopicRefreshStatus_INITIALIZED,
  TopicRefreshStatus_RUNNING,
  TopicRefreshStatus'
  #-}
