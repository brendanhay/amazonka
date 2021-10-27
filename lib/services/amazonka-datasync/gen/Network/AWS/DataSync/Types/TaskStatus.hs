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
-- Module      : Network.AWS.DataSync.Types.TaskStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataSync.Types.TaskStatus
  ( TaskStatus
      ( ..,
        TaskStatus_AVAILABLE,
        TaskStatus_CREATING,
        TaskStatus_QUEUED,
        TaskStatus_RUNNING,
        TaskStatus_UNAVAILABLE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype TaskStatus = TaskStatus'
  { fromTaskStatus ::
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

pattern TaskStatus_AVAILABLE :: TaskStatus
pattern TaskStatus_AVAILABLE = TaskStatus' "AVAILABLE"

pattern TaskStatus_CREATING :: TaskStatus
pattern TaskStatus_CREATING = TaskStatus' "CREATING"

pattern TaskStatus_QUEUED :: TaskStatus
pattern TaskStatus_QUEUED = TaskStatus' "QUEUED"

pattern TaskStatus_RUNNING :: TaskStatus
pattern TaskStatus_RUNNING = TaskStatus' "RUNNING"

pattern TaskStatus_UNAVAILABLE :: TaskStatus
pattern TaskStatus_UNAVAILABLE = TaskStatus' "UNAVAILABLE"

{-# COMPLETE
  TaskStatus_AVAILABLE,
  TaskStatus_CREATING,
  TaskStatus_QUEUED,
  TaskStatus_RUNNING,
  TaskStatus_UNAVAILABLE,
  TaskStatus'
  #-}
