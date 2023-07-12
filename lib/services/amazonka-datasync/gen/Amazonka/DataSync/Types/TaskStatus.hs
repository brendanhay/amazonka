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
-- Module      : Amazonka.DataSync.Types.TaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.TaskStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TaskStatus = TaskStatus'
  { fromTaskStatus ::
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
