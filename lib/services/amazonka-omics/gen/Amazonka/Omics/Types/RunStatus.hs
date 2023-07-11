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
-- Module      : Amazonka.Omics.Types.RunStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.RunStatus
  ( RunStatus
      ( ..,
        RunStatus_CANCELLED,
        RunStatus_COMPLETED,
        RunStatus_DELETED,
        RunStatus_FAILED,
        RunStatus_PENDING,
        RunStatus_RUNNING,
        RunStatus_STARTING,
        RunStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RunStatus = RunStatus'
  { fromRunStatus ::
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

pattern RunStatus_CANCELLED :: RunStatus
pattern RunStatus_CANCELLED = RunStatus' "CANCELLED"

pattern RunStatus_COMPLETED :: RunStatus
pattern RunStatus_COMPLETED = RunStatus' "COMPLETED"

pattern RunStatus_DELETED :: RunStatus
pattern RunStatus_DELETED = RunStatus' "DELETED"

pattern RunStatus_FAILED :: RunStatus
pattern RunStatus_FAILED = RunStatus' "FAILED"

pattern RunStatus_PENDING :: RunStatus
pattern RunStatus_PENDING = RunStatus' "PENDING"

pattern RunStatus_RUNNING :: RunStatus
pattern RunStatus_RUNNING = RunStatus' "RUNNING"

pattern RunStatus_STARTING :: RunStatus
pattern RunStatus_STARTING = RunStatus' "STARTING"

pattern RunStatus_STOPPING :: RunStatus
pattern RunStatus_STOPPING = RunStatus' "STOPPING"

{-# COMPLETE
  RunStatus_CANCELLED,
  RunStatus_COMPLETED,
  RunStatus_DELETED,
  RunStatus_FAILED,
  RunStatus_PENDING,
  RunStatus_RUNNING,
  RunStatus_STARTING,
  RunStatus_STOPPING,
  RunStatus'
  #-}
