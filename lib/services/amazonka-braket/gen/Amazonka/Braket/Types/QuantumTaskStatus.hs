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
-- Module      : Amazonka.Braket.Types.QuantumTaskStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Braket.Types.QuantumTaskStatus
  ( QuantumTaskStatus
      ( ..,
        QuantumTaskStatus_CANCELLED,
        QuantumTaskStatus_CANCELLING,
        QuantumTaskStatus_COMPLETED,
        QuantumTaskStatus_CREATED,
        QuantumTaskStatus_FAILED,
        QuantumTaskStatus_QUEUED,
        QuantumTaskStatus_RUNNING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype QuantumTaskStatus = QuantumTaskStatus'
  { fromQuantumTaskStatus ::
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

pattern QuantumTaskStatus_CANCELLED :: QuantumTaskStatus
pattern QuantumTaskStatus_CANCELLED = QuantumTaskStatus' "CANCELLED"

pattern QuantumTaskStatus_CANCELLING :: QuantumTaskStatus
pattern QuantumTaskStatus_CANCELLING = QuantumTaskStatus' "CANCELLING"

pattern QuantumTaskStatus_COMPLETED :: QuantumTaskStatus
pattern QuantumTaskStatus_COMPLETED = QuantumTaskStatus' "COMPLETED"

pattern QuantumTaskStatus_CREATED :: QuantumTaskStatus
pattern QuantumTaskStatus_CREATED = QuantumTaskStatus' "CREATED"

pattern QuantumTaskStatus_FAILED :: QuantumTaskStatus
pattern QuantumTaskStatus_FAILED = QuantumTaskStatus' "FAILED"

pattern QuantumTaskStatus_QUEUED :: QuantumTaskStatus
pattern QuantumTaskStatus_QUEUED = QuantumTaskStatus' "QUEUED"

pattern QuantumTaskStatus_RUNNING :: QuantumTaskStatus
pattern QuantumTaskStatus_RUNNING = QuantumTaskStatus' "RUNNING"

{-# COMPLETE
  QuantumTaskStatus_CANCELLED,
  QuantumTaskStatus_CANCELLING,
  QuantumTaskStatus_COMPLETED,
  QuantumTaskStatus_CREATED,
  QuantumTaskStatus_FAILED,
  QuantumTaskStatus_QUEUED,
  QuantumTaskStatus_RUNNING,
  QuantumTaskStatus'
  #-}
