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
-- Module      : Amazonka.FIS.Types.ExperimentActionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentActionStatus
  ( ExperimentActionStatus
      ( ..,
        ExperimentActionStatus_Cancelled,
        ExperimentActionStatus_Completed,
        ExperimentActionStatus_Failed,
        ExperimentActionStatus_Initiating,
        ExperimentActionStatus_Pending,
        ExperimentActionStatus_Running,
        ExperimentActionStatus_Stopped,
        ExperimentActionStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExperimentActionStatus = ExperimentActionStatus'
  { fromExperimentActionStatus ::
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

pattern ExperimentActionStatus_Cancelled :: ExperimentActionStatus
pattern ExperimentActionStatus_Cancelled = ExperimentActionStatus' "cancelled"

pattern ExperimentActionStatus_Completed :: ExperimentActionStatus
pattern ExperimentActionStatus_Completed = ExperimentActionStatus' "completed"

pattern ExperimentActionStatus_Failed :: ExperimentActionStatus
pattern ExperimentActionStatus_Failed = ExperimentActionStatus' "failed"

pattern ExperimentActionStatus_Initiating :: ExperimentActionStatus
pattern ExperimentActionStatus_Initiating = ExperimentActionStatus' "initiating"

pattern ExperimentActionStatus_Pending :: ExperimentActionStatus
pattern ExperimentActionStatus_Pending = ExperimentActionStatus' "pending"

pattern ExperimentActionStatus_Running :: ExperimentActionStatus
pattern ExperimentActionStatus_Running = ExperimentActionStatus' "running"

pattern ExperimentActionStatus_Stopped :: ExperimentActionStatus
pattern ExperimentActionStatus_Stopped = ExperimentActionStatus' "stopped"

pattern ExperimentActionStatus_Stopping :: ExperimentActionStatus
pattern ExperimentActionStatus_Stopping = ExperimentActionStatus' "stopping"

{-# COMPLETE
  ExperimentActionStatus_Cancelled,
  ExperimentActionStatus_Completed,
  ExperimentActionStatus_Failed,
  ExperimentActionStatus_Initiating,
  ExperimentActionStatus_Pending,
  ExperimentActionStatus_Running,
  ExperimentActionStatus_Stopped,
  ExperimentActionStatus_Stopping,
  ExperimentActionStatus'
  #-}
