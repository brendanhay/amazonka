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
-- Module      : Network.AWS.FIS.Types.ExperimentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FIS.Types.ExperimentStatus
  ( ExperimentStatus
      ( ..,
        ExperimentStatus_Completed,
        ExperimentStatus_Failed,
        ExperimentStatus_Initiating,
        ExperimentStatus_Pending,
        ExperimentStatus_Running,
        ExperimentStatus_Stopped,
        ExperimentStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ExperimentStatus = ExperimentStatus'
  { fromExperimentStatus ::
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

pattern ExperimentStatus_Completed :: ExperimentStatus
pattern ExperimentStatus_Completed = ExperimentStatus' "completed"

pattern ExperimentStatus_Failed :: ExperimentStatus
pattern ExperimentStatus_Failed = ExperimentStatus' "failed"

pattern ExperimentStatus_Initiating :: ExperimentStatus
pattern ExperimentStatus_Initiating = ExperimentStatus' "initiating"

pattern ExperimentStatus_Pending :: ExperimentStatus
pattern ExperimentStatus_Pending = ExperimentStatus' "pending"

pattern ExperimentStatus_Running :: ExperimentStatus
pattern ExperimentStatus_Running = ExperimentStatus' "running"

pattern ExperimentStatus_Stopped :: ExperimentStatus
pattern ExperimentStatus_Stopped = ExperimentStatus' "stopped"

pattern ExperimentStatus_Stopping :: ExperimentStatus
pattern ExperimentStatus_Stopping = ExperimentStatus' "stopping"

{-# COMPLETE
  ExperimentStatus_Completed,
  ExperimentStatus_Failed,
  ExperimentStatus_Initiating,
  ExperimentStatus_Pending,
  ExperimentStatus_Running,
  ExperimentStatus_Stopped,
  ExperimentStatus_Stopping,
  ExperimentStatus'
  #-}
