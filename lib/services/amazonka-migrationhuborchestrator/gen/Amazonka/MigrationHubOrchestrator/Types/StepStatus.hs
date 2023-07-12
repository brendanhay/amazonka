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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.StepStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.StepStatus
  ( StepStatus
      ( ..,
        StepStatus_AWAITING_DEPENDENCIES,
        StepStatus_COMPLETED,
        StepStatus_FAILED,
        StepStatus_IN_PROGRESS,
        StepStatus_PAUSED,
        StepStatus_READY,
        StepStatus_USER_ATTENTION_REQUIRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StepStatus = StepStatus'
  { fromStepStatus ::
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

pattern StepStatus_AWAITING_DEPENDENCIES :: StepStatus
pattern StepStatus_AWAITING_DEPENDENCIES = StepStatus' "AWAITING_DEPENDENCIES"

pattern StepStatus_COMPLETED :: StepStatus
pattern StepStatus_COMPLETED = StepStatus' "COMPLETED"

pattern StepStatus_FAILED :: StepStatus
pattern StepStatus_FAILED = StepStatus' "FAILED"

pattern StepStatus_IN_PROGRESS :: StepStatus
pattern StepStatus_IN_PROGRESS = StepStatus' "IN_PROGRESS"

pattern StepStatus_PAUSED :: StepStatus
pattern StepStatus_PAUSED = StepStatus' "PAUSED"

pattern StepStatus_READY :: StepStatus
pattern StepStatus_READY = StepStatus' "READY"

pattern StepStatus_USER_ATTENTION_REQUIRED :: StepStatus
pattern StepStatus_USER_ATTENTION_REQUIRED = StepStatus' "USER_ATTENTION_REQUIRED"

{-# COMPLETE
  StepStatus_AWAITING_DEPENDENCIES,
  StepStatus_COMPLETED,
  StepStatus_FAILED,
  StepStatus_IN_PROGRESS,
  StepStatus_PAUSED,
  StepStatus_READY,
  StepStatus_USER_ATTENTION_REQUIRED,
  StepStatus'
  #-}
