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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype StepStatus = StepStatus'
  { fromStepStatus ::
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
