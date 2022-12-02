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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.StepGroupStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.StepGroupStatus
  ( StepGroupStatus
      ( ..,
        StepGroupStatus_AWAITING_DEPENDENCIES,
        StepGroupStatus_COMPLETED,
        StepGroupStatus_FAILED,
        StepGroupStatus_IN_PROGRESS,
        StepGroupStatus_PAUSED,
        StepGroupStatus_PAUSING,
        StepGroupStatus_READY,
        StepGroupStatus_USER_ATTENTION_REQUIRED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StepGroupStatus = StepGroupStatus'
  { fromStepGroupStatus ::
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

pattern StepGroupStatus_AWAITING_DEPENDENCIES :: StepGroupStatus
pattern StepGroupStatus_AWAITING_DEPENDENCIES = StepGroupStatus' "AWAITING_DEPENDENCIES"

pattern StepGroupStatus_COMPLETED :: StepGroupStatus
pattern StepGroupStatus_COMPLETED = StepGroupStatus' "COMPLETED"

pattern StepGroupStatus_FAILED :: StepGroupStatus
pattern StepGroupStatus_FAILED = StepGroupStatus' "FAILED"

pattern StepGroupStatus_IN_PROGRESS :: StepGroupStatus
pattern StepGroupStatus_IN_PROGRESS = StepGroupStatus' "IN_PROGRESS"

pattern StepGroupStatus_PAUSED :: StepGroupStatus
pattern StepGroupStatus_PAUSED = StepGroupStatus' "PAUSED"

pattern StepGroupStatus_PAUSING :: StepGroupStatus
pattern StepGroupStatus_PAUSING = StepGroupStatus' "PAUSING"

pattern StepGroupStatus_READY :: StepGroupStatus
pattern StepGroupStatus_READY = StepGroupStatus' "READY"

pattern StepGroupStatus_USER_ATTENTION_REQUIRED :: StepGroupStatus
pattern StepGroupStatus_USER_ATTENTION_REQUIRED = StepGroupStatus' "USER_ATTENTION_REQUIRED"

{-# COMPLETE
  StepGroupStatus_AWAITING_DEPENDENCIES,
  StepGroupStatus_COMPLETED,
  StepGroupStatus_FAILED,
  StepGroupStatus_IN_PROGRESS,
  StepGroupStatus_PAUSED,
  StepGroupStatus_PAUSING,
  StepGroupStatus_READY,
  StepGroupStatus_USER_ATTENTION_REQUIRED,
  StepGroupStatus'
  #-}
