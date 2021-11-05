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
-- Module      : Amazonka.IoT.Types.DetectMitigationActionExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DetectMitigationActionExecutionStatus
  ( DetectMitigationActionExecutionStatus
      ( ..,
        DetectMitigationActionExecutionStatus_FAILED,
        DetectMitigationActionExecutionStatus_IN_PROGRESS,
        DetectMitigationActionExecutionStatus_SKIPPED,
        DetectMitigationActionExecutionStatus_SUCCESSFUL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DetectMitigationActionExecutionStatus = DetectMitigationActionExecutionStatus'
  { fromDetectMitigationActionExecutionStatus ::
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

pattern DetectMitigationActionExecutionStatus_FAILED :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_FAILED = DetectMitigationActionExecutionStatus' "FAILED"

pattern DetectMitigationActionExecutionStatus_IN_PROGRESS :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_IN_PROGRESS = DetectMitigationActionExecutionStatus' "IN_PROGRESS"

pattern DetectMitigationActionExecutionStatus_SKIPPED :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_SKIPPED = DetectMitigationActionExecutionStatus' "SKIPPED"

pattern DetectMitigationActionExecutionStatus_SUCCESSFUL :: DetectMitigationActionExecutionStatus
pattern DetectMitigationActionExecutionStatus_SUCCESSFUL = DetectMitigationActionExecutionStatus' "SUCCESSFUL"

{-# COMPLETE
  DetectMitigationActionExecutionStatus_FAILED,
  DetectMitigationActionExecutionStatus_IN_PROGRESS,
  DetectMitigationActionExecutionStatus_SKIPPED,
  DetectMitigationActionExecutionStatus_SUCCESSFUL,
  DetectMitigationActionExecutionStatus'
  #-}
