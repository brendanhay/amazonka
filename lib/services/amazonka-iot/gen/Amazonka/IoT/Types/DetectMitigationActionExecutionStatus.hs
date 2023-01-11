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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DetectMitigationActionExecutionStatus = DetectMitigationActionExecutionStatus'
  { fromDetectMitigationActionExecutionStatus ::
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
