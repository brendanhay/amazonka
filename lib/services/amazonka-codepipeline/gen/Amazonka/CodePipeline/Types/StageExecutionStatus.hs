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
-- Module      : Amazonka.CodePipeline.Types.StageExecutionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.StageExecutionStatus
  ( StageExecutionStatus
      ( ..,
        StageExecutionStatus_Cancelled,
        StageExecutionStatus_Failed,
        StageExecutionStatus_InProgress,
        StageExecutionStatus_Stopped,
        StageExecutionStatus_Stopping,
        StageExecutionStatus_Succeeded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StageExecutionStatus = StageExecutionStatus'
  { fromStageExecutionStatus ::
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

pattern StageExecutionStatus_Cancelled :: StageExecutionStatus
pattern StageExecutionStatus_Cancelled = StageExecutionStatus' "Cancelled"

pattern StageExecutionStatus_Failed :: StageExecutionStatus
pattern StageExecutionStatus_Failed = StageExecutionStatus' "Failed"

pattern StageExecutionStatus_InProgress :: StageExecutionStatus
pattern StageExecutionStatus_InProgress = StageExecutionStatus' "InProgress"

pattern StageExecutionStatus_Stopped :: StageExecutionStatus
pattern StageExecutionStatus_Stopped = StageExecutionStatus' "Stopped"

pattern StageExecutionStatus_Stopping :: StageExecutionStatus
pattern StageExecutionStatus_Stopping = StageExecutionStatus' "Stopping"

pattern StageExecutionStatus_Succeeded :: StageExecutionStatus
pattern StageExecutionStatus_Succeeded = StageExecutionStatus' "Succeeded"

{-# COMPLETE
  StageExecutionStatus_Cancelled,
  StageExecutionStatus_Failed,
  StageExecutionStatus_InProgress,
  StageExecutionStatus_Stopped,
  StageExecutionStatus_Stopping,
  StageExecutionStatus_Succeeded,
  StageExecutionStatus'
  #-}
