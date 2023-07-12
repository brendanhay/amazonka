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
-- Module      : Amazonka.CodePipeline.Types.PipelineExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.PipelineExecutionStatus
  ( PipelineExecutionStatus
      ( ..,
        PipelineExecutionStatus_Cancelled,
        PipelineExecutionStatus_Failed,
        PipelineExecutionStatus_InProgress,
        PipelineExecutionStatus_Stopped,
        PipelineExecutionStatus_Stopping,
        PipelineExecutionStatus_Succeeded,
        PipelineExecutionStatus_Superseded
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PipelineExecutionStatus = PipelineExecutionStatus'
  { fromPipelineExecutionStatus ::
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

pattern PipelineExecutionStatus_Cancelled :: PipelineExecutionStatus
pattern PipelineExecutionStatus_Cancelled = PipelineExecutionStatus' "Cancelled"

pattern PipelineExecutionStatus_Failed :: PipelineExecutionStatus
pattern PipelineExecutionStatus_Failed = PipelineExecutionStatus' "Failed"

pattern PipelineExecutionStatus_InProgress :: PipelineExecutionStatus
pattern PipelineExecutionStatus_InProgress = PipelineExecutionStatus' "InProgress"

pattern PipelineExecutionStatus_Stopped :: PipelineExecutionStatus
pattern PipelineExecutionStatus_Stopped = PipelineExecutionStatus' "Stopped"

pattern PipelineExecutionStatus_Stopping :: PipelineExecutionStatus
pattern PipelineExecutionStatus_Stopping = PipelineExecutionStatus' "Stopping"

pattern PipelineExecutionStatus_Succeeded :: PipelineExecutionStatus
pattern PipelineExecutionStatus_Succeeded = PipelineExecutionStatus' "Succeeded"

pattern PipelineExecutionStatus_Superseded :: PipelineExecutionStatus
pattern PipelineExecutionStatus_Superseded = PipelineExecutionStatus' "Superseded"

{-# COMPLETE
  PipelineExecutionStatus_Cancelled,
  PipelineExecutionStatus_Failed,
  PipelineExecutionStatus_InProgress,
  PipelineExecutionStatus_Stopped,
  PipelineExecutionStatus_Stopping,
  PipelineExecutionStatus_Succeeded,
  PipelineExecutionStatus_Superseded,
  PipelineExecutionStatus'
  #-}
