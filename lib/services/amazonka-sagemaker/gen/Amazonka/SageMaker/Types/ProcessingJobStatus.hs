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
-- Module      : Amazonka.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProcessingJobStatus
  ( ProcessingJobStatus
      ( ..,
        ProcessingJobStatus_Completed,
        ProcessingJobStatus_Failed,
        ProcessingJobStatus_InProgress,
        ProcessingJobStatus_Stopped,
        ProcessingJobStatus_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProcessingJobStatus = ProcessingJobStatus'
  { fromProcessingJobStatus ::
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

pattern ProcessingJobStatus_Completed :: ProcessingJobStatus
pattern ProcessingJobStatus_Completed = ProcessingJobStatus' "Completed"

pattern ProcessingJobStatus_Failed :: ProcessingJobStatus
pattern ProcessingJobStatus_Failed = ProcessingJobStatus' "Failed"

pattern ProcessingJobStatus_InProgress :: ProcessingJobStatus
pattern ProcessingJobStatus_InProgress = ProcessingJobStatus' "InProgress"

pattern ProcessingJobStatus_Stopped :: ProcessingJobStatus
pattern ProcessingJobStatus_Stopped = ProcessingJobStatus' "Stopped"

pattern ProcessingJobStatus_Stopping :: ProcessingJobStatus
pattern ProcessingJobStatus_Stopping = ProcessingJobStatus' "Stopping"

{-# COMPLETE
  ProcessingJobStatus_Completed,
  ProcessingJobStatus_Failed,
  ProcessingJobStatus_InProgress,
  ProcessingJobStatus_Stopped,
  ProcessingJobStatus_Stopping,
  ProcessingJobStatus'
  #-}
