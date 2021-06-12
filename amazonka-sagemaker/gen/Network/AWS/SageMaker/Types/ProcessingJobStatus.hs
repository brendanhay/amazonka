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
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStatus
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

import qualified Network.AWS.Core as Core

newtype ProcessingJobStatus = ProcessingJobStatus'
  { fromProcessingJobStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
