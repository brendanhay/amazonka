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
-- Module      : Network.AWS.CodePipeline.Types.JobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobStatus
  ( JobStatus
      ( ..,
        JobStatus_Created,
        JobStatus_Dispatched,
        JobStatus_Failed,
        JobStatus_InProgress,
        JobStatus_Queued,
        JobStatus_Succeeded,
        JobStatus_TimedOut
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype JobStatus = JobStatus'
  { fromJobStatus ::
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

pattern JobStatus_Created :: JobStatus
pattern JobStatus_Created = JobStatus' "Created"

pattern JobStatus_Dispatched :: JobStatus
pattern JobStatus_Dispatched = JobStatus' "Dispatched"

pattern JobStatus_Failed :: JobStatus
pattern JobStatus_Failed = JobStatus' "Failed"

pattern JobStatus_InProgress :: JobStatus
pattern JobStatus_InProgress = JobStatus' "InProgress"

pattern JobStatus_Queued :: JobStatus
pattern JobStatus_Queued = JobStatus' "Queued"

pattern JobStatus_Succeeded :: JobStatus
pattern JobStatus_Succeeded = JobStatus' "Succeeded"

pattern JobStatus_TimedOut :: JobStatus
pattern JobStatus_TimedOut = JobStatus' "TimedOut"

{-# COMPLETE
  JobStatus_Created,
  JobStatus_Dispatched,
  JobStatus_Failed,
  JobStatus_InProgress,
  JobStatus_Queued,
  JobStatus_Succeeded,
  JobStatus_TimedOut,
  JobStatus'
  #-}
