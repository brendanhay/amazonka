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
-- Module      : Network.AWS.Comprehend.Types.JobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.JobStatus
  ( JobStatus
      ( ..,
        JobStatus_COMPLETED,
        JobStatus_FAILED,
        JobStatus_IN_PROGRESS,
        JobStatus_STOPPED,
        JobStatus_STOP_REQUESTED,
        JobStatus_SUBMITTED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype JobStatus = JobStatus'
  { fromJobStatus ::
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

pattern JobStatus_COMPLETED :: JobStatus
pattern JobStatus_COMPLETED = JobStatus' "COMPLETED"

pattern JobStatus_FAILED :: JobStatus
pattern JobStatus_FAILED = JobStatus' "FAILED"

pattern JobStatus_IN_PROGRESS :: JobStatus
pattern JobStatus_IN_PROGRESS = JobStatus' "IN_PROGRESS"

pattern JobStatus_STOPPED :: JobStatus
pattern JobStatus_STOPPED = JobStatus' "STOPPED"

pattern JobStatus_STOP_REQUESTED :: JobStatus
pattern JobStatus_STOP_REQUESTED = JobStatus' "STOP_REQUESTED"

pattern JobStatus_SUBMITTED :: JobStatus
pattern JobStatus_SUBMITTED = JobStatus' "SUBMITTED"

{-# COMPLETE
  JobStatus_COMPLETED,
  JobStatus_FAILED,
  JobStatus_IN_PROGRESS,
  JobStatus_STOPPED,
  JobStatus_STOP_REQUESTED,
  JobStatus_SUBMITTED,
  JobStatus'
  #-}
