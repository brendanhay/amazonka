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
-- Module      : Network.AWS.Glue.Types.JobRunState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobRunState
  ( JobRunState
      ( ..,
        JobRunState_FAILED,
        JobRunState_RUNNING,
        JobRunState_STARTING,
        JobRunState_STOPPED,
        JobRunState_STOPPING,
        JobRunState_SUCCEEDED,
        JobRunState_TIMEOUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype JobRunState = JobRunState'
  { fromJobRunState ::
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

pattern JobRunState_FAILED :: JobRunState
pattern JobRunState_FAILED = JobRunState' "FAILED"

pattern JobRunState_RUNNING :: JobRunState
pattern JobRunState_RUNNING = JobRunState' "RUNNING"

pattern JobRunState_STARTING :: JobRunState
pattern JobRunState_STARTING = JobRunState' "STARTING"

pattern JobRunState_STOPPED :: JobRunState
pattern JobRunState_STOPPED = JobRunState' "STOPPED"

pattern JobRunState_STOPPING :: JobRunState
pattern JobRunState_STOPPING = JobRunState' "STOPPING"

pattern JobRunState_SUCCEEDED :: JobRunState
pattern JobRunState_SUCCEEDED = JobRunState' "SUCCEEDED"

pattern JobRunState_TIMEOUT :: JobRunState
pattern JobRunState_TIMEOUT = JobRunState' "TIMEOUT"

{-# COMPLETE
  JobRunState_FAILED,
  JobRunState_RUNNING,
  JobRunState_STARTING,
  JobRunState_STOPPED,
  JobRunState_STOPPING,
  JobRunState_SUCCEEDED,
  JobRunState_TIMEOUT,
  JobRunState'
  #-}
