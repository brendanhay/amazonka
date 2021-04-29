{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype JobStatus = JobStatus'
  { fromJobStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
