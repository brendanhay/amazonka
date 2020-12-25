{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        JobStatusSubmitted,
        JobStatusProgressing,
        JobStatusComplete,
        JobStatusCanceled,
        JobStatusError,
        fromJobStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | A job's status can be SUBMITTED, PROGRESSING, COMPLETE, CANCELED, or ERROR.
newtype JobStatus = JobStatus' {fromJobStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern JobStatusSubmitted :: JobStatus
pattern JobStatusSubmitted = JobStatus' "SUBMITTED"

pattern JobStatusProgressing :: JobStatus
pattern JobStatusProgressing = JobStatus' "PROGRESSING"

pattern JobStatusComplete :: JobStatus
pattern JobStatusComplete = JobStatus' "COMPLETE"

pattern JobStatusCanceled :: JobStatus
pattern JobStatusCanceled = JobStatus' "CANCELED"

pattern JobStatusError :: JobStatus
pattern JobStatusError = JobStatus' "ERROR"

{-# COMPLETE
  JobStatusSubmitted,
  JobStatusProgressing,
  JobStatusComplete,
  JobStatusCanceled,
  JobStatusError,
  JobStatus'
  #-}
