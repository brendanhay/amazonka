{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ExecutionStatus
  ( ExecutionStatus
      ( ExecutionStatus',
        ExecutionStatusUnavailable,
        ExecutionStatusAvailable,
        ExecutionStatusExecuteInProgress,
        ExecutionStatusExecuteComplete,
        ExecutionStatusExecuteFailed,
        ExecutionStatusObsolete,
        fromExecutionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ExecutionStatus = ExecutionStatus'
  { fromExecutionStatus ::
      Core.Text
  }
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

pattern ExecutionStatusUnavailable :: ExecutionStatus
pattern ExecutionStatusUnavailable = ExecutionStatus' "UNAVAILABLE"

pattern ExecutionStatusAvailable :: ExecutionStatus
pattern ExecutionStatusAvailable = ExecutionStatus' "AVAILABLE"

pattern ExecutionStatusExecuteInProgress :: ExecutionStatus
pattern ExecutionStatusExecuteInProgress = ExecutionStatus' "EXECUTE_IN_PROGRESS"

pattern ExecutionStatusExecuteComplete :: ExecutionStatus
pattern ExecutionStatusExecuteComplete = ExecutionStatus' "EXECUTE_COMPLETE"

pattern ExecutionStatusExecuteFailed :: ExecutionStatus
pattern ExecutionStatusExecuteFailed = ExecutionStatus' "EXECUTE_FAILED"

pattern ExecutionStatusObsolete :: ExecutionStatus
pattern ExecutionStatusObsolete = ExecutionStatus' "OBSOLETE"

{-# COMPLETE
  ExecutionStatusUnavailable,
  ExecutionStatusAvailable,
  ExecutionStatusExecuteInProgress,
  ExecutionStatusExecuteComplete,
  ExecutionStatusExecuteFailed,
  ExecutionStatusObsolete,
  ExecutionStatus'
  #-}
