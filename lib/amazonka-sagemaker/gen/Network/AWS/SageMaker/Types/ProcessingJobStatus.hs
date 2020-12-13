{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStatus
  ( ProcessingJobStatus
      ( ProcessingJobStatus',
        PJSInProgress,
        PJSCompleted,
        PJSFailed,
        PJSStopping,
        PJSStopped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ProcessingJobStatus = ProcessingJobStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PJSInProgress :: ProcessingJobStatus
pattern PJSInProgress = ProcessingJobStatus' "InProgress"

pattern PJSCompleted :: ProcessingJobStatus
pattern PJSCompleted = ProcessingJobStatus' "Completed"

pattern PJSFailed :: ProcessingJobStatus
pattern PJSFailed = ProcessingJobStatus' "Failed"

pattern PJSStopping :: ProcessingJobStatus
pattern PJSStopping = ProcessingJobStatus' "Stopping"

pattern PJSStopped :: ProcessingJobStatus
pattern PJSStopped = ProcessingJobStatus' "Stopped"

{-# COMPLETE
  PJSInProgress,
  PJSCompleted,
  PJSFailed,
  PJSStopping,
  PJSStopped,
  ProcessingJobStatus'
  #-}
