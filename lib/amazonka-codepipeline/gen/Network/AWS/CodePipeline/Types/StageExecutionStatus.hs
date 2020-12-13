{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageExecutionStatus
  ( StageExecutionStatus
      ( StageExecutionStatus',
        SESInProgress,
        SESFailed,
        SESStopped,
        SESStopping,
        SESSucceeded
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StageExecutionStatus = StageExecutionStatus' Lude.Text
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

pattern SESInProgress :: StageExecutionStatus
pattern SESInProgress = StageExecutionStatus' "InProgress"

pattern SESFailed :: StageExecutionStatus
pattern SESFailed = StageExecutionStatus' "Failed"

pattern SESStopped :: StageExecutionStatus
pattern SESStopped = StageExecutionStatus' "Stopped"

pattern SESStopping :: StageExecutionStatus
pattern SESStopping = StageExecutionStatus' "Stopping"

pattern SESSucceeded :: StageExecutionStatus
pattern SESSucceeded = StageExecutionStatus' "Succeeded"

{-# COMPLETE
  SESInProgress,
  SESFailed,
  SESStopped,
  SESStopping,
  SESSucceeded,
  StageExecutionStatus'
  #-}
