{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.StageExecutionStatus
  ( StageExecutionStatus
    ( StageExecutionStatus'
    , StageExecutionStatusInProgress
    , StageExecutionStatusFailed
    , StageExecutionStatusStopped
    , StageExecutionStatusStopping
    , StageExecutionStatusSucceeded
    , fromStageExecutionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StageExecutionStatus = StageExecutionStatus'{fromStageExecutionStatus
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern StageExecutionStatusInProgress :: StageExecutionStatus
pattern StageExecutionStatusInProgress = StageExecutionStatus' "InProgress"

pattern StageExecutionStatusFailed :: StageExecutionStatus
pattern StageExecutionStatusFailed = StageExecutionStatus' "Failed"

pattern StageExecutionStatusStopped :: StageExecutionStatus
pattern StageExecutionStatusStopped = StageExecutionStatus' "Stopped"

pattern StageExecutionStatusStopping :: StageExecutionStatus
pattern StageExecutionStatusStopping = StageExecutionStatus' "Stopping"

pattern StageExecutionStatusSucceeded :: StageExecutionStatus
pattern StageExecutionStatusSucceeded = StageExecutionStatus' "Succeeded"

{-# COMPLETE 
  StageExecutionStatusInProgress,

  StageExecutionStatusFailed,

  StageExecutionStatusStopped,

  StageExecutionStatusStopping,

  StageExecutionStatusSucceeded,
  StageExecutionStatus'
  #-}
