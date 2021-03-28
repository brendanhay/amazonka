{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.RemediationExecutionState
  ( RemediationExecutionState
    ( RemediationExecutionState'
    , RemediationExecutionStateQueued
    , RemediationExecutionStateInProgress
    , RemediationExecutionStateSucceeded
    , RemediationExecutionStateFailed
    , fromRemediationExecutionState
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype RemediationExecutionState = RemediationExecutionState'{fromRemediationExecutionState
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern RemediationExecutionStateQueued :: RemediationExecutionState
pattern RemediationExecutionStateQueued = RemediationExecutionState' "QUEUED"

pattern RemediationExecutionStateInProgress :: RemediationExecutionState
pattern RemediationExecutionStateInProgress = RemediationExecutionState' "IN_PROGRESS"

pattern RemediationExecutionStateSucceeded :: RemediationExecutionState
pattern RemediationExecutionStateSucceeded = RemediationExecutionState' "SUCCEEDED"

pattern RemediationExecutionStateFailed :: RemediationExecutionState
pattern RemediationExecutionStateFailed = RemediationExecutionState' "FAILED"

{-# COMPLETE 
  RemediationExecutionStateQueued,

  RemediationExecutionStateInProgress,

  RemediationExecutionStateSucceeded,

  RemediationExecutionStateFailed,
  RemediationExecutionState'
  #-}
