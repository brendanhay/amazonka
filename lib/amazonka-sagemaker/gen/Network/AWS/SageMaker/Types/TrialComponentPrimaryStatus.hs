{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TrialComponentPrimaryStatus
  ( TrialComponentPrimaryStatus
    ( TrialComponentPrimaryStatus'
    , TrialComponentPrimaryStatusInProgress
    , TrialComponentPrimaryStatusCompleted
    , TrialComponentPrimaryStatusFailed
    , TrialComponentPrimaryStatusStopping
    , TrialComponentPrimaryStatusStopped
    , fromTrialComponentPrimaryStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TrialComponentPrimaryStatus = TrialComponentPrimaryStatus'{fromTrialComponentPrimaryStatus
                                                                   :: Core.Text}
                                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                        Core.Generic)
                                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                          Core.ToJSONKey, Core.FromJSONKey,
                                                          Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                          Core.FromXML, Core.ToText, Core.FromText,
                                                          Core.ToByteString, Core.ToQuery,
                                                          Core.ToHeader)

pattern TrialComponentPrimaryStatusInProgress :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatusInProgress = TrialComponentPrimaryStatus' "InProgress"

pattern TrialComponentPrimaryStatusCompleted :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatusCompleted = TrialComponentPrimaryStatus' "Completed"

pattern TrialComponentPrimaryStatusFailed :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatusFailed = TrialComponentPrimaryStatus' "Failed"

pattern TrialComponentPrimaryStatusStopping :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatusStopping = TrialComponentPrimaryStatus' "Stopping"

pattern TrialComponentPrimaryStatusStopped :: TrialComponentPrimaryStatus
pattern TrialComponentPrimaryStatusStopped = TrialComponentPrimaryStatus' "Stopped"

{-# COMPLETE 
  TrialComponentPrimaryStatusInProgress,

  TrialComponentPrimaryStatusCompleted,

  TrialComponentPrimaryStatusFailed,

  TrialComponentPrimaryStatusStopping,

  TrialComponentPrimaryStatusStopped,
  TrialComponentPrimaryStatus'
  #-}
