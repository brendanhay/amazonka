{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CandidateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CandidateStatus
  ( CandidateStatus
    ( CandidateStatus'
    , CandidateStatusCompleted
    , CandidateStatusInProgress
    , CandidateStatusFailed
    , CandidateStatusStopped
    , CandidateStatusStopping
    , fromCandidateStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CandidateStatus = CandidateStatus'{fromCandidateStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern CandidateStatusCompleted :: CandidateStatus
pattern CandidateStatusCompleted = CandidateStatus' "Completed"

pattern CandidateStatusInProgress :: CandidateStatus
pattern CandidateStatusInProgress = CandidateStatus' "InProgress"

pattern CandidateStatusFailed :: CandidateStatus
pattern CandidateStatusFailed = CandidateStatus' "Failed"

pattern CandidateStatusStopped :: CandidateStatus
pattern CandidateStatusStopped = CandidateStatus' "Stopped"

pattern CandidateStatusStopping :: CandidateStatus
pattern CandidateStatusStopping = CandidateStatus' "Stopping"

{-# COMPLETE 
  CandidateStatusCompleted,

  CandidateStatusInProgress,

  CandidateStatusFailed,

  CandidateStatusStopped,

  CandidateStatusStopping,
  CandidateStatus'
  #-}
