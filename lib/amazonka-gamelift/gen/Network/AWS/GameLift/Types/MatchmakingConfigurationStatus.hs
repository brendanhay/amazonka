{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
  ( MatchmakingConfigurationStatus
    ( MatchmakingConfigurationStatus'
    , MatchmakingConfigurationStatusCancelled
    , MatchmakingConfigurationStatusCompleted
    , MatchmakingConfigurationStatusFailed
    , MatchmakingConfigurationStatusPlacing
    , MatchmakingConfigurationStatusQueued
    , MatchmakingConfigurationStatusRequiresAcceptance
    , MatchmakingConfigurationStatusSearching
    , MatchmakingConfigurationStatusTimedOut
    , fromMatchmakingConfigurationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MatchmakingConfigurationStatus = MatchmakingConfigurationStatus'{fromMatchmakingConfigurationStatus
                                                                         :: Core.Text}
                                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                           Core.Generic)
                                           deriving newtype (Core.IsString, Core.Hashable,
                                                             Core.NFData, Core.ToJSONKey,
                                                             Core.FromJSONKey, Core.ToJSON,
                                                             Core.FromJSON, Core.ToXML,
                                                             Core.FromXML, Core.ToText,
                                                             Core.FromText, Core.ToByteString,
                                                             Core.ToQuery, Core.ToHeader)

pattern MatchmakingConfigurationStatusCancelled :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusCancelled = MatchmakingConfigurationStatus' "CANCELLED"

pattern MatchmakingConfigurationStatusCompleted :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusCompleted = MatchmakingConfigurationStatus' "COMPLETED"

pattern MatchmakingConfigurationStatusFailed :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusFailed = MatchmakingConfigurationStatus' "FAILED"

pattern MatchmakingConfigurationStatusPlacing :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusPlacing = MatchmakingConfigurationStatus' "PLACING"

pattern MatchmakingConfigurationStatusQueued :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusQueued = MatchmakingConfigurationStatus' "QUEUED"

pattern MatchmakingConfigurationStatusRequiresAcceptance :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusRequiresAcceptance = MatchmakingConfigurationStatus' "REQUIRES_ACCEPTANCE"

pattern MatchmakingConfigurationStatusSearching :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusSearching = MatchmakingConfigurationStatus' "SEARCHING"

pattern MatchmakingConfigurationStatusTimedOut :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatusTimedOut = MatchmakingConfigurationStatus' "TIMED_OUT"

{-# COMPLETE 
  MatchmakingConfigurationStatusCancelled,

  MatchmakingConfigurationStatusCompleted,

  MatchmakingConfigurationStatusFailed,

  MatchmakingConfigurationStatusPlacing,

  MatchmakingConfigurationStatusQueued,

  MatchmakingConfigurationStatusRequiresAcceptance,

  MatchmakingConfigurationStatusSearching,

  MatchmakingConfigurationStatusTimedOut,
  MatchmakingConfigurationStatus'
  #-}
