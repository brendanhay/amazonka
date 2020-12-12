{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
  ( MatchmakingConfigurationStatus
      ( MatchmakingConfigurationStatus',
        MCSCancelled,
        MCSCompleted,
        MCSFailed,
        MCSPlacing,
        MCSQueued,
        MCSRequiresAcceptance,
        MCSSearching,
        MCSTimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MatchmakingConfigurationStatus = MatchmakingConfigurationStatus' Lude.Text
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

pattern MCSCancelled :: MatchmakingConfigurationStatus
pattern MCSCancelled = MatchmakingConfigurationStatus' "CANCELLED"

pattern MCSCompleted :: MatchmakingConfigurationStatus
pattern MCSCompleted = MatchmakingConfigurationStatus' "COMPLETED"

pattern MCSFailed :: MatchmakingConfigurationStatus
pattern MCSFailed = MatchmakingConfigurationStatus' "FAILED"

pattern MCSPlacing :: MatchmakingConfigurationStatus
pattern MCSPlacing = MatchmakingConfigurationStatus' "PLACING"

pattern MCSQueued :: MatchmakingConfigurationStatus
pattern MCSQueued = MatchmakingConfigurationStatus' "QUEUED"

pattern MCSRequiresAcceptance :: MatchmakingConfigurationStatus
pattern MCSRequiresAcceptance = MatchmakingConfigurationStatus' "REQUIRES_ACCEPTANCE"

pattern MCSSearching :: MatchmakingConfigurationStatus
pattern MCSSearching = MatchmakingConfigurationStatus' "SEARCHING"

pattern MCSTimedOut :: MatchmakingConfigurationStatus
pattern MCSTimedOut = MatchmakingConfigurationStatus' "TIMED_OUT"

{-# COMPLETE
  MCSCancelled,
  MCSCompleted,
  MCSFailed,
  MCSPlacing,
  MCSQueued,
  MCSRequiresAcceptance,
  MCSSearching,
  MCSTimedOut,
  MatchmakingConfigurationStatus'
  #-}
