{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.MatchmakingConfigurationStatus
  ( MatchmakingConfigurationStatus
      ( ..,
        MatchmakingConfigurationStatus_CANCELLED,
        MatchmakingConfigurationStatus_COMPLETED,
        MatchmakingConfigurationStatus_FAILED,
        MatchmakingConfigurationStatus_PLACING,
        MatchmakingConfigurationStatus_QUEUED,
        MatchmakingConfigurationStatus_REQUIRES_ACCEPTANCE,
        MatchmakingConfigurationStatus_SEARCHING,
        MatchmakingConfigurationStatus_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype MatchmakingConfigurationStatus = MatchmakingConfigurationStatus'
  { fromMatchmakingConfigurationStatus ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern MatchmakingConfigurationStatus_CANCELLED :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_CANCELLED = MatchmakingConfigurationStatus' "CANCELLED"

pattern MatchmakingConfigurationStatus_COMPLETED :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_COMPLETED = MatchmakingConfigurationStatus' "COMPLETED"

pattern MatchmakingConfigurationStatus_FAILED :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_FAILED = MatchmakingConfigurationStatus' "FAILED"

pattern MatchmakingConfigurationStatus_PLACING :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_PLACING = MatchmakingConfigurationStatus' "PLACING"

pattern MatchmakingConfigurationStatus_QUEUED :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_QUEUED = MatchmakingConfigurationStatus' "QUEUED"

pattern MatchmakingConfigurationStatus_REQUIRES_ACCEPTANCE :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_REQUIRES_ACCEPTANCE = MatchmakingConfigurationStatus' "REQUIRES_ACCEPTANCE"

pattern MatchmakingConfigurationStatus_SEARCHING :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_SEARCHING = MatchmakingConfigurationStatus' "SEARCHING"

pattern MatchmakingConfigurationStatus_TIMED_OUT :: MatchmakingConfigurationStatus
pattern MatchmakingConfigurationStatus_TIMED_OUT = MatchmakingConfigurationStatus' "TIMED_OUT"

{-# COMPLETE
  MatchmakingConfigurationStatus_CANCELLED,
  MatchmakingConfigurationStatus_COMPLETED,
  MatchmakingConfigurationStatus_FAILED,
  MatchmakingConfigurationStatus_PLACING,
  MatchmakingConfigurationStatus_QUEUED,
  MatchmakingConfigurationStatus_REQUIRES_ACCEPTANCE,
  MatchmakingConfigurationStatus_SEARCHING,
  MatchmakingConfigurationStatus_TIMED_OUT,
  MatchmakingConfigurationStatus'
  #-}
