{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.EnvironmentHealthStatus
  ( EnvironmentHealthStatus
      ( EnvironmentHealthStatus',
        EnvironmentHealthStatusNoData,
        EnvironmentHealthStatusUnknown,
        EnvironmentHealthStatusPending,
        EnvironmentHealthStatusOK,
        EnvironmentHealthStatusInfo,
        EnvironmentHealthStatusWarning,
        EnvironmentHealthStatusDegraded,
        EnvironmentHealthStatusSevere,
        EnvironmentHealthStatusSuspended,
        fromEnvironmentHealthStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EnvironmentHealthStatus = EnvironmentHealthStatus'
  { fromEnvironmentHealthStatus ::
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

pattern EnvironmentHealthStatusNoData :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusNoData = EnvironmentHealthStatus' "NoData"

pattern EnvironmentHealthStatusUnknown :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusUnknown = EnvironmentHealthStatus' "Unknown"

pattern EnvironmentHealthStatusPending :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusPending = EnvironmentHealthStatus' "Pending"

pattern EnvironmentHealthStatusOK :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusOK = EnvironmentHealthStatus' "Ok"

pattern EnvironmentHealthStatusInfo :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusInfo = EnvironmentHealthStatus' "Info"

pattern EnvironmentHealthStatusWarning :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusWarning = EnvironmentHealthStatus' "Warning"

pattern EnvironmentHealthStatusDegraded :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusDegraded = EnvironmentHealthStatus' "Degraded"

pattern EnvironmentHealthStatusSevere :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusSevere = EnvironmentHealthStatus' "Severe"

pattern EnvironmentHealthStatusSuspended :: EnvironmentHealthStatus
pattern EnvironmentHealthStatusSuspended = EnvironmentHealthStatus' "Suspended"

{-# COMPLETE
  EnvironmentHealthStatusNoData,
  EnvironmentHealthStatusUnknown,
  EnvironmentHealthStatusPending,
  EnvironmentHealthStatusOK,
  EnvironmentHealthStatusInfo,
  EnvironmentHealthStatusWarning,
  EnvironmentHealthStatusDegraded,
  EnvironmentHealthStatusSevere,
  EnvironmentHealthStatusSuspended,
  EnvironmentHealthStatus'
  #-}
