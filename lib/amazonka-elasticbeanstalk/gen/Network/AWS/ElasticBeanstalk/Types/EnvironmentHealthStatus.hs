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
        EHSDegraded,
        EHSInfo,
        EHSNoData,
        EHSOK,
        EHSPending,
        EHSSevere,
        EHSSuspended,
        EHSUnknown,
        EHSWarning
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EnvironmentHealthStatus = EnvironmentHealthStatus' Lude.Text
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

pattern EHSDegraded :: EnvironmentHealthStatus
pattern EHSDegraded = EnvironmentHealthStatus' "Degraded"

pattern EHSInfo :: EnvironmentHealthStatus
pattern EHSInfo = EnvironmentHealthStatus' "Info"

pattern EHSNoData :: EnvironmentHealthStatus
pattern EHSNoData = EnvironmentHealthStatus' "NoData"

pattern EHSOK :: EnvironmentHealthStatus
pattern EHSOK = EnvironmentHealthStatus' "Ok"

pattern EHSPending :: EnvironmentHealthStatus
pattern EHSPending = EnvironmentHealthStatus' "Pending"

pattern EHSSevere :: EnvironmentHealthStatus
pattern EHSSevere = EnvironmentHealthStatus' "Severe"

pattern EHSSuspended :: EnvironmentHealthStatus
pattern EHSSuspended = EnvironmentHealthStatus' "Suspended"

pattern EHSUnknown :: EnvironmentHealthStatus
pattern EHSUnknown = EnvironmentHealthStatus' "Unknown"

pattern EHSWarning :: EnvironmentHealthStatus
pattern EHSWarning = EnvironmentHealthStatus' "Warning"

{-# COMPLETE
  EHSDegraded,
  EHSInfo,
  EHSNoData,
  EHSOK,
  EHSPending,
  EHSSevere,
  EHSSuspended,
  EHSUnknown,
  EHSWarning,
  EnvironmentHealthStatus'
  #-}
