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
        EHSNoData,
        EHSUnknown,
        EHSPending,
        EHSOK,
        EHSInfo,
        EHSWarning,
        EHSDegraded,
        EHSSevere,
        EHSSuspended
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

pattern EHSNoData :: EnvironmentHealthStatus
pattern EHSNoData = EnvironmentHealthStatus' "NoData"

pattern EHSUnknown :: EnvironmentHealthStatus
pattern EHSUnknown = EnvironmentHealthStatus' "Unknown"

pattern EHSPending :: EnvironmentHealthStatus
pattern EHSPending = EnvironmentHealthStatus' "Pending"

pattern EHSOK :: EnvironmentHealthStatus
pattern EHSOK = EnvironmentHealthStatus' "Ok"

pattern EHSInfo :: EnvironmentHealthStatus
pattern EHSInfo = EnvironmentHealthStatus' "Info"

pattern EHSWarning :: EnvironmentHealthStatus
pattern EHSWarning = EnvironmentHealthStatus' "Warning"

pattern EHSDegraded :: EnvironmentHealthStatus
pattern EHSDegraded = EnvironmentHealthStatus' "Degraded"

pattern EHSSevere :: EnvironmentHealthStatus
pattern EHSSevere = EnvironmentHealthStatus' "Severe"

pattern EHSSuspended :: EnvironmentHealthStatus
pattern EHSSuspended = EnvironmentHealthStatus' "Suspended"

{-# COMPLETE
  EHSNoData,
  EHSUnknown,
  EHSPending,
  EHSOK,
  EHSInfo,
  EHSWarning,
  EHSDegraded,
  EHSSevere,
  EHSSuspended,
  EnvironmentHealthStatus'
  #-}
