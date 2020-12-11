-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.HealthStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.HealthStatus
  ( HealthStatus
      ( HealthStatus',
        HSHealthy,
        HSUnhealthy,
        HSUnknown
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HealthStatus = HealthStatus' Lude.Text
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

pattern HSHealthy :: HealthStatus
pattern HSHealthy = HealthStatus' "HEALTHY"

pattern HSUnhealthy :: HealthStatus
pattern HSUnhealthy = HealthStatus' "UNHEALTHY"

pattern HSUnknown :: HealthStatus
pattern HSUnknown = HealthStatus' "UNKNOWN"

{-# COMPLETE
  HSHealthy,
  HSUnhealthy,
  HSUnknown,
  HealthStatus'
  #-}
