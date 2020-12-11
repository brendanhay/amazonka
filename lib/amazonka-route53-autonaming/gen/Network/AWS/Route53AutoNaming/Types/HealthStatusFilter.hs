-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.HealthStatusFilter
  ( HealthStatusFilter
      ( HealthStatusFilter',
        HSFAll,
        HSFHealthy,
        HSFUnhealthy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HealthStatusFilter = HealthStatusFilter' Lude.Text
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

pattern HSFAll :: HealthStatusFilter
pattern HSFAll = HealthStatusFilter' "ALL"

pattern HSFHealthy :: HealthStatusFilter
pattern HSFHealthy = HealthStatusFilter' "HEALTHY"

pattern HSFUnhealthy :: HealthStatusFilter
pattern HSFUnhealthy = HealthStatusFilter' "UNHEALTHY"

{-# COMPLETE
  HSFAll,
  HSFHealthy,
  HSFUnhealthy,
  HealthStatusFilter'
  #-}
