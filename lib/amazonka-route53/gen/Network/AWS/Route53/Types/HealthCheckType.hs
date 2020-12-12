{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckType
  ( HealthCheckType
      ( HealthCheckType',
        Calculated,
        CloudwatchMetric,
        HTTP,
        HTTPS,
        HTTPSStrMatch,
        HTTPStrMatch,
        TCP
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype HealthCheckType = HealthCheckType' Lude.Text
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

pattern Calculated :: HealthCheckType
pattern Calculated = HealthCheckType' "CALCULATED"

pattern CloudwatchMetric :: HealthCheckType
pattern CloudwatchMetric = HealthCheckType' "CLOUDWATCH_METRIC"

pattern HTTP :: HealthCheckType
pattern HTTP = HealthCheckType' "HTTP"

pattern HTTPS :: HealthCheckType
pattern HTTPS = HealthCheckType' "HTTPS"

pattern HTTPSStrMatch :: HealthCheckType
pattern HTTPSStrMatch = HealthCheckType' "HTTPS_STR_MATCH"

pattern HTTPStrMatch :: HealthCheckType
pattern HTTPStrMatch = HealthCheckType' "HTTP_STR_MATCH"

pattern TCP :: HealthCheckType
pattern TCP = HealthCheckType' "TCP"

{-# COMPLETE
  Calculated,
  CloudwatchMetric,
  HTTP,
  HTTPS,
  HTTPSStrMatch,
  HTTPStrMatch,
  TCP,
  HealthCheckType'
  #-}
