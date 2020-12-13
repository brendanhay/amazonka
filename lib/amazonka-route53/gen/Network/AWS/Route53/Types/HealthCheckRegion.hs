{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HealthCheckRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HealthCheckRegion
  ( HealthCheckRegion
      ( HealthCheckRegion',
        UsEast1,
        UsWest1,
        UsWest2,
        EuWest1,
        ApSoutheast1,
        ApSoutheast2,
        ApNortheast1,
        SaEast1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype HealthCheckRegion = HealthCheckRegion' Lude.Text
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

pattern UsEast1 :: HealthCheckRegion
pattern UsEast1 = HealthCheckRegion' "us-east-1"

pattern UsWest1 :: HealthCheckRegion
pattern UsWest1 = HealthCheckRegion' "us-west-1"

pattern UsWest2 :: HealthCheckRegion
pattern UsWest2 = HealthCheckRegion' "us-west-2"

pattern EuWest1 :: HealthCheckRegion
pattern EuWest1 = HealthCheckRegion' "eu-west-1"

pattern ApSoutheast1 :: HealthCheckRegion
pattern ApSoutheast1 = HealthCheckRegion' "ap-southeast-1"

pattern ApSoutheast2 :: HealthCheckRegion
pattern ApSoutheast2 = HealthCheckRegion' "ap-southeast-2"

pattern ApNortheast1 :: HealthCheckRegion
pattern ApNortheast1 = HealthCheckRegion' "ap-northeast-1"

pattern SaEast1 :: HealthCheckRegion
pattern SaEast1 = HealthCheckRegion' "sa-east-1"

{-# COMPLETE
  UsEast1,
  UsWest1,
  UsWest2,
  EuWest1,
  ApSoutheast1,
  ApSoutheast2,
  ApNortheast1,
  SaEast1,
  HealthCheckRegion'
  #-}
