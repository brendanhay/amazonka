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
        HealthCheckRegionUsEast1,
        HealthCheckRegionUsWest1,
        HealthCheckRegionUsWest2,
        HealthCheckRegionEuWest1,
        HealthCheckRegionApSoutheast1,
        HealthCheckRegionApSoutheast2,
        HealthCheckRegionApNortheast1,
        HealthCheckRegionSaEast1,
        fromHealthCheckRegion
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types

newtype HealthCheckRegion = HealthCheckRegion'
  { fromHealthCheckRegion ::
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

pattern HealthCheckRegionUsEast1 :: HealthCheckRegion
pattern HealthCheckRegionUsEast1 = HealthCheckRegion' "us-east-1"

pattern HealthCheckRegionUsWest1 :: HealthCheckRegion
pattern HealthCheckRegionUsWest1 = HealthCheckRegion' "us-west-1"

pattern HealthCheckRegionUsWest2 :: HealthCheckRegion
pattern HealthCheckRegionUsWest2 = HealthCheckRegion' "us-west-2"

pattern HealthCheckRegionEuWest1 :: HealthCheckRegion
pattern HealthCheckRegionEuWest1 = HealthCheckRegion' "eu-west-1"

pattern HealthCheckRegionApSoutheast1 :: HealthCheckRegion
pattern HealthCheckRegionApSoutheast1 = HealthCheckRegion' "ap-southeast-1"

pattern HealthCheckRegionApSoutheast2 :: HealthCheckRegion
pattern HealthCheckRegionApSoutheast2 = HealthCheckRegion' "ap-southeast-2"

pattern HealthCheckRegionApNortheast1 :: HealthCheckRegion
pattern HealthCheckRegionApNortheast1 = HealthCheckRegion' "ap-northeast-1"

pattern HealthCheckRegionSaEast1 :: HealthCheckRegion
pattern HealthCheckRegionSaEast1 = HealthCheckRegion' "sa-east-1"

{-# COMPLETE
  HealthCheckRegionUsEast1,
  HealthCheckRegionUsWest1,
  HealthCheckRegionUsWest2,
  HealthCheckRegionEuWest1,
  HealthCheckRegionApSoutheast1,
  HealthCheckRegionApSoutheast2,
  HealthCheckRegionApNortheast1,
  HealthCheckRegionSaEast1,
  HealthCheckRegion'
  #-}
