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
        HCRApNortheast1,
        HCRApSoutheast1,
        HCRApSoutheast2,
        HCREuWest1,
        HCRSaEast1,
        HCRUsEast1,
        HCRUsWest1,
        HCRUsWest2
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

pattern HCRApNortheast1 :: HealthCheckRegion
pattern HCRApNortheast1 = HealthCheckRegion' "ap-northeast-1"

pattern HCRApSoutheast1 :: HealthCheckRegion
pattern HCRApSoutheast1 = HealthCheckRegion' "ap-southeast-1"

pattern HCRApSoutheast2 :: HealthCheckRegion
pattern HCRApSoutheast2 = HealthCheckRegion' "ap-southeast-2"

pattern HCREuWest1 :: HealthCheckRegion
pattern HCREuWest1 = HealthCheckRegion' "eu-west-1"

pattern HCRSaEast1 :: HealthCheckRegion
pattern HCRSaEast1 = HealthCheckRegion' "sa-east-1"

pattern HCRUsEast1 :: HealthCheckRegion
pattern HCRUsEast1 = HealthCheckRegion' "us-east-1"

pattern HCRUsWest1 :: HealthCheckRegion
pattern HCRUsWest1 = HealthCheckRegion' "us-west-1"

pattern HCRUsWest2 :: HealthCheckRegion
pattern HCRUsWest2 = HealthCheckRegion' "us-west-2"

{-# COMPLETE
  HCRApNortheast1,
  HCRApSoutheast1,
  HCRApSoutheast2,
  HCREuWest1,
  HCRSaEast1,
  HCRUsEast1,
  HCRUsWest1,
  HCRUsWest2,
  HealthCheckRegion'
  #-}
