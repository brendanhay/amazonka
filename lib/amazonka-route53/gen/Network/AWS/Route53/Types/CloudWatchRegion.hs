{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.CloudWatchRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.CloudWatchRegion
  ( CloudWatchRegion
      ( CloudWatchRegion',
        CloudWatchRegionUsEast1,
        CloudWatchRegionUsEast2,
        CloudWatchRegionUsWest1,
        CloudWatchRegionUsWest2,
        CloudWatchRegionCaCentral1,
        CloudWatchRegionEuCentral1,
        CloudWatchRegionEuWest1,
        CloudWatchRegionEuWest2,
        CloudWatchRegionEuWest3,
        CloudWatchRegionApEast1,
        CloudWatchRegionMeSouth1,
        CloudWatchRegionApSouth1,
        CloudWatchRegionApSoutheast1,
        CloudWatchRegionApSoutheast2,
        CloudWatchRegionApNortheast1,
        CloudWatchRegionApNortheast2,
        CloudWatchRegionApNortheast3,
        CloudWatchRegionEuNorth1,
        CloudWatchRegionSaEast1,
        CloudWatchRegionCnNorthwest1,
        CloudWatchRegionCnNorth1,
        CloudWatchRegionAfSouth1,
        CloudWatchRegionEuSouth1,
        CloudWatchRegionUsGovWest1,
        CloudWatchRegionUsGovEast1,
        CloudWatchRegionUsIsoEast1,
        CloudWatchRegionUsIsobEast1,
        fromCloudWatchRegion
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types

newtype CloudWatchRegion = CloudWatchRegion'
  { fromCloudWatchRegion ::
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

pattern CloudWatchRegionUsEast1 :: CloudWatchRegion
pattern CloudWatchRegionUsEast1 = CloudWatchRegion' "us-east-1"

pattern CloudWatchRegionUsEast2 :: CloudWatchRegion
pattern CloudWatchRegionUsEast2 = CloudWatchRegion' "us-east-2"

pattern CloudWatchRegionUsWest1 :: CloudWatchRegion
pattern CloudWatchRegionUsWest1 = CloudWatchRegion' "us-west-1"

pattern CloudWatchRegionUsWest2 :: CloudWatchRegion
pattern CloudWatchRegionUsWest2 = CloudWatchRegion' "us-west-2"

pattern CloudWatchRegionCaCentral1 :: CloudWatchRegion
pattern CloudWatchRegionCaCentral1 = CloudWatchRegion' "ca-central-1"

pattern CloudWatchRegionEuCentral1 :: CloudWatchRegion
pattern CloudWatchRegionEuCentral1 = CloudWatchRegion' "eu-central-1"

pattern CloudWatchRegionEuWest1 :: CloudWatchRegion
pattern CloudWatchRegionEuWest1 = CloudWatchRegion' "eu-west-1"

pattern CloudWatchRegionEuWest2 :: CloudWatchRegion
pattern CloudWatchRegionEuWest2 = CloudWatchRegion' "eu-west-2"

pattern CloudWatchRegionEuWest3 :: CloudWatchRegion
pattern CloudWatchRegionEuWest3 = CloudWatchRegion' "eu-west-3"

pattern CloudWatchRegionApEast1 :: CloudWatchRegion
pattern CloudWatchRegionApEast1 = CloudWatchRegion' "ap-east-1"

pattern CloudWatchRegionMeSouth1 :: CloudWatchRegion
pattern CloudWatchRegionMeSouth1 = CloudWatchRegion' "me-south-1"

pattern CloudWatchRegionApSouth1 :: CloudWatchRegion
pattern CloudWatchRegionApSouth1 = CloudWatchRegion' "ap-south-1"

pattern CloudWatchRegionApSoutheast1 :: CloudWatchRegion
pattern CloudWatchRegionApSoutheast1 = CloudWatchRegion' "ap-southeast-1"

pattern CloudWatchRegionApSoutheast2 :: CloudWatchRegion
pattern CloudWatchRegionApSoutheast2 = CloudWatchRegion' "ap-southeast-2"

pattern CloudWatchRegionApNortheast1 :: CloudWatchRegion
pattern CloudWatchRegionApNortheast1 = CloudWatchRegion' "ap-northeast-1"

pattern CloudWatchRegionApNortheast2 :: CloudWatchRegion
pattern CloudWatchRegionApNortheast2 = CloudWatchRegion' "ap-northeast-2"

pattern CloudWatchRegionApNortheast3 :: CloudWatchRegion
pattern CloudWatchRegionApNortheast3 = CloudWatchRegion' "ap-northeast-3"

pattern CloudWatchRegionEuNorth1 :: CloudWatchRegion
pattern CloudWatchRegionEuNorth1 = CloudWatchRegion' "eu-north-1"

pattern CloudWatchRegionSaEast1 :: CloudWatchRegion
pattern CloudWatchRegionSaEast1 = CloudWatchRegion' "sa-east-1"

pattern CloudWatchRegionCnNorthwest1 :: CloudWatchRegion
pattern CloudWatchRegionCnNorthwest1 = CloudWatchRegion' "cn-northwest-1"

pattern CloudWatchRegionCnNorth1 :: CloudWatchRegion
pattern CloudWatchRegionCnNorth1 = CloudWatchRegion' "cn-north-1"

pattern CloudWatchRegionAfSouth1 :: CloudWatchRegion
pattern CloudWatchRegionAfSouth1 = CloudWatchRegion' "af-south-1"

pattern CloudWatchRegionEuSouth1 :: CloudWatchRegion
pattern CloudWatchRegionEuSouth1 = CloudWatchRegion' "eu-south-1"

pattern CloudWatchRegionUsGovWest1 :: CloudWatchRegion
pattern CloudWatchRegionUsGovWest1 = CloudWatchRegion' "us-gov-west-1"

pattern CloudWatchRegionUsGovEast1 :: CloudWatchRegion
pattern CloudWatchRegionUsGovEast1 = CloudWatchRegion' "us-gov-east-1"

pattern CloudWatchRegionUsIsoEast1 :: CloudWatchRegion
pattern CloudWatchRegionUsIsoEast1 = CloudWatchRegion' "us-iso-east-1"

pattern CloudWatchRegionUsIsobEast1 :: CloudWatchRegion
pattern CloudWatchRegionUsIsobEast1 = CloudWatchRegion' "us-isob-east-1"

{-# COMPLETE
  CloudWatchRegionUsEast1,
  CloudWatchRegionUsEast2,
  CloudWatchRegionUsWest1,
  CloudWatchRegionUsWest2,
  CloudWatchRegionCaCentral1,
  CloudWatchRegionEuCentral1,
  CloudWatchRegionEuWest1,
  CloudWatchRegionEuWest2,
  CloudWatchRegionEuWest3,
  CloudWatchRegionApEast1,
  CloudWatchRegionMeSouth1,
  CloudWatchRegionApSouth1,
  CloudWatchRegionApSoutheast1,
  CloudWatchRegionApSoutheast2,
  CloudWatchRegionApNortheast1,
  CloudWatchRegionApNortheast2,
  CloudWatchRegionApNortheast3,
  CloudWatchRegionEuNorth1,
  CloudWatchRegionSaEast1,
  CloudWatchRegionCnNorthwest1,
  CloudWatchRegionCnNorth1,
  CloudWatchRegionAfSouth1,
  CloudWatchRegionEuSouth1,
  CloudWatchRegionUsGovWest1,
  CloudWatchRegionUsGovEast1,
  CloudWatchRegionUsIsoEast1,
  CloudWatchRegionUsIsobEast1,
  CloudWatchRegion'
  #-}
