{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AWSRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostAndUsageReport.Types.AWSRegion
  ( AWSRegion
    ( AWSRegion'
    , AWSRegionAfSouth1
    , AWSRegionApEast1
    , AWSRegionApSouth1
    , AWSRegionApSoutheast1
    , AWSRegionApSoutheast2
    , AWSRegionApNortheast1
    , AWSRegionApNortheast2
    , AWSRegionApNortheast3
    , AWSRegionCaCentral1
    , AWSRegionEuCentral1
    , AWSRegionEuWest1
    , AWSRegionEuWest2
    , AWSRegionEuWest3
    , AWSRegionEuNorth1
    , AWSRegionEuSouth1
    , AWSRegionMeSouth1
    , AWSRegionSaEast1
    , AWSRegionUsEast1
    , AWSRegionUsEast2
    , AWSRegionUsWest1
    , AWSRegionUsWest2
    , AWSRegionCnNorth1
    , AWSRegionCnNorthwest1
    , fromAWSRegion
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The region of the S3 bucket that AWS delivers the report into.
newtype AWSRegion = AWSRegion'{fromAWSRegion :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern AWSRegionAfSouth1 :: AWSRegion
pattern AWSRegionAfSouth1 = AWSRegion' "af-south-1"

pattern AWSRegionApEast1 :: AWSRegion
pattern AWSRegionApEast1 = AWSRegion' "ap-east-1"

pattern AWSRegionApSouth1 :: AWSRegion
pattern AWSRegionApSouth1 = AWSRegion' "ap-south-1"

pattern AWSRegionApSoutheast1 :: AWSRegion
pattern AWSRegionApSoutheast1 = AWSRegion' "ap-southeast-1"

pattern AWSRegionApSoutheast2 :: AWSRegion
pattern AWSRegionApSoutheast2 = AWSRegion' "ap-southeast-2"

pattern AWSRegionApNortheast1 :: AWSRegion
pattern AWSRegionApNortheast1 = AWSRegion' "ap-northeast-1"

pattern AWSRegionApNortheast2 :: AWSRegion
pattern AWSRegionApNortheast2 = AWSRegion' "ap-northeast-2"

pattern AWSRegionApNortheast3 :: AWSRegion
pattern AWSRegionApNortheast3 = AWSRegion' "ap-northeast-3"

pattern AWSRegionCaCentral1 :: AWSRegion
pattern AWSRegionCaCentral1 = AWSRegion' "ca-central-1"

pattern AWSRegionEuCentral1 :: AWSRegion
pattern AWSRegionEuCentral1 = AWSRegion' "eu-central-1"

pattern AWSRegionEuWest1 :: AWSRegion
pattern AWSRegionEuWest1 = AWSRegion' "eu-west-1"

pattern AWSRegionEuWest2 :: AWSRegion
pattern AWSRegionEuWest2 = AWSRegion' "eu-west-2"

pattern AWSRegionEuWest3 :: AWSRegion
pattern AWSRegionEuWest3 = AWSRegion' "eu-west-3"

pattern AWSRegionEuNorth1 :: AWSRegion
pattern AWSRegionEuNorth1 = AWSRegion' "eu-north-1"

pattern AWSRegionEuSouth1 :: AWSRegion
pattern AWSRegionEuSouth1 = AWSRegion' "eu-south-1"

pattern AWSRegionMeSouth1 :: AWSRegion
pattern AWSRegionMeSouth1 = AWSRegion' "me-south-1"

pattern AWSRegionSaEast1 :: AWSRegion
pattern AWSRegionSaEast1 = AWSRegion' "sa-east-1"

pattern AWSRegionUsEast1 :: AWSRegion
pattern AWSRegionUsEast1 = AWSRegion' "us-east-1"

pattern AWSRegionUsEast2 :: AWSRegion
pattern AWSRegionUsEast2 = AWSRegion' "us-east-2"

pattern AWSRegionUsWest1 :: AWSRegion
pattern AWSRegionUsWest1 = AWSRegion' "us-west-1"

pattern AWSRegionUsWest2 :: AWSRegion
pattern AWSRegionUsWest2 = AWSRegion' "us-west-2"

pattern AWSRegionCnNorth1 :: AWSRegion
pattern AWSRegionCnNorth1 = AWSRegion' "cn-north-1"

pattern AWSRegionCnNorthwest1 :: AWSRegion
pattern AWSRegionCnNorthwest1 = AWSRegion' "cn-northwest-1"

{-# COMPLETE 
  AWSRegionAfSouth1,

  AWSRegionApEast1,

  AWSRegionApSouth1,

  AWSRegionApSoutheast1,

  AWSRegionApSoutheast2,

  AWSRegionApNortheast1,

  AWSRegionApNortheast2,

  AWSRegionApNortheast3,

  AWSRegionCaCentral1,

  AWSRegionEuCentral1,

  AWSRegionEuWest1,

  AWSRegionEuWest2,

  AWSRegionEuWest3,

  AWSRegionEuNorth1,

  AWSRegionEuSouth1,

  AWSRegionMeSouth1,

  AWSRegionSaEast1,

  AWSRegionUsEast1,

  AWSRegionUsEast2,

  AWSRegionUsWest1,

  AWSRegionUsWest2,

  AWSRegionCnNorth1,

  AWSRegionCnNorthwest1,
  AWSRegion'
  #-}
