{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RegionName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RegionName
  ( RegionName
      ( RegionName',
        RegionNameUsEast1,
        RegionNameUsEast2,
        RegionNameUsWest1,
        RegionNameUsWest2,
        RegionNameEuWest1,
        RegionNameEuWest2,
        RegionNameEuWest3,
        RegionNameEuCentral1,
        RegionNameCaCentral1,
        RegionNameApSouth1,
        RegionNameApSoutheast1,
        RegionNameApSoutheast2,
        RegionNameApNortheast1,
        RegionNameApNortheast2,
        fromRegionName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RegionName = RegionName' {fromRegionName :: Core.Text}
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

pattern RegionNameUsEast1 :: RegionName
pattern RegionNameUsEast1 = RegionName' "us-east-1"

pattern RegionNameUsEast2 :: RegionName
pattern RegionNameUsEast2 = RegionName' "us-east-2"

pattern RegionNameUsWest1 :: RegionName
pattern RegionNameUsWest1 = RegionName' "us-west-1"

pattern RegionNameUsWest2 :: RegionName
pattern RegionNameUsWest2 = RegionName' "us-west-2"

pattern RegionNameEuWest1 :: RegionName
pattern RegionNameEuWest1 = RegionName' "eu-west-1"

pattern RegionNameEuWest2 :: RegionName
pattern RegionNameEuWest2 = RegionName' "eu-west-2"

pattern RegionNameEuWest3 :: RegionName
pattern RegionNameEuWest3 = RegionName' "eu-west-3"

pattern RegionNameEuCentral1 :: RegionName
pattern RegionNameEuCentral1 = RegionName' "eu-central-1"

pattern RegionNameCaCentral1 :: RegionName
pattern RegionNameCaCentral1 = RegionName' "ca-central-1"

pattern RegionNameApSouth1 :: RegionName
pattern RegionNameApSouth1 = RegionName' "ap-south-1"

pattern RegionNameApSoutheast1 :: RegionName
pattern RegionNameApSoutheast1 = RegionName' "ap-southeast-1"

pattern RegionNameApSoutheast2 :: RegionName
pattern RegionNameApSoutheast2 = RegionName' "ap-southeast-2"

pattern RegionNameApNortheast1 :: RegionName
pattern RegionNameApNortheast1 = RegionName' "ap-northeast-1"

pattern RegionNameApNortheast2 :: RegionName
pattern RegionNameApNortheast2 = RegionName' "ap-northeast-2"

{-# COMPLETE
  RegionNameUsEast1,
  RegionNameUsEast2,
  RegionNameUsWest1,
  RegionNameUsWest2,
  RegionNameEuWest1,
  RegionNameEuWest2,
  RegionNameEuWest3,
  RegionNameEuCentral1,
  RegionNameCaCentral1,
  RegionNameApSouth1,
  RegionNameApSoutheast1,
  RegionNameApSoutheast2,
  RegionNameApNortheast1,
  RegionNameApNortheast2,
  RegionName'
  #-}
