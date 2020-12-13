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
        UsEast1,
        UsEast2,
        UsWest1,
        UsWest2,
        EuWest1,
        EuWest2,
        EuWest3,
        EuCentral1,
        CaCentral1,
        ApSouth1,
        ApSoutheast1,
        ApSoutheast2,
        ApNortheast1,
        ApNortheast2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RegionName = RegionName' Lude.Text
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

pattern UsEast1 :: RegionName
pattern UsEast1 = RegionName' "us-east-1"

pattern UsEast2 :: RegionName
pattern UsEast2 = RegionName' "us-east-2"

pattern UsWest1 :: RegionName
pattern UsWest1 = RegionName' "us-west-1"

pattern UsWest2 :: RegionName
pattern UsWest2 = RegionName' "us-west-2"

pattern EuWest1 :: RegionName
pattern EuWest1 = RegionName' "eu-west-1"

pattern EuWest2 :: RegionName
pattern EuWest2 = RegionName' "eu-west-2"

pattern EuWest3 :: RegionName
pattern EuWest3 = RegionName' "eu-west-3"

pattern EuCentral1 :: RegionName
pattern EuCentral1 = RegionName' "eu-central-1"

pattern CaCentral1 :: RegionName
pattern CaCentral1 = RegionName' "ca-central-1"

pattern ApSouth1 :: RegionName
pattern ApSouth1 = RegionName' "ap-south-1"

pattern ApSoutheast1 :: RegionName
pattern ApSoutheast1 = RegionName' "ap-southeast-1"

pattern ApSoutheast2 :: RegionName
pattern ApSoutheast2 = RegionName' "ap-southeast-2"

pattern ApNortheast1 :: RegionName
pattern ApNortheast1 = RegionName' "ap-northeast-1"

pattern ApNortheast2 :: RegionName
pattern ApNortheast2 = RegionName' "ap-northeast-2"

{-# COMPLETE
  UsEast1,
  UsEast2,
  UsWest1,
  UsWest2,
  EuWest1,
  EuWest2,
  EuWest3,
  EuCentral1,
  CaCentral1,
  ApSouth1,
  ApSoutheast1,
  ApSoutheast2,
  ApNortheast1,
  ApNortheast2,
  RegionName'
  #-}
