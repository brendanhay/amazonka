{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.VPCRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.VPCRegion
  ( VPCRegion
      ( VPCRegion',
        AfSouth1,
        ApEast1,
        ApNortheast1,
        ApNortheast2,
        ApNortheast3,
        ApSouth1,
        ApSoutheast1,
        ApSoutheast2,
        CaCentral1,
        CnNorth1,
        EuCentral1,
        EuNorth1,
        EuSouth1,
        EuWest1,
        EuWest2,
        EuWest3,
        MeSouth1,
        SaEast1,
        UsEast1,
        UsEast2,
        UsGovEast1,
        UsGovWest1,
        UsIsoEast1,
        UsIsobEast1,
        UsWest1,
        UsWest2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype VPCRegion = VPCRegion' Lude.Text
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

pattern AfSouth1 :: VPCRegion
pattern AfSouth1 = VPCRegion' "af-south-1"

pattern ApEast1 :: VPCRegion
pattern ApEast1 = VPCRegion' "ap-east-1"

pattern ApNortheast1 :: VPCRegion
pattern ApNortheast1 = VPCRegion' "ap-northeast-1"

pattern ApNortheast2 :: VPCRegion
pattern ApNortheast2 = VPCRegion' "ap-northeast-2"

pattern ApNortheast3 :: VPCRegion
pattern ApNortheast3 = VPCRegion' "ap-northeast-3"

pattern ApSouth1 :: VPCRegion
pattern ApSouth1 = VPCRegion' "ap-south-1"

pattern ApSoutheast1 :: VPCRegion
pattern ApSoutheast1 = VPCRegion' "ap-southeast-1"

pattern ApSoutheast2 :: VPCRegion
pattern ApSoutheast2 = VPCRegion' "ap-southeast-2"

pattern CaCentral1 :: VPCRegion
pattern CaCentral1 = VPCRegion' "ca-central-1"

pattern CnNorth1 :: VPCRegion
pattern CnNorth1 = VPCRegion' "cn-north-1"

pattern EuCentral1 :: VPCRegion
pattern EuCentral1 = VPCRegion' "eu-central-1"

pattern EuNorth1 :: VPCRegion
pattern EuNorth1 = VPCRegion' "eu-north-1"

pattern EuSouth1 :: VPCRegion
pattern EuSouth1 = VPCRegion' "eu-south-1"

pattern EuWest1 :: VPCRegion
pattern EuWest1 = VPCRegion' "eu-west-1"

pattern EuWest2 :: VPCRegion
pattern EuWest2 = VPCRegion' "eu-west-2"

pattern EuWest3 :: VPCRegion
pattern EuWest3 = VPCRegion' "eu-west-3"

pattern MeSouth1 :: VPCRegion
pattern MeSouth1 = VPCRegion' "me-south-1"

pattern SaEast1 :: VPCRegion
pattern SaEast1 = VPCRegion' "sa-east-1"

pattern UsEast1 :: VPCRegion
pattern UsEast1 = VPCRegion' "us-east-1"

pattern UsEast2 :: VPCRegion
pattern UsEast2 = VPCRegion' "us-east-2"

pattern UsGovEast1 :: VPCRegion
pattern UsGovEast1 = VPCRegion' "us-gov-east-1"

pattern UsGovWest1 :: VPCRegion
pattern UsGovWest1 = VPCRegion' "us-gov-west-1"

pattern UsIsoEast1 :: VPCRegion
pattern UsIsoEast1 = VPCRegion' "us-iso-east-1"

pattern UsIsobEast1 :: VPCRegion
pattern UsIsobEast1 = VPCRegion' "us-isob-east-1"

pattern UsWest1 :: VPCRegion
pattern UsWest1 = VPCRegion' "us-west-1"

pattern UsWest2 :: VPCRegion
pattern UsWest2 = VPCRegion' "us-west-2"

{-# COMPLETE
  AfSouth1,
  ApEast1,
  ApNortheast1,
  ApNortheast2,
  ApNortheast3,
  ApSouth1,
  ApSoutheast1,
  ApSoutheast2,
  CaCentral1,
  CnNorth1,
  EuCentral1,
  EuNorth1,
  EuSouth1,
  EuWest1,
  EuWest2,
  EuWest3,
  MeSouth1,
  SaEast1,
  UsEast1,
  UsEast2,
  UsGovEast1,
  UsGovWest1,
  UsIsoEast1,
  UsIsobEast1,
  UsWest1,
  UsWest2,
  VPCRegion'
  #-}
