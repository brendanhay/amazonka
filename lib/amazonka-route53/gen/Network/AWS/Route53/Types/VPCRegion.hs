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
        VRUsEast1,
        VRUsEast2,
        VRUsWest1,
        VRUsWest2,
        VREuWest1,
        VREuWest2,
        VREuWest3,
        VREuCentral1,
        VRApEast1,
        VRMeSouth1,
        VRUsGovWest1,
        VRUsGovEast1,
        VRUsIsoEast1,
        VRUsIsobEast1,
        VRApSoutheast1,
        VRApSoutheast2,
        VRApSouth1,
        VRApNortheast1,
        VRApNortheast2,
        VRApNortheast3,
        VREuNorth1,
        VRSaEast1,
        VRCaCentral1,
        VRCnNorth1,
        VRAfSouth1,
        VREuSouth1
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

pattern VRUsEast1 :: VPCRegion
pattern VRUsEast1 = VPCRegion' "us-east-1"

pattern VRUsEast2 :: VPCRegion
pattern VRUsEast2 = VPCRegion' "us-east-2"

pattern VRUsWest1 :: VPCRegion
pattern VRUsWest1 = VPCRegion' "us-west-1"

pattern VRUsWest2 :: VPCRegion
pattern VRUsWest2 = VPCRegion' "us-west-2"

pattern VREuWest1 :: VPCRegion
pattern VREuWest1 = VPCRegion' "eu-west-1"

pattern VREuWest2 :: VPCRegion
pattern VREuWest2 = VPCRegion' "eu-west-2"

pattern VREuWest3 :: VPCRegion
pattern VREuWest3 = VPCRegion' "eu-west-3"

pattern VREuCentral1 :: VPCRegion
pattern VREuCentral1 = VPCRegion' "eu-central-1"

pattern VRApEast1 :: VPCRegion
pattern VRApEast1 = VPCRegion' "ap-east-1"

pattern VRMeSouth1 :: VPCRegion
pattern VRMeSouth1 = VPCRegion' "me-south-1"

pattern VRUsGovWest1 :: VPCRegion
pattern VRUsGovWest1 = VPCRegion' "us-gov-west-1"

pattern VRUsGovEast1 :: VPCRegion
pattern VRUsGovEast1 = VPCRegion' "us-gov-east-1"

pattern VRUsIsoEast1 :: VPCRegion
pattern VRUsIsoEast1 = VPCRegion' "us-iso-east-1"

pattern VRUsIsobEast1 :: VPCRegion
pattern VRUsIsobEast1 = VPCRegion' "us-isob-east-1"

pattern VRApSoutheast1 :: VPCRegion
pattern VRApSoutheast1 = VPCRegion' "ap-southeast-1"

pattern VRApSoutheast2 :: VPCRegion
pattern VRApSoutheast2 = VPCRegion' "ap-southeast-2"

pattern VRApSouth1 :: VPCRegion
pattern VRApSouth1 = VPCRegion' "ap-south-1"

pattern VRApNortheast1 :: VPCRegion
pattern VRApNortheast1 = VPCRegion' "ap-northeast-1"

pattern VRApNortheast2 :: VPCRegion
pattern VRApNortheast2 = VPCRegion' "ap-northeast-2"

pattern VRApNortheast3 :: VPCRegion
pattern VRApNortheast3 = VPCRegion' "ap-northeast-3"

pattern VREuNorth1 :: VPCRegion
pattern VREuNorth1 = VPCRegion' "eu-north-1"

pattern VRSaEast1 :: VPCRegion
pattern VRSaEast1 = VPCRegion' "sa-east-1"

pattern VRCaCentral1 :: VPCRegion
pattern VRCaCentral1 = VPCRegion' "ca-central-1"

pattern VRCnNorth1 :: VPCRegion
pattern VRCnNorth1 = VPCRegion' "cn-north-1"

pattern VRAfSouth1 :: VPCRegion
pattern VRAfSouth1 = VPCRegion' "af-south-1"

pattern VREuSouth1 :: VPCRegion
pattern VREuSouth1 = VPCRegion' "eu-south-1"

{-# COMPLETE
  VRUsEast1,
  VRUsEast2,
  VRUsWest1,
  VRUsWest2,
  VREuWest1,
  VREuWest2,
  VREuWest3,
  VREuCentral1,
  VRApEast1,
  VRMeSouth1,
  VRUsGovWest1,
  VRUsGovEast1,
  VRUsIsoEast1,
  VRUsIsobEast1,
  VRApSoutheast1,
  VRApSoutheast2,
  VRApSouth1,
  VRApNortheast1,
  VRApNortheast2,
  VRApNortheast3,
  VREuNorth1,
  VRSaEast1,
  VRCaCentral1,
  VRCnNorth1,
  VRAfSouth1,
  VREuSouth1,
  VPCRegion'
  #-}
