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
        CWRAfSouth1,
        CWRApEast1,
        CWRApNortheast1,
        CWRApNortheast2,
        CWRApNortheast3,
        CWRApSouth1,
        CWRApSoutheast1,
        CWRApSoutheast2,
        CWRCaCentral1,
        CWRCnNorth1,
        CWRCnNorthwest1,
        CWREuCentral1,
        CWREuNorth1,
        CWREuSouth1,
        CWREuWest1,
        CWREuWest2,
        CWREuWest3,
        CWRMeSouth1,
        CWRSaEast1,
        CWRUsEast1,
        CWRUsEast2,
        CWRUsGovEast1,
        CWRUsGovWest1,
        CWRUsIsoEast1,
        CWRUsIsobEast1,
        CWRUsWest1,
        CWRUsWest2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

newtype CloudWatchRegion = CloudWatchRegion' Lude.Text
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

pattern CWRAfSouth1 :: CloudWatchRegion
pattern CWRAfSouth1 = CloudWatchRegion' "af-south-1"

pattern CWRApEast1 :: CloudWatchRegion
pattern CWRApEast1 = CloudWatchRegion' "ap-east-1"

pattern CWRApNortheast1 :: CloudWatchRegion
pattern CWRApNortheast1 = CloudWatchRegion' "ap-northeast-1"

pattern CWRApNortheast2 :: CloudWatchRegion
pattern CWRApNortheast2 = CloudWatchRegion' "ap-northeast-2"

pattern CWRApNortheast3 :: CloudWatchRegion
pattern CWRApNortheast3 = CloudWatchRegion' "ap-northeast-3"

pattern CWRApSouth1 :: CloudWatchRegion
pattern CWRApSouth1 = CloudWatchRegion' "ap-south-1"

pattern CWRApSoutheast1 :: CloudWatchRegion
pattern CWRApSoutheast1 = CloudWatchRegion' "ap-southeast-1"

pattern CWRApSoutheast2 :: CloudWatchRegion
pattern CWRApSoutheast2 = CloudWatchRegion' "ap-southeast-2"

pattern CWRCaCentral1 :: CloudWatchRegion
pattern CWRCaCentral1 = CloudWatchRegion' "ca-central-1"

pattern CWRCnNorth1 :: CloudWatchRegion
pattern CWRCnNorth1 = CloudWatchRegion' "cn-north-1"

pattern CWRCnNorthwest1 :: CloudWatchRegion
pattern CWRCnNorthwest1 = CloudWatchRegion' "cn-northwest-1"

pattern CWREuCentral1 :: CloudWatchRegion
pattern CWREuCentral1 = CloudWatchRegion' "eu-central-1"

pattern CWREuNorth1 :: CloudWatchRegion
pattern CWREuNorth1 = CloudWatchRegion' "eu-north-1"

pattern CWREuSouth1 :: CloudWatchRegion
pattern CWREuSouth1 = CloudWatchRegion' "eu-south-1"

pattern CWREuWest1 :: CloudWatchRegion
pattern CWREuWest1 = CloudWatchRegion' "eu-west-1"

pattern CWREuWest2 :: CloudWatchRegion
pattern CWREuWest2 = CloudWatchRegion' "eu-west-2"

pattern CWREuWest3 :: CloudWatchRegion
pattern CWREuWest3 = CloudWatchRegion' "eu-west-3"

pattern CWRMeSouth1 :: CloudWatchRegion
pattern CWRMeSouth1 = CloudWatchRegion' "me-south-1"

pattern CWRSaEast1 :: CloudWatchRegion
pattern CWRSaEast1 = CloudWatchRegion' "sa-east-1"

pattern CWRUsEast1 :: CloudWatchRegion
pattern CWRUsEast1 = CloudWatchRegion' "us-east-1"

pattern CWRUsEast2 :: CloudWatchRegion
pattern CWRUsEast2 = CloudWatchRegion' "us-east-2"

pattern CWRUsGovEast1 :: CloudWatchRegion
pattern CWRUsGovEast1 = CloudWatchRegion' "us-gov-east-1"

pattern CWRUsGovWest1 :: CloudWatchRegion
pattern CWRUsGovWest1 = CloudWatchRegion' "us-gov-west-1"

pattern CWRUsIsoEast1 :: CloudWatchRegion
pattern CWRUsIsoEast1 = CloudWatchRegion' "us-iso-east-1"

pattern CWRUsIsobEast1 :: CloudWatchRegion
pattern CWRUsIsobEast1 = CloudWatchRegion' "us-isob-east-1"

pattern CWRUsWest1 :: CloudWatchRegion
pattern CWRUsWest1 = CloudWatchRegion' "us-west-1"

pattern CWRUsWest2 :: CloudWatchRegion
pattern CWRUsWest2 = CloudWatchRegion' "us-west-2"

{-# COMPLETE
  CWRAfSouth1,
  CWRApEast1,
  CWRApNortheast1,
  CWRApNortheast2,
  CWRApNortheast3,
  CWRApSouth1,
  CWRApSoutheast1,
  CWRApSoutheast2,
  CWRCaCentral1,
  CWRCnNorth1,
  CWRCnNorthwest1,
  CWREuCentral1,
  CWREuNorth1,
  CWREuSouth1,
  CWREuWest1,
  CWREuWest2,
  CWREuWest3,
  CWRMeSouth1,
  CWRSaEast1,
  CWRUsEast1,
  CWRUsEast2,
  CWRUsGovEast1,
  CWRUsGovWest1,
  CWRUsIsoEast1,
  CWRUsIsobEast1,
  CWRUsWest1,
  CWRUsWest2,
  CloudWatchRegion'
  #-}
