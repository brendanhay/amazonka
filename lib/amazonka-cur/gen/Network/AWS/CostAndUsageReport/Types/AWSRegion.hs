{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.AWSRegion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.AWSRegion
  ( AWSRegion
      ( AWSRegion',
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
        CnNorthwest1,
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
        UsWest1,
        UsWest2
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The region of the S3 bucket that AWS delivers the report into.
newtype AWSRegion = AWSRegion' Lude.Text
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

pattern AfSouth1 :: AWSRegion
pattern AfSouth1 = AWSRegion' "af-south-1"

pattern ApEast1 :: AWSRegion
pattern ApEast1 = AWSRegion' "ap-east-1"

pattern ApNortheast1 :: AWSRegion
pattern ApNortheast1 = AWSRegion' "ap-northeast-1"

pattern ApNortheast2 :: AWSRegion
pattern ApNortheast2 = AWSRegion' "ap-northeast-2"

pattern ApNortheast3 :: AWSRegion
pattern ApNortheast3 = AWSRegion' "ap-northeast-3"

pattern ApSouth1 :: AWSRegion
pattern ApSouth1 = AWSRegion' "ap-south-1"

pattern ApSoutheast1 :: AWSRegion
pattern ApSoutheast1 = AWSRegion' "ap-southeast-1"

pattern ApSoutheast2 :: AWSRegion
pattern ApSoutheast2 = AWSRegion' "ap-southeast-2"

pattern CaCentral1 :: AWSRegion
pattern CaCentral1 = AWSRegion' "ca-central-1"

pattern CnNorth1 :: AWSRegion
pattern CnNorth1 = AWSRegion' "cn-north-1"

pattern CnNorthwest1 :: AWSRegion
pattern CnNorthwest1 = AWSRegion' "cn-northwest-1"

pattern EuCentral1 :: AWSRegion
pattern EuCentral1 = AWSRegion' "eu-central-1"

pattern EuNorth1 :: AWSRegion
pattern EuNorth1 = AWSRegion' "eu-north-1"

pattern EuSouth1 :: AWSRegion
pattern EuSouth1 = AWSRegion' "eu-south-1"

pattern EuWest1 :: AWSRegion
pattern EuWest1 = AWSRegion' "eu-west-1"

pattern EuWest2 :: AWSRegion
pattern EuWest2 = AWSRegion' "eu-west-2"

pattern EuWest3 :: AWSRegion
pattern EuWest3 = AWSRegion' "eu-west-3"

pattern MeSouth1 :: AWSRegion
pattern MeSouth1 = AWSRegion' "me-south-1"

pattern SaEast1 :: AWSRegion
pattern SaEast1 = AWSRegion' "sa-east-1"

pattern UsEast1 :: AWSRegion
pattern UsEast1 = AWSRegion' "us-east-1"

pattern UsEast2 :: AWSRegion
pattern UsEast2 = AWSRegion' "us-east-2"

pattern UsWest1 :: AWSRegion
pattern UsWest1 = AWSRegion' "us-west-1"

pattern UsWest2 :: AWSRegion
pattern UsWest2 = AWSRegion' "us-west-2"

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
  CnNorthwest1,
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
  UsWest1,
  UsWest2,
  AWSRegion'
  #-}
