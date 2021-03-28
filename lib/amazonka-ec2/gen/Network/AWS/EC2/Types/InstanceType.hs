{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceType
  ( InstanceType
    ( InstanceType'
    , InstanceTypeT1_Micro
    , InstanceTypeT2_Nano
    , InstanceTypeT2_Micro
    , InstanceTypeT2_Small
    , InstanceTypeT2_Medium
    , InstanceTypeT2_Large
    , InstanceTypeT2_Xlarge
    , InstanceTypeT2_2xlarge
    , InstanceTypeT3_Nano
    , InstanceTypeT3_Micro
    , InstanceTypeT3_Small
    , InstanceTypeT3_Medium
    , InstanceTypeT3_Large
    , InstanceTypeT3_Xlarge
    , InstanceTypeT3_2xlarge
    , InstanceTypeT3a_Nano
    , InstanceTypeT3a_Micro
    , InstanceTypeT3a_Small
    , InstanceTypeT3a_Medium
    , InstanceTypeT3a_Large
    , InstanceTypeT3a_Xlarge
    , InstanceTypeT3a_2xlarge
    , InstanceTypeT4g_Nano
    , InstanceTypeT4g_Micro
    , InstanceTypeT4g_Small
    , InstanceTypeT4g_Medium
    , InstanceTypeT4g_Large
    , InstanceTypeT4g_Xlarge
    , InstanceTypeT4g_2xlarge
    , InstanceTypeM1_Small
    , InstanceTypeM1_Medium
    , InstanceTypeM1_Large
    , InstanceTypeM1_Xlarge
    , InstanceTypeM3_Medium
    , InstanceTypeM3_Large
    , InstanceTypeM3_Xlarge
    , InstanceTypeM3_2xlarge
    , InstanceTypeM4_Large
    , InstanceTypeM4_Xlarge
    , InstanceTypeM4_2xlarge
    , InstanceTypeM4_4xlarge
    , InstanceTypeM4_10xlarge
    , InstanceTypeM4_16xlarge
    , InstanceTypeM2_Xlarge
    , InstanceTypeM2_2xlarge
    , InstanceTypeM2_4xlarge
    , InstanceTypeCR1_8xlarge
    , InstanceTypeR3_Large
    , InstanceTypeR3_Xlarge
    , InstanceTypeR3_2xlarge
    , InstanceTypeR3_4xlarge
    , InstanceTypeR3_8xlarge
    , InstanceTypeR4_Large
    , InstanceTypeR4_Xlarge
    , InstanceTypeR4_2xlarge
    , InstanceTypeR4_4xlarge
    , InstanceTypeR4_8xlarge
    , InstanceTypeR4_16xlarge
    , InstanceTypeR5_Large
    , InstanceTypeR5_Xlarge
    , InstanceTypeR5_2xlarge
    , InstanceTypeR5_4xlarge
    , InstanceTypeR5_8xlarge
    , InstanceTypeR5_12xlarge
    , InstanceTypeR5_16xlarge
    , InstanceTypeR5_24xlarge
    , InstanceTypeR5_Metal
    , InstanceTypeR5a_Large
    , InstanceTypeR5a_Xlarge
    , InstanceTypeR5a_2xlarge
    , InstanceTypeR5a_4xlarge
    , InstanceTypeR5a_8xlarge
    , InstanceTypeR5a_12xlarge
    , InstanceTypeR5a_16xlarge
    , InstanceTypeR5a_24xlarge
    , InstanceTypeR5d_Large
    , InstanceTypeR5d_Xlarge
    , InstanceTypeR5d_2xlarge
    , InstanceTypeR5d_4xlarge
    , InstanceTypeR5d_8xlarge
    , InstanceTypeR5d_12xlarge
    , InstanceTypeR5d_16xlarge
    , InstanceTypeR5d_24xlarge
    , InstanceTypeR5d_Metal
    , InstanceTypeR5ad_Large
    , InstanceTypeR5ad_Xlarge
    , InstanceTypeR5ad_2xlarge
    , InstanceTypeR5ad_4xlarge
    , InstanceTypeR5ad_8xlarge
    , InstanceTypeR5ad_12xlarge
    , InstanceTypeR5ad_16xlarge
    , InstanceTypeR5ad_24xlarge
    , InstanceTypeR6g_Metal
    , InstanceTypeR6g_Medium
    , InstanceTypeR6g_Large
    , InstanceTypeR6g_Xlarge
    , InstanceTypeR6g_2xlarge
    , InstanceTypeR6g_4xlarge
    , InstanceTypeR6g_8xlarge
    , InstanceTypeR6g_12xlarge
    , InstanceTypeR6g_16xlarge
    , InstanceTypeR6gd_Metal
    , InstanceTypeR6gd_Medium
    , InstanceTypeR6gd_Large
    , InstanceTypeR6gd_Xlarge
    , InstanceTypeR6gd_2xlarge
    , InstanceTypeR6gd_4xlarge
    , InstanceTypeR6gd_8xlarge
    , InstanceTypeR6gd_12xlarge
    , InstanceTypeR6gd_16xlarge
    , InstanceTypeX1_16xlarge
    , InstanceTypeX1_32xlarge
    , InstanceTypeX1e_Xlarge
    , InstanceTypeX1e_2xlarge
    , InstanceTypeX1e_4xlarge
    , InstanceTypeX1e_8xlarge
    , InstanceTypeX1e_16xlarge
    , InstanceTypeX1e_32xlarge
    , InstanceTypeI2_Xlarge
    , InstanceTypeI2_2xlarge
    , InstanceTypeI2_4xlarge
    , InstanceTypeI2_8xlarge
    , InstanceTypeI3_Large
    , InstanceTypeI3_Xlarge
    , InstanceTypeI3_2xlarge
    , InstanceTypeI3_4xlarge
    , InstanceTypeI3_8xlarge
    , InstanceTypeI3_16xlarge
    , InstanceTypeI3_Metal
    , InstanceTypeI3en_Large
    , InstanceTypeI3en_Xlarge
    , InstanceTypeI3en_2xlarge
    , InstanceTypeI3en_3xlarge
    , InstanceTypeI3en_6xlarge
    , InstanceTypeI3en_12xlarge
    , InstanceTypeI3en_24xlarge
    , InstanceTypeI3en_Metal
    , InstanceTypeHI1_4xlarge
    , InstanceTypeHS1_8xlarge
    , InstanceTypeC1_Medium
    , InstanceTypeC1_Xlarge
    , InstanceTypeC3_Large
    , InstanceTypeC3_Xlarge
    , InstanceTypeC3_2xlarge
    , InstanceTypeC3_4xlarge
    , InstanceTypeC3_8xlarge
    , InstanceTypeC4_Large
    , InstanceTypeC4_Xlarge
    , InstanceTypeC4_2xlarge
    , InstanceTypeC4_4xlarge
    , InstanceTypeC4_8xlarge
    , InstanceTypeC5_Large
    , InstanceTypeC5_Xlarge
    , InstanceTypeC5_2xlarge
    , InstanceTypeC5_4xlarge
    , InstanceTypeC5_9xlarge
    , InstanceTypeC5_12xlarge
    , InstanceTypeC5_18xlarge
    , InstanceTypeC5_24xlarge
    , InstanceTypeC5_Metal
    , InstanceTypeC5a_Large
    , InstanceTypeC5a_Xlarge
    , InstanceTypeC5a_2xlarge
    , InstanceTypeC5a_4xlarge
    , InstanceTypeC5a_8xlarge
    , InstanceTypeC5a_12xlarge
    , InstanceTypeC5a_16xlarge
    , InstanceTypeC5a_24xlarge
    , InstanceTypeC5ad_Large
    , InstanceTypeC5ad_Xlarge
    , InstanceTypeC5ad_2xlarge
    , InstanceTypeC5ad_4xlarge
    , InstanceTypeC5ad_8xlarge
    , InstanceTypeC5ad_12xlarge
    , InstanceTypeC5ad_16xlarge
    , InstanceTypeC5ad_24xlarge
    , InstanceTypeC5d_Large
    , InstanceTypeC5d_Xlarge
    , InstanceTypeC5d_2xlarge
    , InstanceTypeC5d_4xlarge
    , InstanceTypeC5d_9xlarge
    , InstanceTypeC5d_12xlarge
    , InstanceTypeC5d_18xlarge
    , InstanceTypeC5d_24xlarge
    , InstanceTypeC5d_Metal
    , InstanceTypeC5n_Large
    , InstanceTypeC5n_Xlarge
    , InstanceTypeC5n_2xlarge
    , InstanceTypeC5n_4xlarge
    , InstanceTypeC5n_9xlarge
    , InstanceTypeC5n_18xlarge
    , InstanceTypeC6g_Metal
    , InstanceTypeC6g_Medium
    , InstanceTypeC6g_Large
    , InstanceTypeC6g_Xlarge
    , InstanceTypeC6g_2xlarge
    , InstanceTypeC6g_4xlarge
    , InstanceTypeC6g_8xlarge
    , InstanceTypeC6g_12xlarge
    , InstanceTypeC6g_16xlarge
    , InstanceTypeC6gd_Metal
    , InstanceTypeC6gd_Medium
    , InstanceTypeC6gd_Large
    , InstanceTypeC6gd_Xlarge
    , InstanceTypeC6gd_2xlarge
    , InstanceTypeC6gd_4xlarge
    , InstanceTypeC6gd_8xlarge
    , InstanceTypeC6gd_12xlarge
    , InstanceTypeC6gd_16xlarge
    , InstanceTypeCC1_4xlarge
    , InstanceTypeCC2_8xlarge
    , InstanceTypeG2_2xlarge
    , InstanceTypeG2_8xlarge
    , InstanceTypeG3_4xlarge
    , InstanceTypeG3_8xlarge
    , InstanceTypeG3_16xlarge
    , InstanceTypeG3s_Xlarge
    , InstanceTypeG4dn_Xlarge
    , InstanceTypeG4dn_2xlarge
    , InstanceTypeG4dn_4xlarge
    , InstanceTypeG4dn_8xlarge
    , InstanceTypeG4dn_12xlarge
    , InstanceTypeG4dn_16xlarge
    , InstanceTypeG4dn_Metal
    , InstanceTypeCG1_4xlarge
    , InstanceTypeP2_Xlarge
    , InstanceTypeP2_8xlarge
    , InstanceTypeP2_16xlarge
    , InstanceTypeP3_2xlarge
    , InstanceTypeP3_8xlarge
    , InstanceTypeP3_16xlarge
    , InstanceTypeP3dn_24xlarge
    , InstanceTypeP4d_24xlarge
    , InstanceTypeD2_Xlarge
    , InstanceTypeD2_2xlarge
    , InstanceTypeD2_4xlarge
    , InstanceTypeD2_8xlarge
    , InstanceTypeF1_2xlarge
    , InstanceTypeF1_4xlarge
    , InstanceTypeF1_16xlarge
    , InstanceTypeM5_Large
    , InstanceTypeM5_Xlarge
    , InstanceTypeM5_2xlarge
    , InstanceTypeM5_4xlarge
    , InstanceTypeM5_8xlarge
    , InstanceTypeM5_12xlarge
    , InstanceTypeM5_16xlarge
    , InstanceTypeM5_24xlarge
    , InstanceTypeM5_Metal
    , InstanceTypeM5a_Large
    , InstanceTypeM5a_Xlarge
    , InstanceTypeM5a_2xlarge
    , InstanceTypeM5a_4xlarge
    , InstanceTypeM5a_8xlarge
    , InstanceTypeM5a_12xlarge
    , InstanceTypeM5a_16xlarge
    , InstanceTypeM5a_24xlarge
    , InstanceTypeM5d_Large
    , InstanceTypeM5d_Xlarge
    , InstanceTypeM5d_2xlarge
    , InstanceTypeM5d_4xlarge
    , InstanceTypeM5d_8xlarge
    , InstanceTypeM5d_12xlarge
    , InstanceTypeM5d_16xlarge
    , InstanceTypeM5d_24xlarge
    , InstanceTypeM5d_Metal
    , InstanceTypeM5ad_Large
    , InstanceTypeM5ad_Xlarge
    , InstanceTypeM5ad_2xlarge
    , InstanceTypeM5ad_4xlarge
    , InstanceTypeM5ad_8xlarge
    , InstanceTypeM5ad_12xlarge
    , InstanceTypeM5ad_16xlarge
    , InstanceTypeM5ad_24xlarge
    , InstanceTypeH1_2xlarge
    , InstanceTypeH1_4xlarge
    , InstanceTypeH1_8xlarge
    , InstanceTypeH1_16xlarge
    , InstanceTypeZ1d_Large
    , InstanceTypeZ1d_Xlarge
    , InstanceTypeZ1d_2xlarge
    , InstanceTypeZ1d_3xlarge
    , InstanceTypeZ1d_6xlarge
    , InstanceTypeZ1d_12xlarge
    , InstanceTypeZ1d_Metal
    , InstanceTypeU6TB1_Metal
    , InstanceTypeU9TB1_Metal
    , InstanceTypeU12TB1_Metal
    , InstanceTypeU18TB1_Metal
    , InstanceTypeU24TB1_Metal
    , InstanceTypeA1_Medium
    , InstanceTypeA1_Large
    , InstanceTypeA1_Xlarge
    , InstanceTypeA1_2xlarge
    , InstanceTypeA1_4xlarge
    , InstanceTypeA1_Metal
    , InstanceTypeM5dn_Large
    , InstanceTypeM5dn_Xlarge
    , InstanceTypeM5dn_2xlarge
    , InstanceTypeM5dn_4xlarge
    , InstanceTypeM5dn_8xlarge
    , InstanceTypeM5dn_12xlarge
    , InstanceTypeM5dn_16xlarge
    , InstanceTypeM5dn_24xlarge
    , InstanceTypeM5n_Large
    , InstanceTypeM5n_Xlarge
    , InstanceTypeM5n_2xlarge
    , InstanceTypeM5n_4xlarge
    , InstanceTypeM5n_8xlarge
    , InstanceTypeM5n_12xlarge
    , InstanceTypeM5n_16xlarge
    , InstanceTypeM5n_24xlarge
    , InstanceTypeR5dn_Large
    , InstanceTypeR5dn_Xlarge
    , InstanceTypeR5dn_2xlarge
    , InstanceTypeR5dn_4xlarge
    , InstanceTypeR5dn_8xlarge
    , InstanceTypeR5dn_12xlarge
    , InstanceTypeR5dn_16xlarge
    , InstanceTypeR5dn_24xlarge
    , InstanceTypeR5n_Large
    , InstanceTypeR5n_Xlarge
    , InstanceTypeR5n_2xlarge
    , InstanceTypeR5n_4xlarge
    , InstanceTypeR5n_8xlarge
    , InstanceTypeR5n_12xlarge
    , InstanceTypeR5n_16xlarge
    , InstanceTypeR5n_24xlarge
    , InstanceTypeINF1_Xlarge
    , InstanceTypeINF1_2xlarge
    , InstanceTypeINF1_6xlarge
    , InstanceTypeINF1_24xlarge
    , InstanceTypeM6g_Metal
    , InstanceTypeM6g_Medium
    , InstanceTypeM6g_Large
    , InstanceTypeM6g_Xlarge
    , InstanceTypeM6g_2xlarge
    , InstanceTypeM6g_4xlarge
    , InstanceTypeM6g_8xlarge
    , InstanceTypeM6g_12xlarge
    , InstanceTypeM6g_16xlarge
    , InstanceTypeM6gd_Metal
    , InstanceTypeM6gd_Medium
    , InstanceTypeM6gd_Large
    , InstanceTypeM6gd_Xlarge
    , InstanceTypeM6gd_2xlarge
    , InstanceTypeM6gd_4xlarge
    , InstanceTypeM6gd_8xlarge
    , InstanceTypeM6gd_12xlarge
    , InstanceTypeM6gd_16xlarge
    , fromInstanceType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceType = InstanceType'{fromInstanceType :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern InstanceTypeT1_Micro :: InstanceType
pattern InstanceTypeT1_Micro = InstanceType' "t1.micro"

pattern InstanceTypeT2_Nano :: InstanceType
pattern InstanceTypeT2_Nano = InstanceType' "t2.nano"

pattern InstanceTypeT2_Micro :: InstanceType
pattern InstanceTypeT2_Micro = InstanceType' "t2.micro"

pattern InstanceTypeT2_Small :: InstanceType
pattern InstanceTypeT2_Small = InstanceType' "t2.small"

pattern InstanceTypeT2_Medium :: InstanceType
pattern InstanceTypeT2_Medium = InstanceType' "t2.medium"

pattern InstanceTypeT2_Large :: InstanceType
pattern InstanceTypeT2_Large = InstanceType' "t2.large"

pattern InstanceTypeT2_Xlarge :: InstanceType
pattern InstanceTypeT2_Xlarge = InstanceType' "t2.xlarge"

pattern InstanceTypeT2_2xlarge :: InstanceType
pattern InstanceTypeT2_2xlarge = InstanceType' "t2.2xlarge"

pattern InstanceTypeT3_Nano :: InstanceType
pattern InstanceTypeT3_Nano = InstanceType' "t3.nano"

pattern InstanceTypeT3_Micro :: InstanceType
pattern InstanceTypeT3_Micro = InstanceType' "t3.micro"

pattern InstanceTypeT3_Small :: InstanceType
pattern InstanceTypeT3_Small = InstanceType' "t3.small"

pattern InstanceTypeT3_Medium :: InstanceType
pattern InstanceTypeT3_Medium = InstanceType' "t3.medium"

pattern InstanceTypeT3_Large :: InstanceType
pattern InstanceTypeT3_Large = InstanceType' "t3.large"

pattern InstanceTypeT3_Xlarge :: InstanceType
pattern InstanceTypeT3_Xlarge = InstanceType' "t3.xlarge"

pattern InstanceTypeT3_2xlarge :: InstanceType
pattern InstanceTypeT3_2xlarge = InstanceType' "t3.2xlarge"

pattern InstanceTypeT3a_Nano :: InstanceType
pattern InstanceTypeT3a_Nano = InstanceType' "t3a.nano"

pattern InstanceTypeT3a_Micro :: InstanceType
pattern InstanceTypeT3a_Micro = InstanceType' "t3a.micro"

pattern InstanceTypeT3a_Small :: InstanceType
pattern InstanceTypeT3a_Small = InstanceType' "t3a.small"

pattern InstanceTypeT3a_Medium :: InstanceType
pattern InstanceTypeT3a_Medium = InstanceType' "t3a.medium"

pattern InstanceTypeT3a_Large :: InstanceType
pattern InstanceTypeT3a_Large = InstanceType' "t3a.large"

pattern InstanceTypeT3a_Xlarge :: InstanceType
pattern InstanceTypeT3a_Xlarge = InstanceType' "t3a.xlarge"

pattern InstanceTypeT3a_2xlarge :: InstanceType
pattern InstanceTypeT3a_2xlarge = InstanceType' "t3a.2xlarge"

pattern InstanceTypeT4g_Nano :: InstanceType
pattern InstanceTypeT4g_Nano = InstanceType' "t4g.nano"

pattern InstanceTypeT4g_Micro :: InstanceType
pattern InstanceTypeT4g_Micro = InstanceType' "t4g.micro"

pattern InstanceTypeT4g_Small :: InstanceType
pattern InstanceTypeT4g_Small = InstanceType' "t4g.small"

pattern InstanceTypeT4g_Medium :: InstanceType
pattern InstanceTypeT4g_Medium = InstanceType' "t4g.medium"

pattern InstanceTypeT4g_Large :: InstanceType
pattern InstanceTypeT4g_Large = InstanceType' "t4g.large"

pattern InstanceTypeT4g_Xlarge :: InstanceType
pattern InstanceTypeT4g_Xlarge = InstanceType' "t4g.xlarge"

pattern InstanceTypeT4g_2xlarge :: InstanceType
pattern InstanceTypeT4g_2xlarge = InstanceType' "t4g.2xlarge"

pattern InstanceTypeM1_Small :: InstanceType
pattern InstanceTypeM1_Small = InstanceType' "m1.small"

pattern InstanceTypeM1_Medium :: InstanceType
pattern InstanceTypeM1_Medium = InstanceType' "m1.medium"

pattern InstanceTypeM1_Large :: InstanceType
pattern InstanceTypeM1_Large = InstanceType' "m1.large"

pattern InstanceTypeM1_Xlarge :: InstanceType
pattern InstanceTypeM1_Xlarge = InstanceType' "m1.xlarge"

pattern InstanceTypeM3_Medium :: InstanceType
pattern InstanceTypeM3_Medium = InstanceType' "m3.medium"

pattern InstanceTypeM3_Large :: InstanceType
pattern InstanceTypeM3_Large = InstanceType' "m3.large"

pattern InstanceTypeM3_Xlarge :: InstanceType
pattern InstanceTypeM3_Xlarge = InstanceType' "m3.xlarge"

pattern InstanceTypeM3_2xlarge :: InstanceType
pattern InstanceTypeM3_2xlarge = InstanceType' "m3.2xlarge"

pattern InstanceTypeM4_Large :: InstanceType
pattern InstanceTypeM4_Large = InstanceType' "m4.large"

pattern InstanceTypeM4_Xlarge :: InstanceType
pattern InstanceTypeM4_Xlarge = InstanceType' "m4.xlarge"

pattern InstanceTypeM4_2xlarge :: InstanceType
pattern InstanceTypeM4_2xlarge = InstanceType' "m4.2xlarge"

pattern InstanceTypeM4_4xlarge :: InstanceType
pattern InstanceTypeM4_4xlarge = InstanceType' "m4.4xlarge"

pattern InstanceTypeM4_10xlarge :: InstanceType
pattern InstanceTypeM4_10xlarge = InstanceType' "m4.10xlarge"

pattern InstanceTypeM4_16xlarge :: InstanceType
pattern InstanceTypeM4_16xlarge = InstanceType' "m4.16xlarge"

pattern InstanceTypeM2_Xlarge :: InstanceType
pattern InstanceTypeM2_Xlarge = InstanceType' "m2.xlarge"

pattern InstanceTypeM2_2xlarge :: InstanceType
pattern InstanceTypeM2_2xlarge = InstanceType' "m2.2xlarge"

pattern InstanceTypeM2_4xlarge :: InstanceType
pattern InstanceTypeM2_4xlarge = InstanceType' "m2.4xlarge"

pattern InstanceTypeCR1_8xlarge :: InstanceType
pattern InstanceTypeCR1_8xlarge = InstanceType' "cr1.8xlarge"

pattern InstanceTypeR3_Large :: InstanceType
pattern InstanceTypeR3_Large = InstanceType' "r3.large"

pattern InstanceTypeR3_Xlarge :: InstanceType
pattern InstanceTypeR3_Xlarge = InstanceType' "r3.xlarge"

pattern InstanceTypeR3_2xlarge :: InstanceType
pattern InstanceTypeR3_2xlarge = InstanceType' "r3.2xlarge"

pattern InstanceTypeR3_4xlarge :: InstanceType
pattern InstanceTypeR3_4xlarge = InstanceType' "r3.4xlarge"

pattern InstanceTypeR3_8xlarge :: InstanceType
pattern InstanceTypeR3_8xlarge = InstanceType' "r3.8xlarge"

pattern InstanceTypeR4_Large :: InstanceType
pattern InstanceTypeR4_Large = InstanceType' "r4.large"

pattern InstanceTypeR4_Xlarge :: InstanceType
pattern InstanceTypeR4_Xlarge = InstanceType' "r4.xlarge"

pattern InstanceTypeR4_2xlarge :: InstanceType
pattern InstanceTypeR4_2xlarge = InstanceType' "r4.2xlarge"

pattern InstanceTypeR4_4xlarge :: InstanceType
pattern InstanceTypeR4_4xlarge = InstanceType' "r4.4xlarge"

pattern InstanceTypeR4_8xlarge :: InstanceType
pattern InstanceTypeR4_8xlarge = InstanceType' "r4.8xlarge"

pattern InstanceTypeR4_16xlarge :: InstanceType
pattern InstanceTypeR4_16xlarge = InstanceType' "r4.16xlarge"

pattern InstanceTypeR5_Large :: InstanceType
pattern InstanceTypeR5_Large = InstanceType' "r5.large"

pattern InstanceTypeR5_Xlarge :: InstanceType
pattern InstanceTypeR5_Xlarge = InstanceType' "r5.xlarge"

pattern InstanceTypeR5_2xlarge :: InstanceType
pattern InstanceTypeR5_2xlarge = InstanceType' "r5.2xlarge"

pattern InstanceTypeR5_4xlarge :: InstanceType
pattern InstanceTypeR5_4xlarge = InstanceType' "r5.4xlarge"

pattern InstanceTypeR5_8xlarge :: InstanceType
pattern InstanceTypeR5_8xlarge = InstanceType' "r5.8xlarge"

pattern InstanceTypeR5_12xlarge :: InstanceType
pattern InstanceTypeR5_12xlarge = InstanceType' "r5.12xlarge"

pattern InstanceTypeR5_16xlarge :: InstanceType
pattern InstanceTypeR5_16xlarge = InstanceType' "r5.16xlarge"

pattern InstanceTypeR5_24xlarge :: InstanceType
pattern InstanceTypeR5_24xlarge = InstanceType' "r5.24xlarge"

pattern InstanceTypeR5_Metal :: InstanceType
pattern InstanceTypeR5_Metal = InstanceType' "r5.metal"

pattern InstanceTypeR5a_Large :: InstanceType
pattern InstanceTypeR5a_Large = InstanceType' "r5a.large"

pattern InstanceTypeR5a_Xlarge :: InstanceType
pattern InstanceTypeR5a_Xlarge = InstanceType' "r5a.xlarge"

pattern InstanceTypeR5a_2xlarge :: InstanceType
pattern InstanceTypeR5a_2xlarge = InstanceType' "r5a.2xlarge"

pattern InstanceTypeR5a_4xlarge :: InstanceType
pattern InstanceTypeR5a_4xlarge = InstanceType' "r5a.4xlarge"

pattern InstanceTypeR5a_8xlarge :: InstanceType
pattern InstanceTypeR5a_8xlarge = InstanceType' "r5a.8xlarge"

pattern InstanceTypeR5a_12xlarge :: InstanceType
pattern InstanceTypeR5a_12xlarge = InstanceType' "r5a.12xlarge"

pattern InstanceTypeR5a_16xlarge :: InstanceType
pattern InstanceTypeR5a_16xlarge = InstanceType' "r5a.16xlarge"

pattern InstanceTypeR5a_24xlarge :: InstanceType
pattern InstanceTypeR5a_24xlarge = InstanceType' "r5a.24xlarge"

pattern InstanceTypeR5d_Large :: InstanceType
pattern InstanceTypeR5d_Large = InstanceType' "r5d.large"

pattern InstanceTypeR5d_Xlarge :: InstanceType
pattern InstanceTypeR5d_Xlarge = InstanceType' "r5d.xlarge"

pattern InstanceTypeR5d_2xlarge :: InstanceType
pattern InstanceTypeR5d_2xlarge = InstanceType' "r5d.2xlarge"

pattern InstanceTypeR5d_4xlarge :: InstanceType
pattern InstanceTypeR5d_4xlarge = InstanceType' "r5d.4xlarge"

pattern InstanceTypeR5d_8xlarge :: InstanceType
pattern InstanceTypeR5d_8xlarge = InstanceType' "r5d.8xlarge"

pattern InstanceTypeR5d_12xlarge :: InstanceType
pattern InstanceTypeR5d_12xlarge = InstanceType' "r5d.12xlarge"

pattern InstanceTypeR5d_16xlarge :: InstanceType
pattern InstanceTypeR5d_16xlarge = InstanceType' "r5d.16xlarge"

pattern InstanceTypeR5d_24xlarge :: InstanceType
pattern InstanceTypeR5d_24xlarge = InstanceType' "r5d.24xlarge"

pattern InstanceTypeR5d_Metal :: InstanceType
pattern InstanceTypeR5d_Metal = InstanceType' "r5d.metal"

pattern InstanceTypeR5ad_Large :: InstanceType
pattern InstanceTypeR5ad_Large = InstanceType' "r5ad.large"

pattern InstanceTypeR5ad_Xlarge :: InstanceType
pattern InstanceTypeR5ad_Xlarge = InstanceType' "r5ad.xlarge"

pattern InstanceTypeR5ad_2xlarge :: InstanceType
pattern InstanceTypeR5ad_2xlarge = InstanceType' "r5ad.2xlarge"

pattern InstanceTypeR5ad_4xlarge :: InstanceType
pattern InstanceTypeR5ad_4xlarge = InstanceType' "r5ad.4xlarge"

pattern InstanceTypeR5ad_8xlarge :: InstanceType
pattern InstanceTypeR5ad_8xlarge = InstanceType' "r5ad.8xlarge"

pattern InstanceTypeR5ad_12xlarge :: InstanceType
pattern InstanceTypeR5ad_12xlarge = InstanceType' "r5ad.12xlarge"

pattern InstanceTypeR5ad_16xlarge :: InstanceType
pattern InstanceTypeR5ad_16xlarge = InstanceType' "r5ad.16xlarge"

pattern InstanceTypeR5ad_24xlarge :: InstanceType
pattern InstanceTypeR5ad_24xlarge = InstanceType' "r5ad.24xlarge"

pattern InstanceTypeR6g_Metal :: InstanceType
pattern InstanceTypeR6g_Metal = InstanceType' "r6g.metal"

pattern InstanceTypeR6g_Medium :: InstanceType
pattern InstanceTypeR6g_Medium = InstanceType' "r6g.medium"

pattern InstanceTypeR6g_Large :: InstanceType
pattern InstanceTypeR6g_Large = InstanceType' "r6g.large"

pattern InstanceTypeR6g_Xlarge :: InstanceType
pattern InstanceTypeR6g_Xlarge = InstanceType' "r6g.xlarge"

pattern InstanceTypeR6g_2xlarge :: InstanceType
pattern InstanceTypeR6g_2xlarge = InstanceType' "r6g.2xlarge"

pattern InstanceTypeR6g_4xlarge :: InstanceType
pattern InstanceTypeR6g_4xlarge = InstanceType' "r6g.4xlarge"

pattern InstanceTypeR6g_8xlarge :: InstanceType
pattern InstanceTypeR6g_8xlarge = InstanceType' "r6g.8xlarge"

pattern InstanceTypeR6g_12xlarge :: InstanceType
pattern InstanceTypeR6g_12xlarge = InstanceType' "r6g.12xlarge"

pattern InstanceTypeR6g_16xlarge :: InstanceType
pattern InstanceTypeR6g_16xlarge = InstanceType' "r6g.16xlarge"

pattern InstanceTypeR6gd_Metal :: InstanceType
pattern InstanceTypeR6gd_Metal = InstanceType' "r6gd.metal"

pattern InstanceTypeR6gd_Medium :: InstanceType
pattern InstanceTypeR6gd_Medium = InstanceType' "r6gd.medium"

pattern InstanceTypeR6gd_Large :: InstanceType
pattern InstanceTypeR6gd_Large = InstanceType' "r6gd.large"

pattern InstanceTypeR6gd_Xlarge :: InstanceType
pattern InstanceTypeR6gd_Xlarge = InstanceType' "r6gd.xlarge"

pattern InstanceTypeR6gd_2xlarge :: InstanceType
pattern InstanceTypeR6gd_2xlarge = InstanceType' "r6gd.2xlarge"

pattern InstanceTypeR6gd_4xlarge :: InstanceType
pattern InstanceTypeR6gd_4xlarge = InstanceType' "r6gd.4xlarge"

pattern InstanceTypeR6gd_8xlarge :: InstanceType
pattern InstanceTypeR6gd_8xlarge = InstanceType' "r6gd.8xlarge"

pattern InstanceTypeR6gd_12xlarge :: InstanceType
pattern InstanceTypeR6gd_12xlarge = InstanceType' "r6gd.12xlarge"

pattern InstanceTypeR6gd_16xlarge :: InstanceType
pattern InstanceTypeR6gd_16xlarge = InstanceType' "r6gd.16xlarge"

pattern InstanceTypeX1_16xlarge :: InstanceType
pattern InstanceTypeX1_16xlarge = InstanceType' "x1.16xlarge"

pattern InstanceTypeX1_32xlarge :: InstanceType
pattern InstanceTypeX1_32xlarge = InstanceType' "x1.32xlarge"

pattern InstanceTypeX1e_Xlarge :: InstanceType
pattern InstanceTypeX1e_Xlarge = InstanceType' "x1e.xlarge"

pattern InstanceTypeX1e_2xlarge :: InstanceType
pattern InstanceTypeX1e_2xlarge = InstanceType' "x1e.2xlarge"

pattern InstanceTypeX1e_4xlarge :: InstanceType
pattern InstanceTypeX1e_4xlarge = InstanceType' "x1e.4xlarge"

pattern InstanceTypeX1e_8xlarge :: InstanceType
pattern InstanceTypeX1e_8xlarge = InstanceType' "x1e.8xlarge"

pattern InstanceTypeX1e_16xlarge :: InstanceType
pattern InstanceTypeX1e_16xlarge = InstanceType' "x1e.16xlarge"

pattern InstanceTypeX1e_32xlarge :: InstanceType
pattern InstanceTypeX1e_32xlarge = InstanceType' "x1e.32xlarge"

pattern InstanceTypeI2_Xlarge :: InstanceType
pattern InstanceTypeI2_Xlarge = InstanceType' "i2.xlarge"

pattern InstanceTypeI2_2xlarge :: InstanceType
pattern InstanceTypeI2_2xlarge = InstanceType' "i2.2xlarge"

pattern InstanceTypeI2_4xlarge :: InstanceType
pattern InstanceTypeI2_4xlarge = InstanceType' "i2.4xlarge"

pattern InstanceTypeI2_8xlarge :: InstanceType
pattern InstanceTypeI2_8xlarge = InstanceType' "i2.8xlarge"

pattern InstanceTypeI3_Large :: InstanceType
pattern InstanceTypeI3_Large = InstanceType' "i3.large"

pattern InstanceTypeI3_Xlarge :: InstanceType
pattern InstanceTypeI3_Xlarge = InstanceType' "i3.xlarge"

pattern InstanceTypeI3_2xlarge :: InstanceType
pattern InstanceTypeI3_2xlarge = InstanceType' "i3.2xlarge"

pattern InstanceTypeI3_4xlarge :: InstanceType
pattern InstanceTypeI3_4xlarge = InstanceType' "i3.4xlarge"

pattern InstanceTypeI3_8xlarge :: InstanceType
pattern InstanceTypeI3_8xlarge = InstanceType' "i3.8xlarge"

pattern InstanceTypeI3_16xlarge :: InstanceType
pattern InstanceTypeI3_16xlarge = InstanceType' "i3.16xlarge"

pattern InstanceTypeI3_Metal :: InstanceType
pattern InstanceTypeI3_Metal = InstanceType' "i3.metal"

pattern InstanceTypeI3en_Large :: InstanceType
pattern InstanceTypeI3en_Large = InstanceType' "i3en.large"

pattern InstanceTypeI3en_Xlarge :: InstanceType
pattern InstanceTypeI3en_Xlarge = InstanceType' "i3en.xlarge"

pattern InstanceTypeI3en_2xlarge :: InstanceType
pattern InstanceTypeI3en_2xlarge = InstanceType' "i3en.2xlarge"

pattern InstanceTypeI3en_3xlarge :: InstanceType
pattern InstanceTypeI3en_3xlarge = InstanceType' "i3en.3xlarge"

pattern InstanceTypeI3en_6xlarge :: InstanceType
pattern InstanceTypeI3en_6xlarge = InstanceType' "i3en.6xlarge"

pattern InstanceTypeI3en_12xlarge :: InstanceType
pattern InstanceTypeI3en_12xlarge = InstanceType' "i3en.12xlarge"

pattern InstanceTypeI3en_24xlarge :: InstanceType
pattern InstanceTypeI3en_24xlarge = InstanceType' "i3en.24xlarge"

pattern InstanceTypeI3en_Metal :: InstanceType
pattern InstanceTypeI3en_Metal = InstanceType' "i3en.metal"

pattern InstanceTypeHI1_4xlarge :: InstanceType
pattern InstanceTypeHI1_4xlarge = InstanceType' "hi1.4xlarge"

pattern InstanceTypeHS1_8xlarge :: InstanceType
pattern InstanceTypeHS1_8xlarge = InstanceType' "hs1.8xlarge"

pattern InstanceTypeC1_Medium :: InstanceType
pattern InstanceTypeC1_Medium = InstanceType' "c1.medium"

pattern InstanceTypeC1_Xlarge :: InstanceType
pattern InstanceTypeC1_Xlarge = InstanceType' "c1.xlarge"

pattern InstanceTypeC3_Large :: InstanceType
pattern InstanceTypeC3_Large = InstanceType' "c3.large"

pattern InstanceTypeC3_Xlarge :: InstanceType
pattern InstanceTypeC3_Xlarge = InstanceType' "c3.xlarge"

pattern InstanceTypeC3_2xlarge :: InstanceType
pattern InstanceTypeC3_2xlarge = InstanceType' "c3.2xlarge"

pattern InstanceTypeC3_4xlarge :: InstanceType
pattern InstanceTypeC3_4xlarge = InstanceType' "c3.4xlarge"

pattern InstanceTypeC3_8xlarge :: InstanceType
pattern InstanceTypeC3_8xlarge = InstanceType' "c3.8xlarge"

pattern InstanceTypeC4_Large :: InstanceType
pattern InstanceTypeC4_Large = InstanceType' "c4.large"

pattern InstanceTypeC4_Xlarge :: InstanceType
pattern InstanceTypeC4_Xlarge = InstanceType' "c4.xlarge"

pattern InstanceTypeC4_2xlarge :: InstanceType
pattern InstanceTypeC4_2xlarge = InstanceType' "c4.2xlarge"

pattern InstanceTypeC4_4xlarge :: InstanceType
pattern InstanceTypeC4_4xlarge = InstanceType' "c4.4xlarge"

pattern InstanceTypeC4_8xlarge :: InstanceType
pattern InstanceTypeC4_8xlarge = InstanceType' "c4.8xlarge"

pattern InstanceTypeC5_Large :: InstanceType
pattern InstanceTypeC5_Large = InstanceType' "c5.large"

pattern InstanceTypeC5_Xlarge :: InstanceType
pattern InstanceTypeC5_Xlarge = InstanceType' "c5.xlarge"

pattern InstanceTypeC5_2xlarge :: InstanceType
pattern InstanceTypeC5_2xlarge = InstanceType' "c5.2xlarge"

pattern InstanceTypeC5_4xlarge :: InstanceType
pattern InstanceTypeC5_4xlarge = InstanceType' "c5.4xlarge"

pattern InstanceTypeC5_9xlarge :: InstanceType
pattern InstanceTypeC5_9xlarge = InstanceType' "c5.9xlarge"

pattern InstanceTypeC5_12xlarge :: InstanceType
pattern InstanceTypeC5_12xlarge = InstanceType' "c5.12xlarge"

pattern InstanceTypeC5_18xlarge :: InstanceType
pattern InstanceTypeC5_18xlarge = InstanceType' "c5.18xlarge"

pattern InstanceTypeC5_24xlarge :: InstanceType
pattern InstanceTypeC5_24xlarge = InstanceType' "c5.24xlarge"

pattern InstanceTypeC5_Metal :: InstanceType
pattern InstanceTypeC5_Metal = InstanceType' "c5.metal"

pattern InstanceTypeC5a_Large :: InstanceType
pattern InstanceTypeC5a_Large = InstanceType' "c5a.large"

pattern InstanceTypeC5a_Xlarge :: InstanceType
pattern InstanceTypeC5a_Xlarge = InstanceType' "c5a.xlarge"

pattern InstanceTypeC5a_2xlarge :: InstanceType
pattern InstanceTypeC5a_2xlarge = InstanceType' "c5a.2xlarge"

pattern InstanceTypeC5a_4xlarge :: InstanceType
pattern InstanceTypeC5a_4xlarge = InstanceType' "c5a.4xlarge"

pattern InstanceTypeC5a_8xlarge :: InstanceType
pattern InstanceTypeC5a_8xlarge = InstanceType' "c5a.8xlarge"

pattern InstanceTypeC5a_12xlarge :: InstanceType
pattern InstanceTypeC5a_12xlarge = InstanceType' "c5a.12xlarge"

pattern InstanceTypeC5a_16xlarge :: InstanceType
pattern InstanceTypeC5a_16xlarge = InstanceType' "c5a.16xlarge"

pattern InstanceTypeC5a_24xlarge :: InstanceType
pattern InstanceTypeC5a_24xlarge = InstanceType' "c5a.24xlarge"

pattern InstanceTypeC5ad_Large :: InstanceType
pattern InstanceTypeC5ad_Large = InstanceType' "c5ad.large"

pattern InstanceTypeC5ad_Xlarge :: InstanceType
pattern InstanceTypeC5ad_Xlarge = InstanceType' "c5ad.xlarge"

pattern InstanceTypeC5ad_2xlarge :: InstanceType
pattern InstanceTypeC5ad_2xlarge = InstanceType' "c5ad.2xlarge"

pattern InstanceTypeC5ad_4xlarge :: InstanceType
pattern InstanceTypeC5ad_4xlarge = InstanceType' "c5ad.4xlarge"

pattern InstanceTypeC5ad_8xlarge :: InstanceType
pattern InstanceTypeC5ad_8xlarge = InstanceType' "c5ad.8xlarge"

pattern InstanceTypeC5ad_12xlarge :: InstanceType
pattern InstanceTypeC5ad_12xlarge = InstanceType' "c5ad.12xlarge"

pattern InstanceTypeC5ad_16xlarge :: InstanceType
pattern InstanceTypeC5ad_16xlarge = InstanceType' "c5ad.16xlarge"

pattern InstanceTypeC5ad_24xlarge :: InstanceType
pattern InstanceTypeC5ad_24xlarge = InstanceType' "c5ad.24xlarge"

pattern InstanceTypeC5d_Large :: InstanceType
pattern InstanceTypeC5d_Large = InstanceType' "c5d.large"

pattern InstanceTypeC5d_Xlarge :: InstanceType
pattern InstanceTypeC5d_Xlarge = InstanceType' "c5d.xlarge"

pattern InstanceTypeC5d_2xlarge :: InstanceType
pattern InstanceTypeC5d_2xlarge = InstanceType' "c5d.2xlarge"

pattern InstanceTypeC5d_4xlarge :: InstanceType
pattern InstanceTypeC5d_4xlarge = InstanceType' "c5d.4xlarge"

pattern InstanceTypeC5d_9xlarge :: InstanceType
pattern InstanceTypeC5d_9xlarge = InstanceType' "c5d.9xlarge"

pattern InstanceTypeC5d_12xlarge :: InstanceType
pattern InstanceTypeC5d_12xlarge = InstanceType' "c5d.12xlarge"

pattern InstanceTypeC5d_18xlarge :: InstanceType
pattern InstanceTypeC5d_18xlarge = InstanceType' "c5d.18xlarge"

pattern InstanceTypeC5d_24xlarge :: InstanceType
pattern InstanceTypeC5d_24xlarge = InstanceType' "c5d.24xlarge"

pattern InstanceTypeC5d_Metal :: InstanceType
pattern InstanceTypeC5d_Metal = InstanceType' "c5d.metal"

pattern InstanceTypeC5n_Large :: InstanceType
pattern InstanceTypeC5n_Large = InstanceType' "c5n.large"

pattern InstanceTypeC5n_Xlarge :: InstanceType
pattern InstanceTypeC5n_Xlarge = InstanceType' "c5n.xlarge"

pattern InstanceTypeC5n_2xlarge :: InstanceType
pattern InstanceTypeC5n_2xlarge = InstanceType' "c5n.2xlarge"

pattern InstanceTypeC5n_4xlarge :: InstanceType
pattern InstanceTypeC5n_4xlarge = InstanceType' "c5n.4xlarge"

pattern InstanceTypeC5n_9xlarge :: InstanceType
pattern InstanceTypeC5n_9xlarge = InstanceType' "c5n.9xlarge"

pattern InstanceTypeC5n_18xlarge :: InstanceType
pattern InstanceTypeC5n_18xlarge = InstanceType' "c5n.18xlarge"

pattern InstanceTypeC6g_Metal :: InstanceType
pattern InstanceTypeC6g_Metal = InstanceType' "c6g.metal"

pattern InstanceTypeC6g_Medium :: InstanceType
pattern InstanceTypeC6g_Medium = InstanceType' "c6g.medium"

pattern InstanceTypeC6g_Large :: InstanceType
pattern InstanceTypeC6g_Large = InstanceType' "c6g.large"

pattern InstanceTypeC6g_Xlarge :: InstanceType
pattern InstanceTypeC6g_Xlarge = InstanceType' "c6g.xlarge"

pattern InstanceTypeC6g_2xlarge :: InstanceType
pattern InstanceTypeC6g_2xlarge = InstanceType' "c6g.2xlarge"

pattern InstanceTypeC6g_4xlarge :: InstanceType
pattern InstanceTypeC6g_4xlarge = InstanceType' "c6g.4xlarge"

pattern InstanceTypeC6g_8xlarge :: InstanceType
pattern InstanceTypeC6g_8xlarge = InstanceType' "c6g.8xlarge"

pattern InstanceTypeC6g_12xlarge :: InstanceType
pattern InstanceTypeC6g_12xlarge = InstanceType' "c6g.12xlarge"

pattern InstanceTypeC6g_16xlarge :: InstanceType
pattern InstanceTypeC6g_16xlarge = InstanceType' "c6g.16xlarge"

pattern InstanceTypeC6gd_Metal :: InstanceType
pattern InstanceTypeC6gd_Metal = InstanceType' "c6gd.metal"

pattern InstanceTypeC6gd_Medium :: InstanceType
pattern InstanceTypeC6gd_Medium = InstanceType' "c6gd.medium"

pattern InstanceTypeC6gd_Large :: InstanceType
pattern InstanceTypeC6gd_Large = InstanceType' "c6gd.large"

pattern InstanceTypeC6gd_Xlarge :: InstanceType
pattern InstanceTypeC6gd_Xlarge = InstanceType' "c6gd.xlarge"

pattern InstanceTypeC6gd_2xlarge :: InstanceType
pattern InstanceTypeC6gd_2xlarge = InstanceType' "c6gd.2xlarge"

pattern InstanceTypeC6gd_4xlarge :: InstanceType
pattern InstanceTypeC6gd_4xlarge = InstanceType' "c6gd.4xlarge"

pattern InstanceTypeC6gd_8xlarge :: InstanceType
pattern InstanceTypeC6gd_8xlarge = InstanceType' "c6gd.8xlarge"

pattern InstanceTypeC6gd_12xlarge :: InstanceType
pattern InstanceTypeC6gd_12xlarge = InstanceType' "c6gd.12xlarge"

pattern InstanceTypeC6gd_16xlarge :: InstanceType
pattern InstanceTypeC6gd_16xlarge = InstanceType' "c6gd.16xlarge"

pattern InstanceTypeCC1_4xlarge :: InstanceType
pattern InstanceTypeCC1_4xlarge = InstanceType' "cc1.4xlarge"

pattern InstanceTypeCC2_8xlarge :: InstanceType
pattern InstanceTypeCC2_8xlarge = InstanceType' "cc2.8xlarge"

pattern InstanceTypeG2_2xlarge :: InstanceType
pattern InstanceTypeG2_2xlarge = InstanceType' "g2.2xlarge"

pattern InstanceTypeG2_8xlarge :: InstanceType
pattern InstanceTypeG2_8xlarge = InstanceType' "g2.8xlarge"

pattern InstanceTypeG3_4xlarge :: InstanceType
pattern InstanceTypeG3_4xlarge = InstanceType' "g3.4xlarge"

pattern InstanceTypeG3_8xlarge :: InstanceType
pattern InstanceTypeG3_8xlarge = InstanceType' "g3.8xlarge"

pattern InstanceTypeG3_16xlarge :: InstanceType
pattern InstanceTypeG3_16xlarge = InstanceType' "g3.16xlarge"

pattern InstanceTypeG3s_Xlarge :: InstanceType
pattern InstanceTypeG3s_Xlarge = InstanceType' "g3s.xlarge"

pattern InstanceTypeG4dn_Xlarge :: InstanceType
pattern InstanceTypeG4dn_Xlarge = InstanceType' "g4dn.xlarge"

pattern InstanceTypeG4dn_2xlarge :: InstanceType
pattern InstanceTypeG4dn_2xlarge = InstanceType' "g4dn.2xlarge"

pattern InstanceTypeG4dn_4xlarge :: InstanceType
pattern InstanceTypeG4dn_4xlarge = InstanceType' "g4dn.4xlarge"

pattern InstanceTypeG4dn_8xlarge :: InstanceType
pattern InstanceTypeG4dn_8xlarge = InstanceType' "g4dn.8xlarge"

pattern InstanceTypeG4dn_12xlarge :: InstanceType
pattern InstanceTypeG4dn_12xlarge = InstanceType' "g4dn.12xlarge"

pattern InstanceTypeG4dn_16xlarge :: InstanceType
pattern InstanceTypeG4dn_16xlarge = InstanceType' "g4dn.16xlarge"

pattern InstanceTypeG4dn_Metal :: InstanceType
pattern InstanceTypeG4dn_Metal = InstanceType' "g4dn.metal"

pattern InstanceTypeCG1_4xlarge :: InstanceType
pattern InstanceTypeCG1_4xlarge = InstanceType' "cg1.4xlarge"

pattern InstanceTypeP2_Xlarge :: InstanceType
pattern InstanceTypeP2_Xlarge = InstanceType' "p2.xlarge"

pattern InstanceTypeP2_8xlarge :: InstanceType
pattern InstanceTypeP2_8xlarge = InstanceType' "p2.8xlarge"

pattern InstanceTypeP2_16xlarge :: InstanceType
pattern InstanceTypeP2_16xlarge = InstanceType' "p2.16xlarge"

pattern InstanceTypeP3_2xlarge :: InstanceType
pattern InstanceTypeP3_2xlarge = InstanceType' "p3.2xlarge"

pattern InstanceTypeP3_8xlarge :: InstanceType
pattern InstanceTypeP3_8xlarge = InstanceType' "p3.8xlarge"

pattern InstanceTypeP3_16xlarge :: InstanceType
pattern InstanceTypeP3_16xlarge = InstanceType' "p3.16xlarge"

pattern InstanceTypeP3dn_24xlarge :: InstanceType
pattern InstanceTypeP3dn_24xlarge = InstanceType' "p3dn.24xlarge"

pattern InstanceTypeP4d_24xlarge :: InstanceType
pattern InstanceTypeP4d_24xlarge = InstanceType' "p4d.24xlarge"

pattern InstanceTypeD2_Xlarge :: InstanceType
pattern InstanceTypeD2_Xlarge = InstanceType' "d2.xlarge"

pattern InstanceTypeD2_2xlarge :: InstanceType
pattern InstanceTypeD2_2xlarge = InstanceType' "d2.2xlarge"

pattern InstanceTypeD2_4xlarge :: InstanceType
pattern InstanceTypeD2_4xlarge = InstanceType' "d2.4xlarge"

pattern InstanceTypeD2_8xlarge :: InstanceType
pattern InstanceTypeD2_8xlarge = InstanceType' "d2.8xlarge"

pattern InstanceTypeF1_2xlarge :: InstanceType
pattern InstanceTypeF1_2xlarge = InstanceType' "f1.2xlarge"

pattern InstanceTypeF1_4xlarge :: InstanceType
pattern InstanceTypeF1_4xlarge = InstanceType' "f1.4xlarge"

pattern InstanceTypeF1_16xlarge :: InstanceType
pattern InstanceTypeF1_16xlarge = InstanceType' "f1.16xlarge"

pattern InstanceTypeM5_Large :: InstanceType
pattern InstanceTypeM5_Large = InstanceType' "m5.large"

pattern InstanceTypeM5_Xlarge :: InstanceType
pattern InstanceTypeM5_Xlarge = InstanceType' "m5.xlarge"

pattern InstanceTypeM5_2xlarge :: InstanceType
pattern InstanceTypeM5_2xlarge = InstanceType' "m5.2xlarge"

pattern InstanceTypeM5_4xlarge :: InstanceType
pattern InstanceTypeM5_4xlarge = InstanceType' "m5.4xlarge"

pattern InstanceTypeM5_8xlarge :: InstanceType
pattern InstanceTypeM5_8xlarge = InstanceType' "m5.8xlarge"

pattern InstanceTypeM5_12xlarge :: InstanceType
pattern InstanceTypeM5_12xlarge = InstanceType' "m5.12xlarge"

pattern InstanceTypeM5_16xlarge :: InstanceType
pattern InstanceTypeM5_16xlarge = InstanceType' "m5.16xlarge"

pattern InstanceTypeM5_24xlarge :: InstanceType
pattern InstanceTypeM5_24xlarge = InstanceType' "m5.24xlarge"

pattern InstanceTypeM5_Metal :: InstanceType
pattern InstanceTypeM5_Metal = InstanceType' "m5.metal"

pattern InstanceTypeM5a_Large :: InstanceType
pattern InstanceTypeM5a_Large = InstanceType' "m5a.large"

pattern InstanceTypeM5a_Xlarge :: InstanceType
pattern InstanceTypeM5a_Xlarge = InstanceType' "m5a.xlarge"

pattern InstanceTypeM5a_2xlarge :: InstanceType
pattern InstanceTypeM5a_2xlarge = InstanceType' "m5a.2xlarge"

pattern InstanceTypeM5a_4xlarge :: InstanceType
pattern InstanceTypeM5a_4xlarge = InstanceType' "m5a.4xlarge"

pattern InstanceTypeM5a_8xlarge :: InstanceType
pattern InstanceTypeM5a_8xlarge = InstanceType' "m5a.8xlarge"

pattern InstanceTypeM5a_12xlarge :: InstanceType
pattern InstanceTypeM5a_12xlarge = InstanceType' "m5a.12xlarge"

pattern InstanceTypeM5a_16xlarge :: InstanceType
pattern InstanceTypeM5a_16xlarge = InstanceType' "m5a.16xlarge"

pattern InstanceTypeM5a_24xlarge :: InstanceType
pattern InstanceTypeM5a_24xlarge = InstanceType' "m5a.24xlarge"

pattern InstanceTypeM5d_Large :: InstanceType
pattern InstanceTypeM5d_Large = InstanceType' "m5d.large"

pattern InstanceTypeM5d_Xlarge :: InstanceType
pattern InstanceTypeM5d_Xlarge = InstanceType' "m5d.xlarge"

pattern InstanceTypeM5d_2xlarge :: InstanceType
pattern InstanceTypeM5d_2xlarge = InstanceType' "m5d.2xlarge"

pattern InstanceTypeM5d_4xlarge :: InstanceType
pattern InstanceTypeM5d_4xlarge = InstanceType' "m5d.4xlarge"

pattern InstanceTypeM5d_8xlarge :: InstanceType
pattern InstanceTypeM5d_8xlarge = InstanceType' "m5d.8xlarge"

pattern InstanceTypeM5d_12xlarge :: InstanceType
pattern InstanceTypeM5d_12xlarge = InstanceType' "m5d.12xlarge"

pattern InstanceTypeM5d_16xlarge :: InstanceType
pattern InstanceTypeM5d_16xlarge = InstanceType' "m5d.16xlarge"

pattern InstanceTypeM5d_24xlarge :: InstanceType
pattern InstanceTypeM5d_24xlarge = InstanceType' "m5d.24xlarge"

pattern InstanceTypeM5d_Metal :: InstanceType
pattern InstanceTypeM5d_Metal = InstanceType' "m5d.metal"

pattern InstanceTypeM5ad_Large :: InstanceType
pattern InstanceTypeM5ad_Large = InstanceType' "m5ad.large"

pattern InstanceTypeM5ad_Xlarge :: InstanceType
pattern InstanceTypeM5ad_Xlarge = InstanceType' "m5ad.xlarge"

pattern InstanceTypeM5ad_2xlarge :: InstanceType
pattern InstanceTypeM5ad_2xlarge = InstanceType' "m5ad.2xlarge"

pattern InstanceTypeM5ad_4xlarge :: InstanceType
pattern InstanceTypeM5ad_4xlarge = InstanceType' "m5ad.4xlarge"

pattern InstanceTypeM5ad_8xlarge :: InstanceType
pattern InstanceTypeM5ad_8xlarge = InstanceType' "m5ad.8xlarge"

pattern InstanceTypeM5ad_12xlarge :: InstanceType
pattern InstanceTypeM5ad_12xlarge = InstanceType' "m5ad.12xlarge"

pattern InstanceTypeM5ad_16xlarge :: InstanceType
pattern InstanceTypeM5ad_16xlarge = InstanceType' "m5ad.16xlarge"

pattern InstanceTypeM5ad_24xlarge :: InstanceType
pattern InstanceTypeM5ad_24xlarge = InstanceType' "m5ad.24xlarge"

pattern InstanceTypeH1_2xlarge :: InstanceType
pattern InstanceTypeH1_2xlarge = InstanceType' "h1.2xlarge"

pattern InstanceTypeH1_4xlarge :: InstanceType
pattern InstanceTypeH1_4xlarge = InstanceType' "h1.4xlarge"

pattern InstanceTypeH1_8xlarge :: InstanceType
pattern InstanceTypeH1_8xlarge = InstanceType' "h1.8xlarge"

pattern InstanceTypeH1_16xlarge :: InstanceType
pattern InstanceTypeH1_16xlarge = InstanceType' "h1.16xlarge"

pattern InstanceTypeZ1d_Large :: InstanceType
pattern InstanceTypeZ1d_Large = InstanceType' "z1d.large"

pattern InstanceTypeZ1d_Xlarge :: InstanceType
pattern InstanceTypeZ1d_Xlarge = InstanceType' "z1d.xlarge"

pattern InstanceTypeZ1d_2xlarge :: InstanceType
pattern InstanceTypeZ1d_2xlarge = InstanceType' "z1d.2xlarge"

pattern InstanceTypeZ1d_3xlarge :: InstanceType
pattern InstanceTypeZ1d_3xlarge = InstanceType' "z1d.3xlarge"

pattern InstanceTypeZ1d_6xlarge :: InstanceType
pattern InstanceTypeZ1d_6xlarge = InstanceType' "z1d.6xlarge"

pattern InstanceTypeZ1d_12xlarge :: InstanceType
pattern InstanceTypeZ1d_12xlarge = InstanceType' "z1d.12xlarge"

pattern InstanceTypeZ1d_Metal :: InstanceType
pattern InstanceTypeZ1d_Metal = InstanceType' "z1d.metal"

pattern InstanceTypeU6TB1_Metal :: InstanceType
pattern InstanceTypeU6TB1_Metal = InstanceType' "u-6tb1.metal"

pattern InstanceTypeU9TB1_Metal :: InstanceType
pattern InstanceTypeU9TB1_Metal = InstanceType' "u-9tb1.metal"

pattern InstanceTypeU12TB1_Metal :: InstanceType
pattern InstanceTypeU12TB1_Metal = InstanceType' "u-12tb1.metal"

pattern InstanceTypeU18TB1_Metal :: InstanceType
pattern InstanceTypeU18TB1_Metal = InstanceType' "u-18tb1.metal"

pattern InstanceTypeU24TB1_Metal :: InstanceType
pattern InstanceTypeU24TB1_Metal = InstanceType' "u-24tb1.metal"

pattern InstanceTypeA1_Medium :: InstanceType
pattern InstanceTypeA1_Medium = InstanceType' "a1.medium"

pattern InstanceTypeA1_Large :: InstanceType
pattern InstanceTypeA1_Large = InstanceType' "a1.large"

pattern InstanceTypeA1_Xlarge :: InstanceType
pattern InstanceTypeA1_Xlarge = InstanceType' "a1.xlarge"

pattern InstanceTypeA1_2xlarge :: InstanceType
pattern InstanceTypeA1_2xlarge = InstanceType' "a1.2xlarge"

pattern InstanceTypeA1_4xlarge :: InstanceType
pattern InstanceTypeA1_4xlarge = InstanceType' "a1.4xlarge"

pattern InstanceTypeA1_Metal :: InstanceType
pattern InstanceTypeA1_Metal = InstanceType' "a1.metal"

pattern InstanceTypeM5dn_Large :: InstanceType
pattern InstanceTypeM5dn_Large = InstanceType' "m5dn.large"

pattern InstanceTypeM5dn_Xlarge :: InstanceType
pattern InstanceTypeM5dn_Xlarge = InstanceType' "m5dn.xlarge"

pattern InstanceTypeM5dn_2xlarge :: InstanceType
pattern InstanceTypeM5dn_2xlarge = InstanceType' "m5dn.2xlarge"

pattern InstanceTypeM5dn_4xlarge :: InstanceType
pattern InstanceTypeM5dn_4xlarge = InstanceType' "m5dn.4xlarge"

pattern InstanceTypeM5dn_8xlarge :: InstanceType
pattern InstanceTypeM5dn_8xlarge = InstanceType' "m5dn.8xlarge"

pattern InstanceTypeM5dn_12xlarge :: InstanceType
pattern InstanceTypeM5dn_12xlarge = InstanceType' "m5dn.12xlarge"

pattern InstanceTypeM5dn_16xlarge :: InstanceType
pattern InstanceTypeM5dn_16xlarge = InstanceType' "m5dn.16xlarge"

pattern InstanceTypeM5dn_24xlarge :: InstanceType
pattern InstanceTypeM5dn_24xlarge = InstanceType' "m5dn.24xlarge"

pattern InstanceTypeM5n_Large :: InstanceType
pattern InstanceTypeM5n_Large = InstanceType' "m5n.large"

pattern InstanceTypeM5n_Xlarge :: InstanceType
pattern InstanceTypeM5n_Xlarge = InstanceType' "m5n.xlarge"

pattern InstanceTypeM5n_2xlarge :: InstanceType
pattern InstanceTypeM5n_2xlarge = InstanceType' "m5n.2xlarge"

pattern InstanceTypeM5n_4xlarge :: InstanceType
pattern InstanceTypeM5n_4xlarge = InstanceType' "m5n.4xlarge"

pattern InstanceTypeM5n_8xlarge :: InstanceType
pattern InstanceTypeM5n_8xlarge = InstanceType' "m5n.8xlarge"

pattern InstanceTypeM5n_12xlarge :: InstanceType
pattern InstanceTypeM5n_12xlarge = InstanceType' "m5n.12xlarge"

pattern InstanceTypeM5n_16xlarge :: InstanceType
pattern InstanceTypeM5n_16xlarge = InstanceType' "m5n.16xlarge"

pattern InstanceTypeM5n_24xlarge :: InstanceType
pattern InstanceTypeM5n_24xlarge = InstanceType' "m5n.24xlarge"

pattern InstanceTypeR5dn_Large :: InstanceType
pattern InstanceTypeR5dn_Large = InstanceType' "r5dn.large"

pattern InstanceTypeR5dn_Xlarge :: InstanceType
pattern InstanceTypeR5dn_Xlarge = InstanceType' "r5dn.xlarge"

pattern InstanceTypeR5dn_2xlarge :: InstanceType
pattern InstanceTypeR5dn_2xlarge = InstanceType' "r5dn.2xlarge"

pattern InstanceTypeR5dn_4xlarge :: InstanceType
pattern InstanceTypeR5dn_4xlarge = InstanceType' "r5dn.4xlarge"

pattern InstanceTypeR5dn_8xlarge :: InstanceType
pattern InstanceTypeR5dn_8xlarge = InstanceType' "r5dn.8xlarge"

pattern InstanceTypeR5dn_12xlarge :: InstanceType
pattern InstanceTypeR5dn_12xlarge = InstanceType' "r5dn.12xlarge"

pattern InstanceTypeR5dn_16xlarge :: InstanceType
pattern InstanceTypeR5dn_16xlarge = InstanceType' "r5dn.16xlarge"

pattern InstanceTypeR5dn_24xlarge :: InstanceType
pattern InstanceTypeR5dn_24xlarge = InstanceType' "r5dn.24xlarge"

pattern InstanceTypeR5n_Large :: InstanceType
pattern InstanceTypeR5n_Large = InstanceType' "r5n.large"

pattern InstanceTypeR5n_Xlarge :: InstanceType
pattern InstanceTypeR5n_Xlarge = InstanceType' "r5n.xlarge"

pattern InstanceTypeR5n_2xlarge :: InstanceType
pattern InstanceTypeR5n_2xlarge = InstanceType' "r5n.2xlarge"

pattern InstanceTypeR5n_4xlarge :: InstanceType
pattern InstanceTypeR5n_4xlarge = InstanceType' "r5n.4xlarge"

pattern InstanceTypeR5n_8xlarge :: InstanceType
pattern InstanceTypeR5n_8xlarge = InstanceType' "r5n.8xlarge"

pattern InstanceTypeR5n_12xlarge :: InstanceType
pattern InstanceTypeR5n_12xlarge = InstanceType' "r5n.12xlarge"

pattern InstanceTypeR5n_16xlarge :: InstanceType
pattern InstanceTypeR5n_16xlarge = InstanceType' "r5n.16xlarge"

pattern InstanceTypeR5n_24xlarge :: InstanceType
pattern InstanceTypeR5n_24xlarge = InstanceType' "r5n.24xlarge"

pattern InstanceTypeINF1_Xlarge :: InstanceType
pattern InstanceTypeINF1_Xlarge = InstanceType' "inf1.xlarge"

pattern InstanceTypeINF1_2xlarge :: InstanceType
pattern InstanceTypeINF1_2xlarge = InstanceType' "inf1.2xlarge"

pattern InstanceTypeINF1_6xlarge :: InstanceType
pattern InstanceTypeINF1_6xlarge = InstanceType' "inf1.6xlarge"

pattern InstanceTypeINF1_24xlarge :: InstanceType
pattern InstanceTypeINF1_24xlarge = InstanceType' "inf1.24xlarge"

pattern InstanceTypeM6g_Metal :: InstanceType
pattern InstanceTypeM6g_Metal = InstanceType' "m6g.metal"

pattern InstanceTypeM6g_Medium :: InstanceType
pattern InstanceTypeM6g_Medium = InstanceType' "m6g.medium"

pattern InstanceTypeM6g_Large :: InstanceType
pattern InstanceTypeM6g_Large = InstanceType' "m6g.large"

pattern InstanceTypeM6g_Xlarge :: InstanceType
pattern InstanceTypeM6g_Xlarge = InstanceType' "m6g.xlarge"

pattern InstanceTypeM6g_2xlarge :: InstanceType
pattern InstanceTypeM6g_2xlarge = InstanceType' "m6g.2xlarge"

pattern InstanceTypeM6g_4xlarge :: InstanceType
pattern InstanceTypeM6g_4xlarge = InstanceType' "m6g.4xlarge"

pattern InstanceTypeM6g_8xlarge :: InstanceType
pattern InstanceTypeM6g_8xlarge = InstanceType' "m6g.8xlarge"

pattern InstanceTypeM6g_12xlarge :: InstanceType
pattern InstanceTypeM6g_12xlarge = InstanceType' "m6g.12xlarge"

pattern InstanceTypeM6g_16xlarge :: InstanceType
pattern InstanceTypeM6g_16xlarge = InstanceType' "m6g.16xlarge"

pattern InstanceTypeM6gd_Metal :: InstanceType
pattern InstanceTypeM6gd_Metal = InstanceType' "m6gd.metal"

pattern InstanceTypeM6gd_Medium :: InstanceType
pattern InstanceTypeM6gd_Medium = InstanceType' "m6gd.medium"

pattern InstanceTypeM6gd_Large :: InstanceType
pattern InstanceTypeM6gd_Large = InstanceType' "m6gd.large"

pattern InstanceTypeM6gd_Xlarge :: InstanceType
pattern InstanceTypeM6gd_Xlarge = InstanceType' "m6gd.xlarge"

pattern InstanceTypeM6gd_2xlarge :: InstanceType
pattern InstanceTypeM6gd_2xlarge = InstanceType' "m6gd.2xlarge"

pattern InstanceTypeM6gd_4xlarge :: InstanceType
pattern InstanceTypeM6gd_4xlarge = InstanceType' "m6gd.4xlarge"

pattern InstanceTypeM6gd_8xlarge :: InstanceType
pattern InstanceTypeM6gd_8xlarge = InstanceType' "m6gd.8xlarge"

pattern InstanceTypeM6gd_12xlarge :: InstanceType
pattern InstanceTypeM6gd_12xlarge = InstanceType' "m6gd.12xlarge"

pattern InstanceTypeM6gd_16xlarge :: InstanceType
pattern InstanceTypeM6gd_16xlarge = InstanceType' "m6gd.16xlarge"

{-# COMPLETE 
  InstanceTypeT1_Micro,

  InstanceTypeT2_Nano,

  InstanceTypeT2_Micro,

  InstanceTypeT2_Small,

  InstanceTypeT2_Medium,

  InstanceTypeT2_Large,

  InstanceTypeT2_Xlarge,

  InstanceTypeT2_2xlarge,

  InstanceTypeT3_Nano,

  InstanceTypeT3_Micro,

  InstanceTypeT3_Small,

  InstanceTypeT3_Medium,

  InstanceTypeT3_Large,

  InstanceTypeT3_Xlarge,

  InstanceTypeT3_2xlarge,

  InstanceTypeT3a_Nano,

  InstanceTypeT3a_Micro,

  InstanceTypeT3a_Small,

  InstanceTypeT3a_Medium,

  InstanceTypeT3a_Large,

  InstanceTypeT3a_Xlarge,

  InstanceTypeT3a_2xlarge,

  InstanceTypeT4g_Nano,

  InstanceTypeT4g_Micro,

  InstanceTypeT4g_Small,

  InstanceTypeT4g_Medium,

  InstanceTypeT4g_Large,

  InstanceTypeT4g_Xlarge,

  InstanceTypeT4g_2xlarge,

  InstanceTypeM1_Small,

  InstanceTypeM1_Medium,

  InstanceTypeM1_Large,

  InstanceTypeM1_Xlarge,

  InstanceTypeM3_Medium,

  InstanceTypeM3_Large,

  InstanceTypeM3_Xlarge,

  InstanceTypeM3_2xlarge,

  InstanceTypeM4_Large,

  InstanceTypeM4_Xlarge,

  InstanceTypeM4_2xlarge,

  InstanceTypeM4_4xlarge,

  InstanceTypeM4_10xlarge,

  InstanceTypeM4_16xlarge,

  InstanceTypeM2_Xlarge,

  InstanceTypeM2_2xlarge,

  InstanceTypeM2_4xlarge,

  InstanceTypeCR1_8xlarge,

  InstanceTypeR3_Large,

  InstanceTypeR3_Xlarge,

  InstanceTypeR3_2xlarge,

  InstanceTypeR3_4xlarge,

  InstanceTypeR3_8xlarge,

  InstanceTypeR4_Large,

  InstanceTypeR4_Xlarge,

  InstanceTypeR4_2xlarge,

  InstanceTypeR4_4xlarge,

  InstanceTypeR4_8xlarge,

  InstanceTypeR4_16xlarge,

  InstanceTypeR5_Large,

  InstanceTypeR5_Xlarge,

  InstanceTypeR5_2xlarge,

  InstanceTypeR5_4xlarge,

  InstanceTypeR5_8xlarge,

  InstanceTypeR5_12xlarge,

  InstanceTypeR5_16xlarge,

  InstanceTypeR5_24xlarge,

  InstanceTypeR5_Metal,

  InstanceTypeR5a_Large,

  InstanceTypeR5a_Xlarge,

  InstanceTypeR5a_2xlarge,

  InstanceTypeR5a_4xlarge,

  InstanceTypeR5a_8xlarge,

  InstanceTypeR5a_12xlarge,

  InstanceTypeR5a_16xlarge,

  InstanceTypeR5a_24xlarge,

  InstanceTypeR5d_Large,

  InstanceTypeR5d_Xlarge,

  InstanceTypeR5d_2xlarge,

  InstanceTypeR5d_4xlarge,

  InstanceTypeR5d_8xlarge,

  InstanceTypeR5d_12xlarge,

  InstanceTypeR5d_16xlarge,

  InstanceTypeR5d_24xlarge,

  InstanceTypeR5d_Metal,

  InstanceTypeR5ad_Large,

  InstanceTypeR5ad_Xlarge,

  InstanceTypeR5ad_2xlarge,

  InstanceTypeR5ad_4xlarge,

  InstanceTypeR5ad_8xlarge,

  InstanceTypeR5ad_12xlarge,

  InstanceTypeR5ad_16xlarge,

  InstanceTypeR5ad_24xlarge,

  InstanceTypeR6g_Metal,

  InstanceTypeR6g_Medium,

  InstanceTypeR6g_Large,

  InstanceTypeR6g_Xlarge,

  InstanceTypeR6g_2xlarge,

  InstanceTypeR6g_4xlarge,

  InstanceTypeR6g_8xlarge,

  InstanceTypeR6g_12xlarge,

  InstanceTypeR6g_16xlarge,

  InstanceTypeR6gd_Metal,

  InstanceTypeR6gd_Medium,

  InstanceTypeR6gd_Large,

  InstanceTypeR6gd_Xlarge,

  InstanceTypeR6gd_2xlarge,

  InstanceTypeR6gd_4xlarge,

  InstanceTypeR6gd_8xlarge,

  InstanceTypeR6gd_12xlarge,

  InstanceTypeR6gd_16xlarge,

  InstanceTypeX1_16xlarge,

  InstanceTypeX1_32xlarge,

  InstanceTypeX1e_Xlarge,

  InstanceTypeX1e_2xlarge,

  InstanceTypeX1e_4xlarge,

  InstanceTypeX1e_8xlarge,

  InstanceTypeX1e_16xlarge,

  InstanceTypeX1e_32xlarge,

  InstanceTypeI2_Xlarge,

  InstanceTypeI2_2xlarge,

  InstanceTypeI2_4xlarge,

  InstanceTypeI2_8xlarge,

  InstanceTypeI3_Large,

  InstanceTypeI3_Xlarge,

  InstanceTypeI3_2xlarge,

  InstanceTypeI3_4xlarge,

  InstanceTypeI3_8xlarge,

  InstanceTypeI3_16xlarge,

  InstanceTypeI3_Metal,

  InstanceTypeI3en_Large,

  InstanceTypeI3en_Xlarge,

  InstanceTypeI3en_2xlarge,

  InstanceTypeI3en_3xlarge,

  InstanceTypeI3en_6xlarge,

  InstanceTypeI3en_12xlarge,

  InstanceTypeI3en_24xlarge,

  InstanceTypeI3en_Metal,

  InstanceTypeHI1_4xlarge,

  InstanceTypeHS1_8xlarge,

  InstanceTypeC1_Medium,

  InstanceTypeC1_Xlarge,

  InstanceTypeC3_Large,

  InstanceTypeC3_Xlarge,

  InstanceTypeC3_2xlarge,

  InstanceTypeC3_4xlarge,

  InstanceTypeC3_8xlarge,

  InstanceTypeC4_Large,

  InstanceTypeC4_Xlarge,

  InstanceTypeC4_2xlarge,

  InstanceTypeC4_4xlarge,

  InstanceTypeC4_8xlarge,

  InstanceTypeC5_Large,

  InstanceTypeC5_Xlarge,

  InstanceTypeC5_2xlarge,

  InstanceTypeC5_4xlarge,

  InstanceTypeC5_9xlarge,

  InstanceTypeC5_12xlarge,

  InstanceTypeC5_18xlarge,

  InstanceTypeC5_24xlarge,

  InstanceTypeC5_Metal,

  InstanceTypeC5a_Large,

  InstanceTypeC5a_Xlarge,

  InstanceTypeC5a_2xlarge,

  InstanceTypeC5a_4xlarge,

  InstanceTypeC5a_8xlarge,

  InstanceTypeC5a_12xlarge,

  InstanceTypeC5a_16xlarge,

  InstanceTypeC5a_24xlarge,

  InstanceTypeC5ad_Large,

  InstanceTypeC5ad_Xlarge,

  InstanceTypeC5ad_2xlarge,

  InstanceTypeC5ad_4xlarge,

  InstanceTypeC5ad_8xlarge,

  InstanceTypeC5ad_12xlarge,

  InstanceTypeC5ad_16xlarge,

  InstanceTypeC5ad_24xlarge,

  InstanceTypeC5d_Large,

  InstanceTypeC5d_Xlarge,

  InstanceTypeC5d_2xlarge,

  InstanceTypeC5d_4xlarge,

  InstanceTypeC5d_9xlarge,

  InstanceTypeC5d_12xlarge,

  InstanceTypeC5d_18xlarge,

  InstanceTypeC5d_24xlarge,

  InstanceTypeC5d_Metal,

  InstanceTypeC5n_Large,

  InstanceTypeC5n_Xlarge,

  InstanceTypeC5n_2xlarge,

  InstanceTypeC5n_4xlarge,

  InstanceTypeC5n_9xlarge,

  InstanceTypeC5n_18xlarge,

  InstanceTypeC6g_Metal,

  InstanceTypeC6g_Medium,

  InstanceTypeC6g_Large,

  InstanceTypeC6g_Xlarge,

  InstanceTypeC6g_2xlarge,

  InstanceTypeC6g_4xlarge,

  InstanceTypeC6g_8xlarge,

  InstanceTypeC6g_12xlarge,

  InstanceTypeC6g_16xlarge,

  InstanceTypeC6gd_Metal,

  InstanceTypeC6gd_Medium,

  InstanceTypeC6gd_Large,

  InstanceTypeC6gd_Xlarge,

  InstanceTypeC6gd_2xlarge,

  InstanceTypeC6gd_4xlarge,

  InstanceTypeC6gd_8xlarge,

  InstanceTypeC6gd_12xlarge,

  InstanceTypeC6gd_16xlarge,

  InstanceTypeCC1_4xlarge,

  InstanceTypeCC2_8xlarge,

  InstanceTypeG2_2xlarge,

  InstanceTypeG2_8xlarge,

  InstanceTypeG3_4xlarge,

  InstanceTypeG3_8xlarge,

  InstanceTypeG3_16xlarge,

  InstanceTypeG3s_Xlarge,

  InstanceTypeG4dn_Xlarge,

  InstanceTypeG4dn_2xlarge,

  InstanceTypeG4dn_4xlarge,

  InstanceTypeG4dn_8xlarge,

  InstanceTypeG4dn_12xlarge,

  InstanceTypeG4dn_16xlarge,

  InstanceTypeG4dn_Metal,

  InstanceTypeCG1_4xlarge,

  InstanceTypeP2_Xlarge,

  InstanceTypeP2_8xlarge,

  InstanceTypeP2_16xlarge,

  InstanceTypeP3_2xlarge,

  InstanceTypeP3_8xlarge,

  InstanceTypeP3_16xlarge,

  InstanceTypeP3dn_24xlarge,

  InstanceTypeP4d_24xlarge,

  InstanceTypeD2_Xlarge,

  InstanceTypeD2_2xlarge,

  InstanceTypeD2_4xlarge,

  InstanceTypeD2_8xlarge,

  InstanceTypeF1_2xlarge,

  InstanceTypeF1_4xlarge,

  InstanceTypeF1_16xlarge,

  InstanceTypeM5_Large,

  InstanceTypeM5_Xlarge,

  InstanceTypeM5_2xlarge,

  InstanceTypeM5_4xlarge,

  InstanceTypeM5_8xlarge,

  InstanceTypeM5_12xlarge,

  InstanceTypeM5_16xlarge,

  InstanceTypeM5_24xlarge,

  InstanceTypeM5_Metal,

  InstanceTypeM5a_Large,

  InstanceTypeM5a_Xlarge,

  InstanceTypeM5a_2xlarge,

  InstanceTypeM5a_4xlarge,

  InstanceTypeM5a_8xlarge,

  InstanceTypeM5a_12xlarge,

  InstanceTypeM5a_16xlarge,

  InstanceTypeM5a_24xlarge,

  InstanceTypeM5d_Large,

  InstanceTypeM5d_Xlarge,

  InstanceTypeM5d_2xlarge,

  InstanceTypeM5d_4xlarge,

  InstanceTypeM5d_8xlarge,

  InstanceTypeM5d_12xlarge,

  InstanceTypeM5d_16xlarge,

  InstanceTypeM5d_24xlarge,

  InstanceTypeM5d_Metal,

  InstanceTypeM5ad_Large,

  InstanceTypeM5ad_Xlarge,

  InstanceTypeM5ad_2xlarge,

  InstanceTypeM5ad_4xlarge,

  InstanceTypeM5ad_8xlarge,

  InstanceTypeM5ad_12xlarge,

  InstanceTypeM5ad_16xlarge,

  InstanceTypeM5ad_24xlarge,

  InstanceTypeH1_2xlarge,

  InstanceTypeH1_4xlarge,

  InstanceTypeH1_8xlarge,

  InstanceTypeH1_16xlarge,

  InstanceTypeZ1d_Large,

  InstanceTypeZ1d_Xlarge,

  InstanceTypeZ1d_2xlarge,

  InstanceTypeZ1d_3xlarge,

  InstanceTypeZ1d_6xlarge,

  InstanceTypeZ1d_12xlarge,

  InstanceTypeZ1d_Metal,

  InstanceTypeU6TB1_Metal,

  InstanceTypeU9TB1_Metal,

  InstanceTypeU12TB1_Metal,

  InstanceTypeU18TB1_Metal,

  InstanceTypeU24TB1_Metal,

  InstanceTypeA1_Medium,

  InstanceTypeA1_Large,

  InstanceTypeA1_Xlarge,

  InstanceTypeA1_2xlarge,

  InstanceTypeA1_4xlarge,

  InstanceTypeA1_Metal,

  InstanceTypeM5dn_Large,

  InstanceTypeM5dn_Xlarge,

  InstanceTypeM5dn_2xlarge,

  InstanceTypeM5dn_4xlarge,

  InstanceTypeM5dn_8xlarge,

  InstanceTypeM5dn_12xlarge,

  InstanceTypeM5dn_16xlarge,

  InstanceTypeM5dn_24xlarge,

  InstanceTypeM5n_Large,

  InstanceTypeM5n_Xlarge,

  InstanceTypeM5n_2xlarge,

  InstanceTypeM5n_4xlarge,

  InstanceTypeM5n_8xlarge,

  InstanceTypeM5n_12xlarge,

  InstanceTypeM5n_16xlarge,

  InstanceTypeM5n_24xlarge,

  InstanceTypeR5dn_Large,

  InstanceTypeR5dn_Xlarge,

  InstanceTypeR5dn_2xlarge,

  InstanceTypeR5dn_4xlarge,

  InstanceTypeR5dn_8xlarge,

  InstanceTypeR5dn_12xlarge,

  InstanceTypeR5dn_16xlarge,

  InstanceTypeR5dn_24xlarge,

  InstanceTypeR5n_Large,

  InstanceTypeR5n_Xlarge,

  InstanceTypeR5n_2xlarge,

  InstanceTypeR5n_4xlarge,

  InstanceTypeR5n_8xlarge,

  InstanceTypeR5n_12xlarge,

  InstanceTypeR5n_16xlarge,

  InstanceTypeR5n_24xlarge,

  InstanceTypeINF1_Xlarge,

  InstanceTypeINF1_2xlarge,

  InstanceTypeINF1_6xlarge,

  InstanceTypeINF1_24xlarge,

  InstanceTypeM6g_Metal,

  InstanceTypeM6g_Medium,

  InstanceTypeM6g_Large,

  InstanceTypeM6g_Xlarge,

  InstanceTypeM6g_2xlarge,

  InstanceTypeM6g_4xlarge,

  InstanceTypeM6g_8xlarge,

  InstanceTypeM6g_12xlarge,

  InstanceTypeM6g_16xlarge,

  InstanceTypeM6gd_Metal,

  InstanceTypeM6gd_Medium,

  InstanceTypeM6gd_Large,

  InstanceTypeM6gd_Xlarge,

  InstanceTypeM6gd_2xlarge,

  InstanceTypeM6gd_4xlarge,

  InstanceTypeM6gd_8xlarge,

  InstanceTypeM6gd_12xlarge,

  InstanceTypeM6gd_16xlarge,
  InstanceType'
  #-}
