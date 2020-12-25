{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.EC2InstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.EC2InstanceType
  ( EC2InstanceType
      ( EC2InstanceType',
        EC2InstanceTypeT2_Micro,
        EC2InstanceTypeT2_Small,
        EC2InstanceTypeT2_Medium,
        EC2InstanceTypeT2_Large,
        EC2InstanceTypeC3_Large,
        EC2InstanceTypeC3_Xlarge,
        EC2InstanceTypeC3_2xlarge,
        EC2InstanceTypeC3_4xlarge,
        EC2InstanceTypeC3_8xlarge,
        EC2InstanceTypeC4_Large,
        EC2InstanceTypeC4_Xlarge,
        EC2InstanceTypeC4_2xlarge,
        EC2InstanceTypeC4_4xlarge,
        EC2InstanceTypeC4_8xlarge,
        EC2InstanceTypeC5_Large,
        EC2InstanceTypeC5_Xlarge,
        EC2InstanceTypeC5_2xlarge,
        EC2InstanceTypeC5_4xlarge,
        EC2InstanceTypeC5_9xlarge,
        EC2InstanceTypeC5_12xlarge,
        EC2InstanceTypeC5_18xlarge,
        EC2InstanceTypeC5_24xlarge,
        EC2InstanceTypeC5a_Large,
        EC2InstanceTypeC5a_Xlarge,
        EC2InstanceTypeC5a_2xlarge,
        EC2InstanceTypeC5a_4xlarge,
        EC2InstanceTypeC5a_8xlarge,
        EC2InstanceTypeC5a_12xlarge,
        EC2InstanceTypeC5a_16xlarge,
        EC2InstanceTypeC5a_24xlarge,
        EC2InstanceTypeR3_Large,
        EC2InstanceTypeR3_Xlarge,
        EC2InstanceTypeR3_2xlarge,
        EC2InstanceTypeR3_4xlarge,
        EC2InstanceTypeR3_8xlarge,
        EC2InstanceTypeR4_Large,
        EC2InstanceTypeR4_Xlarge,
        EC2InstanceTypeR4_2xlarge,
        EC2InstanceTypeR4_4xlarge,
        EC2InstanceTypeR4_8xlarge,
        EC2InstanceTypeR4_16xlarge,
        EC2InstanceTypeR5_Large,
        EC2InstanceTypeR5_Xlarge,
        EC2InstanceTypeR5_2xlarge,
        EC2InstanceTypeR5_4xlarge,
        EC2InstanceTypeR5_8xlarge,
        EC2InstanceTypeR5_12xlarge,
        EC2InstanceTypeR5_16xlarge,
        EC2InstanceTypeR5_24xlarge,
        EC2InstanceTypeR5a_Large,
        EC2InstanceTypeR5a_Xlarge,
        EC2InstanceTypeR5a_2xlarge,
        EC2InstanceTypeR5a_4xlarge,
        EC2InstanceTypeR5a_8xlarge,
        EC2InstanceTypeR5a_12xlarge,
        EC2InstanceTypeR5a_16xlarge,
        EC2InstanceTypeR5a_24xlarge,
        EC2InstanceTypeM3_Medium,
        EC2InstanceTypeM3_Large,
        EC2InstanceTypeM3_Xlarge,
        EC2InstanceTypeM3_2xlarge,
        EC2InstanceTypeM4_Large,
        EC2InstanceTypeM4_Xlarge,
        EC2InstanceTypeM4_2xlarge,
        EC2InstanceTypeM4_4xlarge,
        EC2InstanceTypeM4_10xlarge,
        EC2InstanceTypeM5_Large,
        EC2InstanceTypeM5_Xlarge,
        EC2InstanceTypeM5_2xlarge,
        EC2InstanceTypeM5_4xlarge,
        EC2InstanceTypeM5_8xlarge,
        EC2InstanceTypeM5_12xlarge,
        EC2InstanceTypeM5_16xlarge,
        EC2InstanceTypeM5_24xlarge,
        EC2InstanceTypeM5a_Large,
        EC2InstanceTypeM5a_Xlarge,
        EC2InstanceTypeM5a_2xlarge,
        EC2InstanceTypeM5a_4xlarge,
        EC2InstanceTypeM5a_8xlarge,
        EC2InstanceTypeM5a_12xlarge,
        EC2InstanceTypeM5a_16xlarge,
        EC2InstanceTypeM5a_24xlarge,
        fromEC2InstanceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EC2InstanceType = EC2InstanceType'
  { fromEC2InstanceType ::
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

pattern EC2InstanceTypeT2_Micro :: EC2InstanceType
pattern EC2InstanceTypeT2_Micro = EC2InstanceType' "t2.micro"

pattern EC2InstanceTypeT2_Small :: EC2InstanceType
pattern EC2InstanceTypeT2_Small = EC2InstanceType' "t2.small"

pattern EC2InstanceTypeT2_Medium :: EC2InstanceType
pattern EC2InstanceTypeT2_Medium = EC2InstanceType' "t2.medium"

pattern EC2InstanceTypeT2_Large :: EC2InstanceType
pattern EC2InstanceTypeT2_Large = EC2InstanceType' "t2.large"

pattern EC2InstanceTypeC3_Large :: EC2InstanceType
pattern EC2InstanceTypeC3_Large = EC2InstanceType' "c3.large"

pattern EC2InstanceTypeC3_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeC3_Xlarge = EC2InstanceType' "c3.xlarge"

pattern EC2InstanceTypeC3_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeC3_2xlarge = EC2InstanceType' "c3.2xlarge"

pattern EC2InstanceTypeC3_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeC3_4xlarge = EC2InstanceType' "c3.4xlarge"

pattern EC2InstanceTypeC3_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeC3_8xlarge = EC2InstanceType' "c3.8xlarge"

pattern EC2InstanceTypeC4_Large :: EC2InstanceType
pattern EC2InstanceTypeC4_Large = EC2InstanceType' "c4.large"

pattern EC2InstanceTypeC4_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeC4_Xlarge = EC2InstanceType' "c4.xlarge"

pattern EC2InstanceTypeC4_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeC4_2xlarge = EC2InstanceType' "c4.2xlarge"

pattern EC2InstanceTypeC4_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeC4_4xlarge = EC2InstanceType' "c4.4xlarge"

pattern EC2InstanceTypeC4_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeC4_8xlarge = EC2InstanceType' "c4.8xlarge"

pattern EC2InstanceTypeC5_Large :: EC2InstanceType
pattern EC2InstanceTypeC5_Large = EC2InstanceType' "c5.large"

pattern EC2InstanceTypeC5_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_Xlarge = EC2InstanceType' "c5.xlarge"

pattern EC2InstanceTypeC5_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_2xlarge = EC2InstanceType' "c5.2xlarge"

pattern EC2InstanceTypeC5_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_4xlarge = EC2InstanceType' "c5.4xlarge"

pattern EC2InstanceTypeC5_9xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_9xlarge = EC2InstanceType' "c5.9xlarge"

pattern EC2InstanceTypeC5_12xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_12xlarge = EC2InstanceType' "c5.12xlarge"

pattern EC2InstanceTypeC5_18xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_18xlarge = EC2InstanceType' "c5.18xlarge"

pattern EC2InstanceTypeC5_24xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5_24xlarge = EC2InstanceType' "c5.24xlarge"

pattern EC2InstanceTypeC5a_Large :: EC2InstanceType
pattern EC2InstanceTypeC5a_Large = EC2InstanceType' "c5a.large"

pattern EC2InstanceTypeC5a_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_Xlarge = EC2InstanceType' "c5a.xlarge"

pattern EC2InstanceTypeC5a_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_2xlarge = EC2InstanceType' "c5a.2xlarge"

pattern EC2InstanceTypeC5a_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_4xlarge = EC2InstanceType' "c5a.4xlarge"

pattern EC2InstanceTypeC5a_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_8xlarge = EC2InstanceType' "c5a.8xlarge"

pattern EC2InstanceTypeC5a_12xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_12xlarge = EC2InstanceType' "c5a.12xlarge"

pattern EC2InstanceTypeC5a_16xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_16xlarge = EC2InstanceType' "c5a.16xlarge"

pattern EC2InstanceTypeC5a_24xlarge :: EC2InstanceType
pattern EC2InstanceTypeC5a_24xlarge = EC2InstanceType' "c5a.24xlarge"

pattern EC2InstanceTypeR3_Large :: EC2InstanceType
pattern EC2InstanceTypeR3_Large = EC2InstanceType' "r3.large"

pattern EC2InstanceTypeR3_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeR3_Xlarge = EC2InstanceType' "r3.xlarge"

pattern EC2InstanceTypeR3_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeR3_2xlarge = EC2InstanceType' "r3.2xlarge"

pattern EC2InstanceTypeR3_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeR3_4xlarge = EC2InstanceType' "r3.4xlarge"

pattern EC2InstanceTypeR3_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeR3_8xlarge = EC2InstanceType' "r3.8xlarge"

pattern EC2InstanceTypeR4_Large :: EC2InstanceType
pattern EC2InstanceTypeR4_Large = EC2InstanceType' "r4.large"

pattern EC2InstanceTypeR4_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeR4_Xlarge = EC2InstanceType' "r4.xlarge"

pattern EC2InstanceTypeR4_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeR4_2xlarge = EC2InstanceType' "r4.2xlarge"

pattern EC2InstanceTypeR4_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeR4_4xlarge = EC2InstanceType' "r4.4xlarge"

pattern EC2InstanceTypeR4_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeR4_8xlarge = EC2InstanceType' "r4.8xlarge"

pattern EC2InstanceTypeR4_16xlarge :: EC2InstanceType
pattern EC2InstanceTypeR4_16xlarge = EC2InstanceType' "r4.16xlarge"

pattern EC2InstanceTypeR5_Large :: EC2InstanceType
pattern EC2InstanceTypeR5_Large = EC2InstanceType' "r5.large"

pattern EC2InstanceTypeR5_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_Xlarge = EC2InstanceType' "r5.xlarge"

pattern EC2InstanceTypeR5_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_2xlarge = EC2InstanceType' "r5.2xlarge"

pattern EC2InstanceTypeR5_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_4xlarge = EC2InstanceType' "r5.4xlarge"

pattern EC2InstanceTypeR5_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_8xlarge = EC2InstanceType' "r5.8xlarge"

pattern EC2InstanceTypeR5_12xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_12xlarge = EC2InstanceType' "r5.12xlarge"

pattern EC2InstanceTypeR5_16xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_16xlarge = EC2InstanceType' "r5.16xlarge"

pattern EC2InstanceTypeR5_24xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5_24xlarge = EC2InstanceType' "r5.24xlarge"

pattern EC2InstanceTypeR5a_Large :: EC2InstanceType
pattern EC2InstanceTypeR5a_Large = EC2InstanceType' "r5a.large"

pattern EC2InstanceTypeR5a_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_Xlarge = EC2InstanceType' "r5a.xlarge"

pattern EC2InstanceTypeR5a_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_2xlarge = EC2InstanceType' "r5a.2xlarge"

pattern EC2InstanceTypeR5a_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_4xlarge = EC2InstanceType' "r5a.4xlarge"

pattern EC2InstanceTypeR5a_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_8xlarge = EC2InstanceType' "r5a.8xlarge"

pattern EC2InstanceTypeR5a_12xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_12xlarge = EC2InstanceType' "r5a.12xlarge"

pattern EC2InstanceTypeR5a_16xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_16xlarge = EC2InstanceType' "r5a.16xlarge"

pattern EC2InstanceTypeR5a_24xlarge :: EC2InstanceType
pattern EC2InstanceTypeR5a_24xlarge = EC2InstanceType' "r5a.24xlarge"

pattern EC2InstanceTypeM3_Medium :: EC2InstanceType
pattern EC2InstanceTypeM3_Medium = EC2InstanceType' "m3.medium"

pattern EC2InstanceTypeM3_Large :: EC2InstanceType
pattern EC2InstanceTypeM3_Large = EC2InstanceType' "m3.large"

pattern EC2InstanceTypeM3_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeM3_Xlarge = EC2InstanceType' "m3.xlarge"

pattern EC2InstanceTypeM3_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeM3_2xlarge = EC2InstanceType' "m3.2xlarge"

pattern EC2InstanceTypeM4_Large :: EC2InstanceType
pattern EC2InstanceTypeM4_Large = EC2InstanceType' "m4.large"

pattern EC2InstanceTypeM4_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeM4_Xlarge = EC2InstanceType' "m4.xlarge"

pattern EC2InstanceTypeM4_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeM4_2xlarge = EC2InstanceType' "m4.2xlarge"

pattern EC2InstanceTypeM4_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeM4_4xlarge = EC2InstanceType' "m4.4xlarge"

pattern EC2InstanceTypeM4_10xlarge :: EC2InstanceType
pattern EC2InstanceTypeM4_10xlarge = EC2InstanceType' "m4.10xlarge"

pattern EC2InstanceTypeM5_Large :: EC2InstanceType
pattern EC2InstanceTypeM5_Large = EC2InstanceType' "m5.large"

pattern EC2InstanceTypeM5_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_Xlarge = EC2InstanceType' "m5.xlarge"

pattern EC2InstanceTypeM5_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_2xlarge = EC2InstanceType' "m5.2xlarge"

pattern EC2InstanceTypeM5_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_4xlarge = EC2InstanceType' "m5.4xlarge"

pattern EC2InstanceTypeM5_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_8xlarge = EC2InstanceType' "m5.8xlarge"

pattern EC2InstanceTypeM5_12xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_12xlarge = EC2InstanceType' "m5.12xlarge"

pattern EC2InstanceTypeM5_16xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_16xlarge = EC2InstanceType' "m5.16xlarge"

pattern EC2InstanceTypeM5_24xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5_24xlarge = EC2InstanceType' "m5.24xlarge"

pattern EC2InstanceTypeM5a_Large :: EC2InstanceType
pattern EC2InstanceTypeM5a_Large = EC2InstanceType' "m5a.large"

pattern EC2InstanceTypeM5a_Xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_Xlarge = EC2InstanceType' "m5a.xlarge"

pattern EC2InstanceTypeM5a_2xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_2xlarge = EC2InstanceType' "m5a.2xlarge"

pattern EC2InstanceTypeM5a_4xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_4xlarge = EC2InstanceType' "m5a.4xlarge"

pattern EC2InstanceTypeM5a_8xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_8xlarge = EC2InstanceType' "m5a.8xlarge"

pattern EC2InstanceTypeM5a_12xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_12xlarge = EC2InstanceType' "m5a.12xlarge"

pattern EC2InstanceTypeM5a_16xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_16xlarge = EC2InstanceType' "m5a.16xlarge"

pattern EC2InstanceTypeM5a_24xlarge :: EC2InstanceType
pattern EC2InstanceTypeM5a_24xlarge = EC2InstanceType' "m5a.24xlarge"

{-# COMPLETE
  EC2InstanceTypeT2_Micro,
  EC2InstanceTypeT2_Small,
  EC2InstanceTypeT2_Medium,
  EC2InstanceTypeT2_Large,
  EC2InstanceTypeC3_Large,
  EC2InstanceTypeC3_Xlarge,
  EC2InstanceTypeC3_2xlarge,
  EC2InstanceTypeC3_4xlarge,
  EC2InstanceTypeC3_8xlarge,
  EC2InstanceTypeC4_Large,
  EC2InstanceTypeC4_Xlarge,
  EC2InstanceTypeC4_2xlarge,
  EC2InstanceTypeC4_4xlarge,
  EC2InstanceTypeC4_8xlarge,
  EC2InstanceTypeC5_Large,
  EC2InstanceTypeC5_Xlarge,
  EC2InstanceTypeC5_2xlarge,
  EC2InstanceTypeC5_4xlarge,
  EC2InstanceTypeC5_9xlarge,
  EC2InstanceTypeC5_12xlarge,
  EC2InstanceTypeC5_18xlarge,
  EC2InstanceTypeC5_24xlarge,
  EC2InstanceTypeC5a_Large,
  EC2InstanceTypeC5a_Xlarge,
  EC2InstanceTypeC5a_2xlarge,
  EC2InstanceTypeC5a_4xlarge,
  EC2InstanceTypeC5a_8xlarge,
  EC2InstanceTypeC5a_12xlarge,
  EC2InstanceTypeC5a_16xlarge,
  EC2InstanceTypeC5a_24xlarge,
  EC2InstanceTypeR3_Large,
  EC2InstanceTypeR3_Xlarge,
  EC2InstanceTypeR3_2xlarge,
  EC2InstanceTypeR3_4xlarge,
  EC2InstanceTypeR3_8xlarge,
  EC2InstanceTypeR4_Large,
  EC2InstanceTypeR4_Xlarge,
  EC2InstanceTypeR4_2xlarge,
  EC2InstanceTypeR4_4xlarge,
  EC2InstanceTypeR4_8xlarge,
  EC2InstanceTypeR4_16xlarge,
  EC2InstanceTypeR5_Large,
  EC2InstanceTypeR5_Xlarge,
  EC2InstanceTypeR5_2xlarge,
  EC2InstanceTypeR5_4xlarge,
  EC2InstanceTypeR5_8xlarge,
  EC2InstanceTypeR5_12xlarge,
  EC2InstanceTypeR5_16xlarge,
  EC2InstanceTypeR5_24xlarge,
  EC2InstanceTypeR5a_Large,
  EC2InstanceTypeR5a_Xlarge,
  EC2InstanceTypeR5a_2xlarge,
  EC2InstanceTypeR5a_4xlarge,
  EC2InstanceTypeR5a_8xlarge,
  EC2InstanceTypeR5a_12xlarge,
  EC2InstanceTypeR5a_16xlarge,
  EC2InstanceTypeR5a_24xlarge,
  EC2InstanceTypeM3_Medium,
  EC2InstanceTypeM3_Large,
  EC2InstanceTypeM3_Xlarge,
  EC2InstanceTypeM3_2xlarge,
  EC2InstanceTypeM4_Large,
  EC2InstanceTypeM4_Xlarge,
  EC2InstanceTypeM4_2xlarge,
  EC2InstanceTypeM4_4xlarge,
  EC2InstanceTypeM4_10xlarge,
  EC2InstanceTypeM5_Large,
  EC2InstanceTypeM5_Xlarge,
  EC2InstanceTypeM5_2xlarge,
  EC2InstanceTypeM5_4xlarge,
  EC2InstanceTypeM5_8xlarge,
  EC2InstanceTypeM5_12xlarge,
  EC2InstanceTypeM5_16xlarge,
  EC2InstanceTypeM5_24xlarge,
  EC2InstanceTypeM5a_Large,
  EC2InstanceTypeM5a_Xlarge,
  EC2InstanceTypeM5a_2xlarge,
  EC2InstanceTypeM5a_4xlarge,
  EC2InstanceTypeM5a_8xlarge,
  EC2InstanceTypeM5a_12xlarge,
  EC2InstanceTypeM5a_16xlarge,
  EC2InstanceTypeM5a_24xlarge,
  EC2InstanceType'
  #-}
