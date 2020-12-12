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
        EITC3_2XLarge,
        EITC3_4XLarge,
        EITC3_8XLarge,
        EITC3_Large,
        EITC3_XLarge,
        EITC4_2XLarge,
        EITC4_4XLarge,
        EITC4_8XLarge,
        EITC4_Large,
        EITC4_XLarge,
        EITC5_12XLarge,
        EITC5_18XLarge,
        EITC5_24XLarge,
        EITC5_2XLarge,
        EITC5_4XLarge,
        EITC5_9XLarge,
        EITC5_Large,
        EITC5_XLarge,
        EITC5a_12XLarge,
        EITC5a_16XLarge,
        EITC5a_24XLarge,
        EITC5a_2XLarge,
        EITC5a_4XLarge,
        EITC5a_8XLarge,
        EITC5a_Large,
        EITC5a_XLarge,
        EITM3_2XLarge,
        EITM3_Large,
        EITM3_Medium,
        EITM3_XLarge,
        EITM4_10XLarge,
        EITM4_2XLarge,
        EITM4_4XLarge,
        EITM4_Large,
        EITM4_XLarge,
        EITM5_12XLarge,
        EITM5_16XLarge,
        EITM5_24XLarge,
        EITM5_2XLarge,
        EITM5_4XLarge,
        EITM5_8XLarge,
        EITM5_Large,
        EITM5_XLarge,
        EITM5a_12XLarge,
        EITM5a_16XLarge,
        EITM5a_24XLarge,
        EITM5a_2XLarge,
        EITM5a_4XLarge,
        EITM5a_8XLarge,
        EITM5a_Large,
        EITM5a_XLarge,
        EITR3_2XLarge,
        EITR3_4XLarge,
        EITR3_8XLarge,
        EITR3_Large,
        EITR3_XLarge,
        EITR4_16XLarge,
        EITR4_2XLarge,
        EITR4_4XLarge,
        EITR4_8XLarge,
        EITR4_Large,
        EITR4_XLarge,
        EITR5_12XLarge,
        EITR5_16XLarge,
        EITR5_24XLarge,
        EITR5_2XLarge,
        EITR5_4XLarge,
        EITR5_8XLarge,
        EITR5_Large,
        EITR5_XLarge,
        EITR5a_12XLarge,
        EITR5a_16XLarge,
        EITR5a_24XLarge,
        EITR5a_2XLarge,
        EITR5a_4XLarge,
        EITR5a_8XLarge,
        EITR5a_Large,
        EITR5a_XLarge,
        EITT2_Large,
        EITT2_Medium,
        EITT2_Micro,
        EITT2_Small
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EC2InstanceType = EC2InstanceType' Lude.Text
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

pattern EITC3_2XLarge :: EC2InstanceType
pattern EITC3_2XLarge = EC2InstanceType' "c3.2xlarge"

pattern EITC3_4XLarge :: EC2InstanceType
pattern EITC3_4XLarge = EC2InstanceType' "c3.4xlarge"

pattern EITC3_8XLarge :: EC2InstanceType
pattern EITC3_8XLarge = EC2InstanceType' "c3.8xlarge"

pattern EITC3_Large :: EC2InstanceType
pattern EITC3_Large = EC2InstanceType' "c3.large"

pattern EITC3_XLarge :: EC2InstanceType
pattern EITC3_XLarge = EC2InstanceType' "c3.xlarge"

pattern EITC4_2XLarge :: EC2InstanceType
pattern EITC4_2XLarge = EC2InstanceType' "c4.2xlarge"

pattern EITC4_4XLarge :: EC2InstanceType
pattern EITC4_4XLarge = EC2InstanceType' "c4.4xlarge"

pattern EITC4_8XLarge :: EC2InstanceType
pattern EITC4_8XLarge = EC2InstanceType' "c4.8xlarge"

pattern EITC4_Large :: EC2InstanceType
pattern EITC4_Large = EC2InstanceType' "c4.large"

pattern EITC4_XLarge :: EC2InstanceType
pattern EITC4_XLarge = EC2InstanceType' "c4.xlarge"

pattern EITC5_12XLarge :: EC2InstanceType
pattern EITC5_12XLarge = EC2InstanceType' "c5.12xlarge"

pattern EITC5_18XLarge :: EC2InstanceType
pattern EITC5_18XLarge = EC2InstanceType' "c5.18xlarge"

pattern EITC5_24XLarge :: EC2InstanceType
pattern EITC5_24XLarge = EC2InstanceType' "c5.24xlarge"

pattern EITC5_2XLarge :: EC2InstanceType
pattern EITC5_2XLarge = EC2InstanceType' "c5.2xlarge"

pattern EITC5_4XLarge :: EC2InstanceType
pattern EITC5_4XLarge = EC2InstanceType' "c5.4xlarge"

pattern EITC5_9XLarge :: EC2InstanceType
pattern EITC5_9XLarge = EC2InstanceType' "c5.9xlarge"

pattern EITC5_Large :: EC2InstanceType
pattern EITC5_Large = EC2InstanceType' "c5.large"

pattern EITC5_XLarge :: EC2InstanceType
pattern EITC5_XLarge = EC2InstanceType' "c5.xlarge"

pattern EITC5a_12XLarge :: EC2InstanceType
pattern EITC5a_12XLarge = EC2InstanceType' "c5a.12xlarge"

pattern EITC5a_16XLarge :: EC2InstanceType
pattern EITC5a_16XLarge = EC2InstanceType' "c5a.16xlarge"

pattern EITC5a_24XLarge :: EC2InstanceType
pattern EITC5a_24XLarge = EC2InstanceType' "c5a.24xlarge"

pattern EITC5a_2XLarge :: EC2InstanceType
pattern EITC5a_2XLarge = EC2InstanceType' "c5a.2xlarge"

pattern EITC5a_4XLarge :: EC2InstanceType
pattern EITC5a_4XLarge = EC2InstanceType' "c5a.4xlarge"

pattern EITC5a_8XLarge :: EC2InstanceType
pattern EITC5a_8XLarge = EC2InstanceType' "c5a.8xlarge"

pattern EITC5a_Large :: EC2InstanceType
pattern EITC5a_Large = EC2InstanceType' "c5a.large"

pattern EITC5a_XLarge :: EC2InstanceType
pattern EITC5a_XLarge = EC2InstanceType' "c5a.xlarge"

pattern EITM3_2XLarge :: EC2InstanceType
pattern EITM3_2XLarge = EC2InstanceType' "m3.2xlarge"

pattern EITM3_Large :: EC2InstanceType
pattern EITM3_Large = EC2InstanceType' "m3.large"

pattern EITM3_Medium :: EC2InstanceType
pattern EITM3_Medium = EC2InstanceType' "m3.medium"

pattern EITM3_XLarge :: EC2InstanceType
pattern EITM3_XLarge = EC2InstanceType' "m3.xlarge"

pattern EITM4_10XLarge :: EC2InstanceType
pattern EITM4_10XLarge = EC2InstanceType' "m4.10xlarge"

pattern EITM4_2XLarge :: EC2InstanceType
pattern EITM4_2XLarge = EC2InstanceType' "m4.2xlarge"

pattern EITM4_4XLarge :: EC2InstanceType
pattern EITM4_4XLarge = EC2InstanceType' "m4.4xlarge"

pattern EITM4_Large :: EC2InstanceType
pattern EITM4_Large = EC2InstanceType' "m4.large"

pattern EITM4_XLarge :: EC2InstanceType
pattern EITM4_XLarge = EC2InstanceType' "m4.xlarge"

pattern EITM5_12XLarge :: EC2InstanceType
pattern EITM5_12XLarge = EC2InstanceType' "m5.12xlarge"

pattern EITM5_16XLarge :: EC2InstanceType
pattern EITM5_16XLarge = EC2InstanceType' "m5.16xlarge"

pattern EITM5_24XLarge :: EC2InstanceType
pattern EITM5_24XLarge = EC2InstanceType' "m5.24xlarge"

pattern EITM5_2XLarge :: EC2InstanceType
pattern EITM5_2XLarge = EC2InstanceType' "m5.2xlarge"

pattern EITM5_4XLarge :: EC2InstanceType
pattern EITM5_4XLarge = EC2InstanceType' "m5.4xlarge"

pattern EITM5_8XLarge :: EC2InstanceType
pattern EITM5_8XLarge = EC2InstanceType' "m5.8xlarge"

pattern EITM5_Large :: EC2InstanceType
pattern EITM5_Large = EC2InstanceType' "m5.large"

pattern EITM5_XLarge :: EC2InstanceType
pattern EITM5_XLarge = EC2InstanceType' "m5.xlarge"

pattern EITM5a_12XLarge :: EC2InstanceType
pattern EITM5a_12XLarge = EC2InstanceType' "m5a.12xlarge"

pattern EITM5a_16XLarge :: EC2InstanceType
pattern EITM5a_16XLarge = EC2InstanceType' "m5a.16xlarge"

pattern EITM5a_24XLarge :: EC2InstanceType
pattern EITM5a_24XLarge = EC2InstanceType' "m5a.24xlarge"

pattern EITM5a_2XLarge :: EC2InstanceType
pattern EITM5a_2XLarge = EC2InstanceType' "m5a.2xlarge"

pattern EITM5a_4XLarge :: EC2InstanceType
pattern EITM5a_4XLarge = EC2InstanceType' "m5a.4xlarge"

pattern EITM5a_8XLarge :: EC2InstanceType
pattern EITM5a_8XLarge = EC2InstanceType' "m5a.8xlarge"

pattern EITM5a_Large :: EC2InstanceType
pattern EITM5a_Large = EC2InstanceType' "m5a.large"

pattern EITM5a_XLarge :: EC2InstanceType
pattern EITM5a_XLarge = EC2InstanceType' "m5a.xlarge"

pattern EITR3_2XLarge :: EC2InstanceType
pattern EITR3_2XLarge = EC2InstanceType' "r3.2xlarge"

pattern EITR3_4XLarge :: EC2InstanceType
pattern EITR3_4XLarge = EC2InstanceType' "r3.4xlarge"

pattern EITR3_8XLarge :: EC2InstanceType
pattern EITR3_8XLarge = EC2InstanceType' "r3.8xlarge"

pattern EITR3_Large :: EC2InstanceType
pattern EITR3_Large = EC2InstanceType' "r3.large"

pattern EITR3_XLarge :: EC2InstanceType
pattern EITR3_XLarge = EC2InstanceType' "r3.xlarge"

pattern EITR4_16XLarge :: EC2InstanceType
pattern EITR4_16XLarge = EC2InstanceType' "r4.16xlarge"

pattern EITR4_2XLarge :: EC2InstanceType
pattern EITR4_2XLarge = EC2InstanceType' "r4.2xlarge"

pattern EITR4_4XLarge :: EC2InstanceType
pattern EITR4_4XLarge = EC2InstanceType' "r4.4xlarge"

pattern EITR4_8XLarge :: EC2InstanceType
pattern EITR4_8XLarge = EC2InstanceType' "r4.8xlarge"

pattern EITR4_Large :: EC2InstanceType
pattern EITR4_Large = EC2InstanceType' "r4.large"

pattern EITR4_XLarge :: EC2InstanceType
pattern EITR4_XLarge = EC2InstanceType' "r4.xlarge"

pattern EITR5_12XLarge :: EC2InstanceType
pattern EITR5_12XLarge = EC2InstanceType' "r5.12xlarge"

pattern EITR5_16XLarge :: EC2InstanceType
pattern EITR5_16XLarge = EC2InstanceType' "r5.16xlarge"

pattern EITR5_24XLarge :: EC2InstanceType
pattern EITR5_24XLarge = EC2InstanceType' "r5.24xlarge"

pattern EITR5_2XLarge :: EC2InstanceType
pattern EITR5_2XLarge = EC2InstanceType' "r5.2xlarge"

pattern EITR5_4XLarge :: EC2InstanceType
pattern EITR5_4XLarge = EC2InstanceType' "r5.4xlarge"

pattern EITR5_8XLarge :: EC2InstanceType
pattern EITR5_8XLarge = EC2InstanceType' "r5.8xlarge"

pattern EITR5_Large :: EC2InstanceType
pattern EITR5_Large = EC2InstanceType' "r5.large"

pattern EITR5_XLarge :: EC2InstanceType
pattern EITR5_XLarge = EC2InstanceType' "r5.xlarge"

pattern EITR5a_12XLarge :: EC2InstanceType
pattern EITR5a_12XLarge = EC2InstanceType' "r5a.12xlarge"

pattern EITR5a_16XLarge :: EC2InstanceType
pattern EITR5a_16XLarge = EC2InstanceType' "r5a.16xlarge"

pattern EITR5a_24XLarge :: EC2InstanceType
pattern EITR5a_24XLarge = EC2InstanceType' "r5a.24xlarge"

pattern EITR5a_2XLarge :: EC2InstanceType
pattern EITR5a_2XLarge = EC2InstanceType' "r5a.2xlarge"

pattern EITR5a_4XLarge :: EC2InstanceType
pattern EITR5a_4XLarge = EC2InstanceType' "r5a.4xlarge"

pattern EITR5a_8XLarge :: EC2InstanceType
pattern EITR5a_8XLarge = EC2InstanceType' "r5a.8xlarge"

pattern EITR5a_Large :: EC2InstanceType
pattern EITR5a_Large = EC2InstanceType' "r5a.large"

pattern EITR5a_XLarge :: EC2InstanceType
pattern EITR5a_XLarge = EC2InstanceType' "r5a.xlarge"

pattern EITT2_Large :: EC2InstanceType
pattern EITT2_Large = EC2InstanceType' "t2.large"

pattern EITT2_Medium :: EC2InstanceType
pattern EITT2_Medium = EC2InstanceType' "t2.medium"

pattern EITT2_Micro :: EC2InstanceType
pattern EITT2_Micro = EC2InstanceType' "t2.micro"

pattern EITT2_Small :: EC2InstanceType
pattern EITT2_Small = EC2InstanceType' "t2.small"

{-# COMPLETE
  EITC3_2XLarge,
  EITC3_4XLarge,
  EITC3_8XLarge,
  EITC3_Large,
  EITC3_XLarge,
  EITC4_2XLarge,
  EITC4_4XLarge,
  EITC4_8XLarge,
  EITC4_Large,
  EITC4_XLarge,
  EITC5_12XLarge,
  EITC5_18XLarge,
  EITC5_24XLarge,
  EITC5_2XLarge,
  EITC5_4XLarge,
  EITC5_9XLarge,
  EITC5_Large,
  EITC5_XLarge,
  EITC5a_12XLarge,
  EITC5a_16XLarge,
  EITC5a_24XLarge,
  EITC5a_2XLarge,
  EITC5a_4XLarge,
  EITC5a_8XLarge,
  EITC5a_Large,
  EITC5a_XLarge,
  EITM3_2XLarge,
  EITM3_Large,
  EITM3_Medium,
  EITM3_XLarge,
  EITM4_10XLarge,
  EITM4_2XLarge,
  EITM4_4XLarge,
  EITM4_Large,
  EITM4_XLarge,
  EITM5_12XLarge,
  EITM5_16XLarge,
  EITM5_24XLarge,
  EITM5_2XLarge,
  EITM5_4XLarge,
  EITM5_8XLarge,
  EITM5_Large,
  EITM5_XLarge,
  EITM5a_12XLarge,
  EITM5a_16XLarge,
  EITM5a_24XLarge,
  EITM5a_2XLarge,
  EITM5a_4XLarge,
  EITM5a_8XLarge,
  EITM5a_Large,
  EITM5a_XLarge,
  EITR3_2XLarge,
  EITR3_4XLarge,
  EITR3_8XLarge,
  EITR3_Large,
  EITR3_XLarge,
  EITR4_16XLarge,
  EITR4_2XLarge,
  EITR4_4XLarge,
  EITR4_8XLarge,
  EITR4_Large,
  EITR4_XLarge,
  EITR5_12XLarge,
  EITR5_16XLarge,
  EITR5_24XLarge,
  EITR5_2XLarge,
  EITR5_4XLarge,
  EITR5_8XLarge,
  EITR5_Large,
  EITR5_XLarge,
  EITR5a_12XLarge,
  EITR5a_16XLarge,
  EITR5a_24XLarge,
  EITR5a_2XLarge,
  EITR5a_4XLarge,
  EITR5a_8XLarge,
  EITR5a_Large,
  EITR5a_XLarge,
  EITT2_Large,
  EITT2_Medium,
  EITT2_Micro,
  EITT2_Small,
  EC2InstanceType'
  #-}
