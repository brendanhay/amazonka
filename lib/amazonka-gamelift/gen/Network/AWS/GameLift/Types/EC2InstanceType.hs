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
        T2_Micro,
        T2_Small,
        T2_Medium,
        T2_Large,
        C3_Large,
        C3_XLarge,
        C3_2XLarge,
        C3_4XLarge,
        C3_8XLarge,
        C4_Large,
        C4_XLarge,
        C4_2XLarge,
        C4_4XLarge,
        C4_8XLarge,
        C5_Large,
        C5_XLarge,
        C5_2XLarge,
        C5_4XLarge,
        C5_9XLarge,
        C5_12XLarge,
        C5_18XLarge,
        C5_24XLarge,
        C5a_Large,
        C5a_XLarge,
        C5a_2XLarge,
        C5a_4XLarge,
        C5a_8XLarge,
        C5a_12XLarge,
        C5a_16XLarge,
        C5a_24XLarge,
        R3_Large,
        R3_XLarge,
        R3_2XLarge,
        R3_4XLarge,
        R3_8XLarge,
        R4_Large,
        R4_XLarge,
        R4_2XLarge,
        R4_4XLarge,
        R4_8XLarge,
        R4_16XLarge,
        R5_Large,
        R5_XLarge,
        R5_2XLarge,
        R5_4XLarge,
        R5_8XLarge,
        R5_12XLarge,
        R5_16XLarge,
        R5_24XLarge,
        R5a_Large,
        R5a_XLarge,
        R5a_2XLarge,
        R5a_4XLarge,
        R5a_8XLarge,
        R5a_12XLarge,
        R5a_16XLarge,
        R5a_24XLarge,
        M3_Medium,
        M3_Large,
        M3_XLarge,
        M3_2XLarge,
        M4_Large,
        M4_XLarge,
        M4_2XLarge,
        M4_4XLarge,
        M4_10XLarge,
        M5_Large,
        M5_XLarge,
        M5_2XLarge,
        M5_4XLarge,
        M5_8XLarge,
        M5_12XLarge,
        M5_16XLarge,
        M5_24XLarge,
        M5a_Large,
        M5a_XLarge,
        M5a_2XLarge,
        M5a_4XLarge,
        M5a_8XLarge,
        M5a_12XLarge,
        M5a_16XLarge,
        M5a_24XLarge
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

pattern T2_Micro :: EC2InstanceType
pattern T2_Micro = EC2InstanceType' "t2.micro"

pattern T2_Small :: EC2InstanceType
pattern T2_Small = EC2InstanceType' "t2.small"

pattern T2_Medium :: EC2InstanceType
pattern T2_Medium = EC2InstanceType' "t2.medium"

pattern T2_Large :: EC2InstanceType
pattern T2_Large = EC2InstanceType' "t2.large"

pattern C3_Large :: EC2InstanceType
pattern C3_Large = EC2InstanceType' "c3.large"

pattern C3_XLarge :: EC2InstanceType
pattern C3_XLarge = EC2InstanceType' "c3.xlarge"

pattern C3_2XLarge :: EC2InstanceType
pattern C3_2XLarge = EC2InstanceType' "c3.2xlarge"

pattern C3_4XLarge :: EC2InstanceType
pattern C3_4XLarge = EC2InstanceType' "c3.4xlarge"

pattern C3_8XLarge :: EC2InstanceType
pattern C3_8XLarge = EC2InstanceType' "c3.8xlarge"

pattern C4_Large :: EC2InstanceType
pattern C4_Large = EC2InstanceType' "c4.large"

pattern C4_XLarge :: EC2InstanceType
pattern C4_XLarge = EC2InstanceType' "c4.xlarge"

pattern C4_2XLarge :: EC2InstanceType
pattern C4_2XLarge = EC2InstanceType' "c4.2xlarge"

pattern C4_4XLarge :: EC2InstanceType
pattern C4_4XLarge = EC2InstanceType' "c4.4xlarge"

pattern C4_8XLarge :: EC2InstanceType
pattern C4_8XLarge = EC2InstanceType' "c4.8xlarge"

pattern C5_Large :: EC2InstanceType
pattern C5_Large = EC2InstanceType' "c5.large"

pattern C5_XLarge :: EC2InstanceType
pattern C5_XLarge = EC2InstanceType' "c5.xlarge"

pattern C5_2XLarge :: EC2InstanceType
pattern C5_2XLarge = EC2InstanceType' "c5.2xlarge"

pattern C5_4XLarge :: EC2InstanceType
pattern C5_4XLarge = EC2InstanceType' "c5.4xlarge"

pattern C5_9XLarge :: EC2InstanceType
pattern C5_9XLarge = EC2InstanceType' "c5.9xlarge"

pattern C5_12XLarge :: EC2InstanceType
pattern C5_12XLarge = EC2InstanceType' "c5.12xlarge"

pattern C5_18XLarge :: EC2InstanceType
pattern C5_18XLarge = EC2InstanceType' "c5.18xlarge"

pattern C5_24XLarge :: EC2InstanceType
pattern C5_24XLarge = EC2InstanceType' "c5.24xlarge"

pattern C5a_Large :: EC2InstanceType
pattern C5a_Large = EC2InstanceType' "c5a.large"

pattern C5a_XLarge :: EC2InstanceType
pattern C5a_XLarge = EC2InstanceType' "c5a.xlarge"

pattern C5a_2XLarge :: EC2InstanceType
pattern C5a_2XLarge = EC2InstanceType' "c5a.2xlarge"

pattern C5a_4XLarge :: EC2InstanceType
pattern C5a_4XLarge = EC2InstanceType' "c5a.4xlarge"

pattern C5a_8XLarge :: EC2InstanceType
pattern C5a_8XLarge = EC2InstanceType' "c5a.8xlarge"

pattern C5a_12XLarge :: EC2InstanceType
pattern C5a_12XLarge = EC2InstanceType' "c5a.12xlarge"

pattern C5a_16XLarge :: EC2InstanceType
pattern C5a_16XLarge = EC2InstanceType' "c5a.16xlarge"

pattern C5a_24XLarge :: EC2InstanceType
pattern C5a_24XLarge = EC2InstanceType' "c5a.24xlarge"

pattern R3_Large :: EC2InstanceType
pattern R3_Large = EC2InstanceType' "r3.large"

pattern R3_XLarge :: EC2InstanceType
pattern R3_XLarge = EC2InstanceType' "r3.xlarge"

pattern R3_2XLarge :: EC2InstanceType
pattern R3_2XLarge = EC2InstanceType' "r3.2xlarge"

pattern R3_4XLarge :: EC2InstanceType
pattern R3_4XLarge = EC2InstanceType' "r3.4xlarge"

pattern R3_8XLarge :: EC2InstanceType
pattern R3_8XLarge = EC2InstanceType' "r3.8xlarge"

pattern R4_Large :: EC2InstanceType
pattern R4_Large = EC2InstanceType' "r4.large"

pattern R4_XLarge :: EC2InstanceType
pattern R4_XLarge = EC2InstanceType' "r4.xlarge"

pattern R4_2XLarge :: EC2InstanceType
pattern R4_2XLarge = EC2InstanceType' "r4.2xlarge"

pattern R4_4XLarge :: EC2InstanceType
pattern R4_4XLarge = EC2InstanceType' "r4.4xlarge"

pattern R4_8XLarge :: EC2InstanceType
pattern R4_8XLarge = EC2InstanceType' "r4.8xlarge"

pattern R4_16XLarge :: EC2InstanceType
pattern R4_16XLarge = EC2InstanceType' "r4.16xlarge"

pattern R5_Large :: EC2InstanceType
pattern R5_Large = EC2InstanceType' "r5.large"

pattern R5_XLarge :: EC2InstanceType
pattern R5_XLarge = EC2InstanceType' "r5.xlarge"

pattern R5_2XLarge :: EC2InstanceType
pattern R5_2XLarge = EC2InstanceType' "r5.2xlarge"

pattern R5_4XLarge :: EC2InstanceType
pattern R5_4XLarge = EC2InstanceType' "r5.4xlarge"

pattern R5_8XLarge :: EC2InstanceType
pattern R5_8XLarge = EC2InstanceType' "r5.8xlarge"

pattern R5_12XLarge :: EC2InstanceType
pattern R5_12XLarge = EC2InstanceType' "r5.12xlarge"

pattern R5_16XLarge :: EC2InstanceType
pattern R5_16XLarge = EC2InstanceType' "r5.16xlarge"

pattern R5_24XLarge :: EC2InstanceType
pattern R5_24XLarge = EC2InstanceType' "r5.24xlarge"

pattern R5a_Large :: EC2InstanceType
pattern R5a_Large = EC2InstanceType' "r5a.large"

pattern R5a_XLarge :: EC2InstanceType
pattern R5a_XLarge = EC2InstanceType' "r5a.xlarge"

pattern R5a_2XLarge :: EC2InstanceType
pattern R5a_2XLarge = EC2InstanceType' "r5a.2xlarge"

pattern R5a_4XLarge :: EC2InstanceType
pattern R5a_4XLarge = EC2InstanceType' "r5a.4xlarge"

pattern R5a_8XLarge :: EC2InstanceType
pattern R5a_8XLarge = EC2InstanceType' "r5a.8xlarge"

pattern R5a_12XLarge :: EC2InstanceType
pattern R5a_12XLarge = EC2InstanceType' "r5a.12xlarge"

pattern R5a_16XLarge :: EC2InstanceType
pattern R5a_16XLarge = EC2InstanceType' "r5a.16xlarge"

pattern R5a_24XLarge :: EC2InstanceType
pattern R5a_24XLarge = EC2InstanceType' "r5a.24xlarge"

pattern M3_Medium :: EC2InstanceType
pattern M3_Medium = EC2InstanceType' "m3.medium"

pattern M3_Large :: EC2InstanceType
pattern M3_Large = EC2InstanceType' "m3.large"

pattern M3_XLarge :: EC2InstanceType
pattern M3_XLarge = EC2InstanceType' "m3.xlarge"

pattern M3_2XLarge :: EC2InstanceType
pattern M3_2XLarge = EC2InstanceType' "m3.2xlarge"

pattern M4_Large :: EC2InstanceType
pattern M4_Large = EC2InstanceType' "m4.large"

pattern M4_XLarge :: EC2InstanceType
pattern M4_XLarge = EC2InstanceType' "m4.xlarge"

pattern M4_2XLarge :: EC2InstanceType
pattern M4_2XLarge = EC2InstanceType' "m4.2xlarge"

pattern M4_4XLarge :: EC2InstanceType
pattern M4_4XLarge = EC2InstanceType' "m4.4xlarge"

pattern M4_10XLarge :: EC2InstanceType
pattern M4_10XLarge = EC2InstanceType' "m4.10xlarge"

pattern M5_Large :: EC2InstanceType
pattern M5_Large = EC2InstanceType' "m5.large"

pattern M5_XLarge :: EC2InstanceType
pattern M5_XLarge = EC2InstanceType' "m5.xlarge"

pattern M5_2XLarge :: EC2InstanceType
pattern M5_2XLarge = EC2InstanceType' "m5.2xlarge"

pattern M5_4XLarge :: EC2InstanceType
pattern M5_4XLarge = EC2InstanceType' "m5.4xlarge"

pattern M5_8XLarge :: EC2InstanceType
pattern M5_8XLarge = EC2InstanceType' "m5.8xlarge"

pattern M5_12XLarge :: EC2InstanceType
pattern M5_12XLarge = EC2InstanceType' "m5.12xlarge"

pattern M5_16XLarge :: EC2InstanceType
pattern M5_16XLarge = EC2InstanceType' "m5.16xlarge"

pattern M5_24XLarge :: EC2InstanceType
pattern M5_24XLarge = EC2InstanceType' "m5.24xlarge"

pattern M5a_Large :: EC2InstanceType
pattern M5a_Large = EC2InstanceType' "m5a.large"

pattern M5a_XLarge :: EC2InstanceType
pattern M5a_XLarge = EC2InstanceType' "m5a.xlarge"

pattern M5a_2XLarge :: EC2InstanceType
pattern M5a_2XLarge = EC2InstanceType' "m5a.2xlarge"

pattern M5a_4XLarge :: EC2InstanceType
pattern M5a_4XLarge = EC2InstanceType' "m5a.4xlarge"

pattern M5a_8XLarge :: EC2InstanceType
pattern M5a_8XLarge = EC2InstanceType' "m5a.8xlarge"

pattern M5a_12XLarge :: EC2InstanceType
pattern M5a_12XLarge = EC2InstanceType' "m5a.12xlarge"

pattern M5a_16XLarge :: EC2InstanceType
pattern M5a_16XLarge = EC2InstanceType' "m5a.16xlarge"

pattern M5a_24XLarge :: EC2InstanceType
pattern M5a_24XLarge = EC2InstanceType' "m5a.24xlarge"

{-# COMPLETE
  T2_Micro,
  T2_Small,
  T2_Medium,
  T2_Large,
  C3_Large,
  C3_XLarge,
  C3_2XLarge,
  C3_4XLarge,
  C3_8XLarge,
  C4_Large,
  C4_XLarge,
  C4_2XLarge,
  C4_4XLarge,
  C4_8XLarge,
  C5_Large,
  C5_XLarge,
  C5_2XLarge,
  C5_4XLarge,
  C5_9XLarge,
  C5_12XLarge,
  C5_18XLarge,
  C5_24XLarge,
  C5a_Large,
  C5a_XLarge,
  C5a_2XLarge,
  C5a_4XLarge,
  C5a_8XLarge,
  C5a_12XLarge,
  C5a_16XLarge,
  C5a_24XLarge,
  R3_Large,
  R3_XLarge,
  R3_2XLarge,
  R3_4XLarge,
  R3_8XLarge,
  R4_Large,
  R4_XLarge,
  R4_2XLarge,
  R4_4XLarge,
  R4_8XLarge,
  R4_16XLarge,
  R5_Large,
  R5_XLarge,
  R5_2XLarge,
  R5_4XLarge,
  R5_8XLarge,
  R5_12XLarge,
  R5_16XLarge,
  R5_24XLarge,
  R5a_Large,
  R5a_XLarge,
  R5a_2XLarge,
  R5a_4XLarge,
  R5a_8XLarge,
  R5a_12XLarge,
  R5a_16XLarge,
  R5a_24XLarge,
  M3_Medium,
  M3_Large,
  M3_XLarge,
  M3_2XLarge,
  M4_Large,
  M4_XLarge,
  M4_2XLarge,
  M4_4XLarge,
  M4_10XLarge,
  M5_Large,
  M5_XLarge,
  M5_2XLarge,
  M5_4XLarge,
  M5_8XLarge,
  M5_12XLarge,
  M5_16XLarge,
  M5_24XLarge,
  M5a_Large,
  M5a_XLarge,
  M5a_2XLarge,
  M5a_4XLarge,
  M5a_8XLarge,
  M5a_12XLarge,
  M5a_16XLarge,
  M5a_24XLarge,
  EC2InstanceType'
  #-}
