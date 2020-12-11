-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupInstanceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupInstanceType
  ( GameServerGroupInstanceType
      ( GameServerGroupInstanceType',
        C4_2XLarge,
        C4_4XLarge,
        C4_8XLarge,
        C4_Large,
        C4_XLarge,
        C5_12XLarge,
        C5_18XLarge,
        C5_24XLarge,
        C5_2XLarge,
        C5_4XLarge,
        C5_9XLarge,
        C5_Large,
        C5_XLarge,
        M4_10XLarge,
        M4_2XLarge,
        M4_4XLarge,
        M4_Large,
        M4_XLarge,
        M5_12XLarge,
        M5_16XLarge,
        M5_24XLarge,
        M5_2XLarge,
        M5_4XLarge,
        M5_8XLarge,
        M5_Large,
        M5_XLarge,
        R4_16XLarge,
        R4_2XLarge,
        R4_4XLarge,
        R4_8XLarge,
        R4_Large,
        R4_XLarge,
        R5_12XLarge,
        R5_16XLarge,
        R5_24XLarge,
        R5_2XLarge,
        R5_4XLarge,
        R5_8XLarge,
        R5_Large,
        R5_XLarge
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GameServerGroupInstanceType = GameServerGroupInstanceType' Lude.Text
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

pattern C4_2XLarge :: GameServerGroupInstanceType
pattern C4_2XLarge = GameServerGroupInstanceType' "c4.2xlarge"

pattern C4_4XLarge :: GameServerGroupInstanceType
pattern C4_4XLarge = GameServerGroupInstanceType' "c4.4xlarge"

pattern C4_8XLarge :: GameServerGroupInstanceType
pattern C4_8XLarge = GameServerGroupInstanceType' "c4.8xlarge"

pattern C4_Large :: GameServerGroupInstanceType
pattern C4_Large = GameServerGroupInstanceType' "c4.large"

pattern C4_XLarge :: GameServerGroupInstanceType
pattern C4_XLarge = GameServerGroupInstanceType' "c4.xlarge"

pattern C5_12XLarge :: GameServerGroupInstanceType
pattern C5_12XLarge = GameServerGroupInstanceType' "c5.12xlarge"

pattern C5_18XLarge :: GameServerGroupInstanceType
pattern C5_18XLarge = GameServerGroupInstanceType' "c5.18xlarge"

pattern C5_24XLarge :: GameServerGroupInstanceType
pattern C5_24XLarge = GameServerGroupInstanceType' "c5.24xlarge"

pattern C5_2XLarge :: GameServerGroupInstanceType
pattern C5_2XLarge = GameServerGroupInstanceType' "c5.2xlarge"

pattern C5_4XLarge :: GameServerGroupInstanceType
pattern C5_4XLarge = GameServerGroupInstanceType' "c5.4xlarge"

pattern C5_9XLarge :: GameServerGroupInstanceType
pattern C5_9XLarge = GameServerGroupInstanceType' "c5.9xlarge"

pattern C5_Large :: GameServerGroupInstanceType
pattern C5_Large = GameServerGroupInstanceType' "c5.large"

pattern C5_XLarge :: GameServerGroupInstanceType
pattern C5_XLarge = GameServerGroupInstanceType' "c5.xlarge"

pattern M4_10XLarge :: GameServerGroupInstanceType
pattern M4_10XLarge = GameServerGroupInstanceType' "m4.10xlarge"

pattern M4_2XLarge :: GameServerGroupInstanceType
pattern M4_2XLarge = GameServerGroupInstanceType' "m4.2xlarge"

pattern M4_4XLarge :: GameServerGroupInstanceType
pattern M4_4XLarge = GameServerGroupInstanceType' "m4.4xlarge"

pattern M4_Large :: GameServerGroupInstanceType
pattern M4_Large = GameServerGroupInstanceType' "m4.large"

pattern M4_XLarge :: GameServerGroupInstanceType
pattern M4_XLarge = GameServerGroupInstanceType' "m4.xlarge"

pattern M5_12XLarge :: GameServerGroupInstanceType
pattern M5_12XLarge = GameServerGroupInstanceType' "m5.12xlarge"

pattern M5_16XLarge :: GameServerGroupInstanceType
pattern M5_16XLarge = GameServerGroupInstanceType' "m5.16xlarge"

pattern M5_24XLarge :: GameServerGroupInstanceType
pattern M5_24XLarge = GameServerGroupInstanceType' "m5.24xlarge"

pattern M5_2XLarge :: GameServerGroupInstanceType
pattern M5_2XLarge = GameServerGroupInstanceType' "m5.2xlarge"

pattern M5_4XLarge :: GameServerGroupInstanceType
pattern M5_4XLarge = GameServerGroupInstanceType' "m5.4xlarge"

pattern M5_8XLarge :: GameServerGroupInstanceType
pattern M5_8XLarge = GameServerGroupInstanceType' "m5.8xlarge"

pattern M5_Large :: GameServerGroupInstanceType
pattern M5_Large = GameServerGroupInstanceType' "m5.large"

pattern M5_XLarge :: GameServerGroupInstanceType
pattern M5_XLarge = GameServerGroupInstanceType' "m5.xlarge"

pattern R4_16XLarge :: GameServerGroupInstanceType
pattern R4_16XLarge = GameServerGroupInstanceType' "r4.16xlarge"

pattern R4_2XLarge :: GameServerGroupInstanceType
pattern R4_2XLarge = GameServerGroupInstanceType' "r4.2xlarge"

pattern R4_4XLarge :: GameServerGroupInstanceType
pattern R4_4XLarge = GameServerGroupInstanceType' "r4.4xlarge"

pattern R4_8XLarge :: GameServerGroupInstanceType
pattern R4_8XLarge = GameServerGroupInstanceType' "r4.8xlarge"

pattern R4_Large :: GameServerGroupInstanceType
pattern R4_Large = GameServerGroupInstanceType' "r4.large"

pattern R4_XLarge :: GameServerGroupInstanceType
pattern R4_XLarge = GameServerGroupInstanceType' "r4.xlarge"

pattern R5_12XLarge :: GameServerGroupInstanceType
pattern R5_12XLarge = GameServerGroupInstanceType' "r5.12xlarge"

pattern R5_16XLarge :: GameServerGroupInstanceType
pattern R5_16XLarge = GameServerGroupInstanceType' "r5.16xlarge"

pattern R5_24XLarge :: GameServerGroupInstanceType
pattern R5_24XLarge = GameServerGroupInstanceType' "r5.24xlarge"

pattern R5_2XLarge :: GameServerGroupInstanceType
pattern R5_2XLarge = GameServerGroupInstanceType' "r5.2xlarge"

pattern R5_4XLarge :: GameServerGroupInstanceType
pattern R5_4XLarge = GameServerGroupInstanceType' "r5.4xlarge"

pattern R5_8XLarge :: GameServerGroupInstanceType
pattern R5_8XLarge = GameServerGroupInstanceType' "r5.8xlarge"

pattern R5_Large :: GameServerGroupInstanceType
pattern R5_Large = GameServerGroupInstanceType' "r5.large"

pattern R5_XLarge :: GameServerGroupInstanceType
pattern R5_XLarge = GameServerGroupInstanceType' "r5.xlarge"

{-# COMPLETE
  C4_2XLarge,
  C4_4XLarge,
  C4_8XLarge,
  C4_Large,
  C4_XLarge,
  C5_12XLarge,
  C5_18XLarge,
  C5_24XLarge,
  C5_2XLarge,
  C5_4XLarge,
  C5_9XLarge,
  C5_Large,
  C5_XLarge,
  M4_10XLarge,
  M4_2XLarge,
  M4_4XLarge,
  M4_Large,
  M4_XLarge,
  M5_12XLarge,
  M5_16XLarge,
  M5_24XLarge,
  M5_2XLarge,
  M5_4XLarge,
  M5_8XLarge,
  M5_Large,
  M5_XLarge,
  R4_16XLarge,
  R4_2XLarge,
  R4_4XLarge,
  R4_8XLarge,
  R4_Large,
  R4_XLarge,
  R5_12XLarge,
  R5_16XLarge,
  R5_24XLarge,
  R5_2XLarge,
  R5_4XLarge,
  R5_8XLarge,
  R5_Large,
  R5_XLarge,
  GameServerGroupInstanceType'
  #-}
