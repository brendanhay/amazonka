{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        GSGITC4_Large,
        GSGITC4_XLarge,
        GSGITC4_2XLarge,
        GSGITC4_4XLarge,
        GSGITC4_8XLarge,
        GSGITC5_Large,
        GSGITC5_XLarge,
        GSGITC5_2XLarge,
        GSGITC5_4XLarge,
        GSGITC5_9XLarge,
        GSGITC5_12XLarge,
        GSGITC5_18XLarge,
        GSGITC5_24XLarge,
        GSGITR4_Large,
        GSGITR4_XLarge,
        GSGITR4_2XLarge,
        GSGITR4_4XLarge,
        GSGITR4_8XLarge,
        GSGITR4_16XLarge,
        GSGITR5_Large,
        GSGITR5_XLarge,
        GSGITR5_2XLarge,
        GSGITR5_4XLarge,
        GSGITR5_8XLarge,
        GSGITR5_12XLarge,
        GSGITR5_16XLarge,
        GSGITR5_24XLarge,
        GSGITM4_Large,
        GSGITM4_XLarge,
        GSGITM4_2XLarge,
        GSGITM4_4XLarge,
        GSGITM4_10XLarge,
        GSGITM5_Large,
        GSGITM5_XLarge,
        GSGITM5_2XLarge,
        GSGITM5_4XLarge,
        GSGITM5_8XLarge,
        GSGITM5_12XLarge,
        GSGITM5_16XLarge,
        GSGITM5_24XLarge
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

pattern GSGITC4_Large :: GameServerGroupInstanceType
pattern GSGITC4_Large = GameServerGroupInstanceType' "c4.large"

pattern GSGITC4_XLarge :: GameServerGroupInstanceType
pattern GSGITC4_XLarge = GameServerGroupInstanceType' "c4.xlarge"

pattern GSGITC4_2XLarge :: GameServerGroupInstanceType
pattern GSGITC4_2XLarge = GameServerGroupInstanceType' "c4.2xlarge"

pattern GSGITC4_4XLarge :: GameServerGroupInstanceType
pattern GSGITC4_4XLarge = GameServerGroupInstanceType' "c4.4xlarge"

pattern GSGITC4_8XLarge :: GameServerGroupInstanceType
pattern GSGITC4_8XLarge = GameServerGroupInstanceType' "c4.8xlarge"

pattern GSGITC5_Large :: GameServerGroupInstanceType
pattern GSGITC5_Large = GameServerGroupInstanceType' "c5.large"

pattern GSGITC5_XLarge :: GameServerGroupInstanceType
pattern GSGITC5_XLarge = GameServerGroupInstanceType' "c5.xlarge"

pattern GSGITC5_2XLarge :: GameServerGroupInstanceType
pattern GSGITC5_2XLarge = GameServerGroupInstanceType' "c5.2xlarge"

pattern GSGITC5_4XLarge :: GameServerGroupInstanceType
pattern GSGITC5_4XLarge = GameServerGroupInstanceType' "c5.4xlarge"

pattern GSGITC5_9XLarge :: GameServerGroupInstanceType
pattern GSGITC5_9XLarge = GameServerGroupInstanceType' "c5.9xlarge"

pattern GSGITC5_12XLarge :: GameServerGroupInstanceType
pattern GSGITC5_12XLarge = GameServerGroupInstanceType' "c5.12xlarge"

pattern GSGITC5_18XLarge :: GameServerGroupInstanceType
pattern GSGITC5_18XLarge = GameServerGroupInstanceType' "c5.18xlarge"

pattern GSGITC5_24XLarge :: GameServerGroupInstanceType
pattern GSGITC5_24XLarge = GameServerGroupInstanceType' "c5.24xlarge"

pattern GSGITR4_Large :: GameServerGroupInstanceType
pattern GSGITR4_Large = GameServerGroupInstanceType' "r4.large"

pattern GSGITR4_XLarge :: GameServerGroupInstanceType
pattern GSGITR4_XLarge = GameServerGroupInstanceType' "r4.xlarge"

pattern GSGITR4_2XLarge :: GameServerGroupInstanceType
pattern GSGITR4_2XLarge = GameServerGroupInstanceType' "r4.2xlarge"

pattern GSGITR4_4XLarge :: GameServerGroupInstanceType
pattern GSGITR4_4XLarge = GameServerGroupInstanceType' "r4.4xlarge"

pattern GSGITR4_8XLarge :: GameServerGroupInstanceType
pattern GSGITR4_8XLarge = GameServerGroupInstanceType' "r4.8xlarge"

pattern GSGITR4_16XLarge :: GameServerGroupInstanceType
pattern GSGITR4_16XLarge = GameServerGroupInstanceType' "r4.16xlarge"

pattern GSGITR5_Large :: GameServerGroupInstanceType
pattern GSGITR5_Large = GameServerGroupInstanceType' "r5.large"

pattern GSGITR5_XLarge :: GameServerGroupInstanceType
pattern GSGITR5_XLarge = GameServerGroupInstanceType' "r5.xlarge"

pattern GSGITR5_2XLarge :: GameServerGroupInstanceType
pattern GSGITR5_2XLarge = GameServerGroupInstanceType' "r5.2xlarge"

pattern GSGITR5_4XLarge :: GameServerGroupInstanceType
pattern GSGITR5_4XLarge = GameServerGroupInstanceType' "r5.4xlarge"

pattern GSGITR5_8XLarge :: GameServerGroupInstanceType
pattern GSGITR5_8XLarge = GameServerGroupInstanceType' "r5.8xlarge"

pattern GSGITR5_12XLarge :: GameServerGroupInstanceType
pattern GSGITR5_12XLarge = GameServerGroupInstanceType' "r5.12xlarge"

pattern GSGITR5_16XLarge :: GameServerGroupInstanceType
pattern GSGITR5_16XLarge = GameServerGroupInstanceType' "r5.16xlarge"

pattern GSGITR5_24XLarge :: GameServerGroupInstanceType
pattern GSGITR5_24XLarge = GameServerGroupInstanceType' "r5.24xlarge"

pattern GSGITM4_Large :: GameServerGroupInstanceType
pattern GSGITM4_Large = GameServerGroupInstanceType' "m4.large"

pattern GSGITM4_XLarge :: GameServerGroupInstanceType
pattern GSGITM4_XLarge = GameServerGroupInstanceType' "m4.xlarge"

pattern GSGITM4_2XLarge :: GameServerGroupInstanceType
pattern GSGITM4_2XLarge = GameServerGroupInstanceType' "m4.2xlarge"

pattern GSGITM4_4XLarge :: GameServerGroupInstanceType
pattern GSGITM4_4XLarge = GameServerGroupInstanceType' "m4.4xlarge"

pattern GSGITM4_10XLarge :: GameServerGroupInstanceType
pattern GSGITM4_10XLarge = GameServerGroupInstanceType' "m4.10xlarge"

pattern GSGITM5_Large :: GameServerGroupInstanceType
pattern GSGITM5_Large = GameServerGroupInstanceType' "m5.large"

pattern GSGITM5_XLarge :: GameServerGroupInstanceType
pattern GSGITM5_XLarge = GameServerGroupInstanceType' "m5.xlarge"

pattern GSGITM5_2XLarge :: GameServerGroupInstanceType
pattern GSGITM5_2XLarge = GameServerGroupInstanceType' "m5.2xlarge"

pattern GSGITM5_4XLarge :: GameServerGroupInstanceType
pattern GSGITM5_4XLarge = GameServerGroupInstanceType' "m5.4xlarge"

pattern GSGITM5_8XLarge :: GameServerGroupInstanceType
pattern GSGITM5_8XLarge = GameServerGroupInstanceType' "m5.8xlarge"

pattern GSGITM5_12XLarge :: GameServerGroupInstanceType
pattern GSGITM5_12XLarge = GameServerGroupInstanceType' "m5.12xlarge"

pattern GSGITM5_16XLarge :: GameServerGroupInstanceType
pattern GSGITM5_16XLarge = GameServerGroupInstanceType' "m5.16xlarge"

pattern GSGITM5_24XLarge :: GameServerGroupInstanceType
pattern GSGITM5_24XLarge = GameServerGroupInstanceType' "m5.24xlarge"

{-# COMPLETE
  GSGITC4_Large,
  GSGITC4_XLarge,
  GSGITC4_2XLarge,
  GSGITC4_4XLarge,
  GSGITC4_8XLarge,
  GSGITC5_Large,
  GSGITC5_XLarge,
  GSGITC5_2XLarge,
  GSGITC5_4XLarge,
  GSGITC5_9XLarge,
  GSGITC5_12XLarge,
  GSGITC5_18XLarge,
  GSGITC5_24XLarge,
  GSGITR4_Large,
  GSGITR4_XLarge,
  GSGITR4_2XLarge,
  GSGITR4_4XLarge,
  GSGITR4_8XLarge,
  GSGITR4_16XLarge,
  GSGITR5_Large,
  GSGITR5_XLarge,
  GSGITR5_2XLarge,
  GSGITR5_4XLarge,
  GSGITR5_8XLarge,
  GSGITR5_12XLarge,
  GSGITR5_16XLarge,
  GSGITR5_24XLarge,
  GSGITM4_Large,
  GSGITM4_XLarge,
  GSGITM4_2XLarge,
  GSGITM4_4XLarge,
  GSGITM4_10XLarge,
  GSGITM5_Large,
  GSGITM5_XLarge,
  GSGITM5_2XLarge,
  GSGITM5_4XLarge,
  GSGITM5_8XLarge,
  GSGITM5_12XLarge,
  GSGITM5_16XLarge,
  GSGITM5_24XLarge,
  GameServerGroupInstanceType'
  #-}
