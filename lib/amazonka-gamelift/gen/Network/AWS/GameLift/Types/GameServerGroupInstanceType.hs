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
        GameServerGroupInstanceTypeC4_Large,
        GameServerGroupInstanceTypeC4_Xlarge,
        GameServerGroupInstanceTypeC4_2xlarge,
        GameServerGroupInstanceTypeC4_4xlarge,
        GameServerGroupInstanceTypeC4_8xlarge,
        GameServerGroupInstanceTypeC5_Large,
        GameServerGroupInstanceTypeC5_Xlarge,
        GameServerGroupInstanceTypeC5_2xlarge,
        GameServerGroupInstanceTypeC5_4xlarge,
        GameServerGroupInstanceTypeC5_9xlarge,
        GameServerGroupInstanceTypeC5_12xlarge,
        GameServerGroupInstanceTypeC5_18xlarge,
        GameServerGroupInstanceTypeC5_24xlarge,
        GameServerGroupInstanceTypeR4_Large,
        GameServerGroupInstanceTypeR4_Xlarge,
        GameServerGroupInstanceTypeR4_2xlarge,
        GameServerGroupInstanceTypeR4_4xlarge,
        GameServerGroupInstanceTypeR4_8xlarge,
        GameServerGroupInstanceTypeR4_16xlarge,
        GameServerGroupInstanceTypeR5_Large,
        GameServerGroupInstanceTypeR5_Xlarge,
        GameServerGroupInstanceTypeR5_2xlarge,
        GameServerGroupInstanceTypeR5_4xlarge,
        GameServerGroupInstanceTypeR5_8xlarge,
        GameServerGroupInstanceTypeR5_12xlarge,
        GameServerGroupInstanceTypeR5_16xlarge,
        GameServerGroupInstanceTypeR5_24xlarge,
        GameServerGroupInstanceTypeM4_Large,
        GameServerGroupInstanceTypeM4_Xlarge,
        GameServerGroupInstanceTypeM4_2xlarge,
        GameServerGroupInstanceTypeM4_4xlarge,
        GameServerGroupInstanceTypeM4_10xlarge,
        GameServerGroupInstanceTypeM5_Large,
        GameServerGroupInstanceTypeM5_Xlarge,
        GameServerGroupInstanceTypeM5_2xlarge,
        GameServerGroupInstanceTypeM5_4xlarge,
        GameServerGroupInstanceTypeM5_8xlarge,
        GameServerGroupInstanceTypeM5_12xlarge,
        GameServerGroupInstanceTypeM5_16xlarge,
        GameServerGroupInstanceTypeM5_24xlarge,
        fromGameServerGroupInstanceType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype GameServerGroupInstanceType = GameServerGroupInstanceType'
  { fromGameServerGroupInstanceType ::
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

pattern GameServerGroupInstanceTypeC4_Large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC4_Large = GameServerGroupInstanceType' "c4.large"

pattern GameServerGroupInstanceTypeC4_Xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC4_Xlarge = GameServerGroupInstanceType' "c4.xlarge"

pattern GameServerGroupInstanceTypeC4_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC4_2xlarge = GameServerGroupInstanceType' "c4.2xlarge"

pattern GameServerGroupInstanceTypeC4_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC4_4xlarge = GameServerGroupInstanceType' "c4.4xlarge"

pattern GameServerGroupInstanceTypeC4_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC4_8xlarge = GameServerGroupInstanceType' "c4.8xlarge"

pattern GameServerGroupInstanceTypeC5_Large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_Large = GameServerGroupInstanceType' "c5.large"

pattern GameServerGroupInstanceTypeC5_Xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_Xlarge = GameServerGroupInstanceType' "c5.xlarge"

pattern GameServerGroupInstanceTypeC5_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_2xlarge = GameServerGroupInstanceType' "c5.2xlarge"

pattern GameServerGroupInstanceTypeC5_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_4xlarge = GameServerGroupInstanceType' "c5.4xlarge"

pattern GameServerGroupInstanceTypeC5_9xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_9xlarge = GameServerGroupInstanceType' "c5.9xlarge"

pattern GameServerGroupInstanceTypeC5_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_12xlarge = GameServerGroupInstanceType' "c5.12xlarge"

pattern GameServerGroupInstanceTypeC5_18xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_18xlarge = GameServerGroupInstanceType' "c5.18xlarge"

pattern GameServerGroupInstanceTypeC5_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeC5_24xlarge = GameServerGroupInstanceType' "c5.24xlarge"

pattern GameServerGroupInstanceTypeR4_Large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR4_Large = GameServerGroupInstanceType' "r4.large"

pattern GameServerGroupInstanceTypeR4_Xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR4_Xlarge = GameServerGroupInstanceType' "r4.xlarge"

pattern GameServerGroupInstanceTypeR4_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR4_2xlarge = GameServerGroupInstanceType' "r4.2xlarge"

pattern GameServerGroupInstanceTypeR4_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR4_4xlarge = GameServerGroupInstanceType' "r4.4xlarge"

pattern GameServerGroupInstanceTypeR4_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR4_8xlarge = GameServerGroupInstanceType' "r4.8xlarge"

pattern GameServerGroupInstanceTypeR4_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR4_16xlarge = GameServerGroupInstanceType' "r4.16xlarge"

pattern GameServerGroupInstanceTypeR5_Large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_Large = GameServerGroupInstanceType' "r5.large"

pattern GameServerGroupInstanceTypeR5_Xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_Xlarge = GameServerGroupInstanceType' "r5.xlarge"

pattern GameServerGroupInstanceTypeR5_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_2xlarge = GameServerGroupInstanceType' "r5.2xlarge"

pattern GameServerGroupInstanceTypeR5_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_4xlarge = GameServerGroupInstanceType' "r5.4xlarge"

pattern GameServerGroupInstanceTypeR5_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_8xlarge = GameServerGroupInstanceType' "r5.8xlarge"

pattern GameServerGroupInstanceTypeR5_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_12xlarge = GameServerGroupInstanceType' "r5.12xlarge"

pattern GameServerGroupInstanceTypeR5_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_16xlarge = GameServerGroupInstanceType' "r5.16xlarge"

pattern GameServerGroupInstanceTypeR5_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeR5_24xlarge = GameServerGroupInstanceType' "r5.24xlarge"

pattern GameServerGroupInstanceTypeM4_Large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM4_Large = GameServerGroupInstanceType' "m4.large"

pattern GameServerGroupInstanceTypeM4_Xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM4_Xlarge = GameServerGroupInstanceType' "m4.xlarge"

pattern GameServerGroupInstanceTypeM4_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM4_2xlarge = GameServerGroupInstanceType' "m4.2xlarge"

pattern GameServerGroupInstanceTypeM4_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM4_4xlarge = GameServerGroupInstanceType' "m4.4xlarge"

pattern GameServerGroupInstanceTypeM4_10xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM4_10xlarge = GameServerGroupInstanceType' "m4.10xlarge"

pattern GameServerGroupInstanceTypeM5_Large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_Large = GameServerGroupInstanceType' "m5.large"

pattern GameServerGroupInstanceTypeM5_Xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_Xlarge = GameServerGroupInstanceType' "m5.xlarge"

pattern GameServerGroupInstanceTypeM5_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_2xlarge = GameServerGroupInstanceType' "m5.2xlarge"

pattern GameServerGroupInstanceTypeM5_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_4xlarge = GameServerGroupInstanceType' "m5.4xlarge"

pattern GameServerGroupInstanceTypeM5_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_8xlarge = GameServerGroupInstanceType' "m5.8xlarge"

pattern GameServerGroupInstanceTypeM5_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_12xlarge = GameServerGroupInstanceType' "m5.12xlarge"

pattern GameServerGroupInstanceTypeM5_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_16xlarge = GameServerGroupInstanceType' "m5.16xlarge"

pattern GameServerGroupInstanceTypeM5_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceTypeM5_24xlarge = GameServerGroupInstanceType' "m5.24xlarge"

{-# COMPLETE
  GameServerGroupInstanceTypeC4_Large,
  GameServerGroupInstanceTypeC4_Xlarge,
  GameServerGroupInstanceTypeC4_2xlarge,
  GameServerGroupInstanceTypeC4_4xlarge,
  GameServerGroupInstanceTypeC4_8xlarge,
  GameServerGroupInstanceTypeC5_Large,
  GameServerGroupInstanceTypeC5_Xlarge,
  GameServerGroupInstanceTypeC5_2xlarge,
  GameServerGroupInstanceTypeC5_4xlarge,
  GameServerGroupInstanceTypeC5_9xlarge,
  GameServerGroupInstanceTypeC5_12xlarge,
  GameServerGroupInstanceTypeC5_18xlarge,
  GameServerGroupInstanceTypeC5_24xlarge,
  GameServerGroupInstanceTypeR4_Large,
  GameServerGroupInstanceTypeR4_Xlarge,
  GameServerGroupInstanceTypeR4_2xlarge,
  GameServerGroupInstanceTypeR4_4xlarge,
  GameServerGroupInstanceTypeR4_8xlarge,
  GameServerGroupInstanceTypeR4_16xlarge,
  GameServerGroupInstanceTypeR5_Large,
  GameServerGroupInstanceTypeR5_Xlarge,
  GameServerGroupInstanceTypeR5_2xlarge,
  GameServerGroupInstanceTypeR5_4xlarge,
  GameServerGroupInstanceTypeR5_8xlarge,
  GameServerGroupInstanceTypeR5_12xlarge,
  GameServerGroupInstanceTypeR5_16xlarge,
  GameServerGroupInstanceTypeR5_24xlarge,
  GameServerGroupInstanceTypeM4_Large,
  GameServerGroupInstanceTypeM4_Xlarge,
  GameServerGroupInstanceTypeM4_2xlarge,
  GameServerGroupInstanceTypeM4_4xlarge,
  GameServerGroupInstanceTypeM4_10xlarge,
  GameServerGroupInstanceTypeM5_Large,
  GameServerGroupInstanceTypeM5_Xlarge,
  GameServerGroupInstanceTypeM5_2xlarge,
  GameServerGroupInstanceTypeM5_4xlarge,
  GameServerGroupInstanceTypeM5_8xlarge,
  GameServerGroupInstanceTypeM5_12xlarge,
  GameServerGroupInstanceTypeM5_16xlarge,
  GameServerGroupInstanceTypeM5_24xlarge,
  GameServerGroupInstanceType'
  #-}
