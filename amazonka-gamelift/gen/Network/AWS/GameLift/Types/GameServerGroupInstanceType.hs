{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.GameServerGroupInstanceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.GameServerGroupInstanceType
  ( GameServerGroupInstanceType
      ( ..,
        GameServerGroupInstanceType_C4_2xlarge,
        GameServerGroupInstanceType_C4_4xlarge,
        GameServerGroupInstanceType_C4_8xlarge,
        GameServerGroupInstanceType_C4_large,
        GameServerGroupInstanceType_C4_xlarge,
        GameServerGroupInstanceType_C5_12xlarge,
        GameServerGroupInstanceType_C5_18xlarge,
        GameServerGroupInstanceType_C5_24xlarge,
        GameServerGroupInstanceType_C5_2xlarge,
        GameServerGroupInstanceType_C5_4xlarge,
        GameServerGroupInstanceType_C5_9xlarge,
        GameServerGroupInstanceType_C5_large,
        GameServerGroupInstanceType_C5_xlarge,
        GameServerGroupInstanceType_C5a_12xlarge,
        GameServerGroupInstanceType_C5a_16xlarge,
        GameServerGroupInstanceType_C5a_24xlarge,
        GameServerGroupInstanceType_C5a_2xlarge,
        GameServerGroupInstanceType_C5a_4xlarge,
        GameServerGroupInstanceType_C5a_8xlarge,
        GameServerGroupInstanceType_C5a_large,
        GameServerGroupInstanceType_C5a_xlarge,
        GameServerGroupInstanceType_M4_10xlarge,
        GameServerGroupInstanceType_M4_2xlarge,
        GameServerGroupInstanceType_M4_4xlarge,
        GameServerGroupInstanceType_M4_large,
        GameServerGroupInstanceType_M4_xlarge,
        GameServerGroupInstanceType_M5_12xlarge,
        GameServerGroupInstanceType_M5_16xlarge,
        GameServerGroupInstanceType_M5_24xlarge,
        GameServerGroupInstanceType_M5_2xlarge,
        GameServerGroupInstanceType_M5_4xlarge,
        GameServerGroupInstanceType_M5_8xlarge,
        GameServerGroupInstanceType_M5_large,
        GameServerGroupInstanceType_M5_xlarge,
        GameServerGroupInstanceType_M5a_12xlarge,
        GameServerGroupInstanceType_M5a_16xlarge,
        GameServerGroupInstanceType_M5a_24xlarge,
        GameServerGroupInstanceType_M5a_2xlarge,
        GameServerGroupInstanceType_M5a_4xlarge,
        GameServerGroupInstanceType_M5a_8xlarge,
        GameServerGroupInstanceType_M5a_large,
        GameServerGroupInstanceType_M5a_xlarge,
        GameServerGroupInstanceType_R4_16xlarge,
        GameServerGroupInstanceType_R4_2xlarge,
        GameServerGroupInstanceType_R4_4xlarge,
        GameServerGroupInstanceType_R4_8xlarge,
        GameServerGroupInstanceType_R4_large,
        GameServerGroupInstanceType_R4_xlarge,
        GameServerGroupInstanceType_R5_12xlarge,
        GameServerGroupInstanceType_R5_16xlarge,
        GameServerGroupInstanceType_R5_24xlarge,
        GameServerGroupInstanceType_R5_2xlarge,
        GameServerGroupInstanceType_R5_4xlarge,
        GameServerGroupInstanceType_R5_8xlarge,
        GameServerGroupInstanceType_R5_large,
        GameServerGroupInstanceType_R5_xlarge,
        GameServerGroupInstanceType_R5a_12xlarge,
        GameServerGroupInstanceType_R5a_16xlarge,
        GameServerGroupInstanceType_R5a_24xlarge,
        GameServerGroupInstanceType_R5a_2xlarge,
        GameServerGroupInstanceType_R5a_4xlarge,
        GameServerGroupInstanceType_R5a_8xlarge,
        GameServerGroupInstanceType_R5a_large,
        GameServerGroupInstanceType_R5a_xlarge
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype GameServerGroupInstanceType = GameServerGroupInstanceType'
  { fromGameServerGroupInstanceType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern GameServerGroupInstanceType_C4_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C4_2xlarge = GameServerGroupInstanceType' "c4.2xlarge"

pattern GameServerGroupInstanceType_C4_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C4_4xlarge = GameServerGroupInstanceType' "c4.4xlarge"

pattern GameServerGroupInstanceType_C4_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C4_8xlarge = GameServerGroupInstanceType' "c4.8xlarge"

pattern GameServerGroupInstanceType_C4_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C4_large = GameServerGroupInstanceType' "c4.large"

pattern GameServerGroupInstanceType_C4_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C4_xlarge = GameServerGroupInstanceType' "c4.xlarge"

pattern GameServerGroupInstanceType_C5_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_12xlarge = GameServerGroupInstanceType' "c5.12xlarge"

pattern GameServerGroupInstanceType_C5_18xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_18xlarge = GameServerGroupInstanceType' "c5.18xlarge"

pattern GameServerGroupInstanceType_C5_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_24xlarge = GameServerGroupInstanceType' "c5.24xlarge"

pattern GameServerGroupInstanceType_C5_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_2xlarge = GameServerGroupInstanceType' "c5.2xlarge"

pattern GameServerGroupInstanceType_C5_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_4xlarge = GameServerGroupInstanceType' "c5.4xlarge"

pattern GameServerGroupInstanceType_C5_9xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_9xlarge = GameServerGroupInstanceType' "c5.9xlarge"

pattern GameServerGroupInstanceType_C5_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_large = GameServerGroupInstanceType' "c5.large"

pattern GameServerGroupInstanceType_C5_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5_xlarge = GameServerGroupInstanceType' "c5.xlarge"

pattern GameServerGroupInstanceType_C5a_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_12xlarge = GameServerGroupInstanceType' "c5a.12xlarge"

pattern GameServerGroupInstanceType_C5a_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_16xlarge = GameServerGroupInstanceType' "c5a.16xlarge"

pattern GameServerGroupInstanceType_C5a_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_24xlarge = GameServerGroupInstanceType' "c5a.24xlarge"

pattern GameServerGroupInstanceType_C5a_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_2xlarge = GameServerGroupInstanceType' "c5a.2xlarge"

pattern GameServerGroupInstanceType_C5a_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_4xlarge = GameServerGroupInstanceType' "c5a.4xlarge"

pattern GameServerGroupInstanceType_C5a_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_8xlarge = GameServerGroupInstanceType' "c5a.8xlarge"

pattern GameServerGroupInstanceType_C5a_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_large = GameServerGroupInstanceType' "c5a.large"

pattern GameServerGroupInstanceType_C5a_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_C5a_xlarge = GameServerGroupInstanceType' "c5a.xlarge"

pattern GameServerGroupInstanceType_M4_10xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M4_10xlarge = GameServerGroupInstanceType' "m4.10xlarge"

pattern GameServerGroupInstanceType_M4_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M4_2xlarge = GameServerGroupInstanceType' "m4.2xlarge"

pattern GameServerGroupInstanceType_M4_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M4_4xlarge = GameServerGroupInstanceType' "m4.4xlarge"

pattern GameServerGroupInstanceType_M4_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M4_large = GameServerGroupInstanceType' "m4.large"

pattern GameServerGroupInstanceType_M4_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M4_xlarge = GameServerGroupInstanceType' "m4.xlarge"

pattern GameServerGroupInstanceType_M5_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_12xlarge = GameServerGroupInstanceType' "m5.12xlarge"

pattern GameServerGroupInstanceType_M5_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_16xlarge = GameServerGroupInstanceType' "m5.16xlarge"

pattern GameServerGroupInstanceType_M5_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_24xlarge = GameServerGroupInstanceType' "m5.24xlarge"

pattern GameServerGroupInstanceType_M5_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_2xlarge = GameServerGroupInstanceType' "m5.2xlarge"

pattern GameServerGroupInstanceType_M5_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_4xlarge = GameServerGroupInstanceType' "m5.4xlarge"

pattern GameServerGroupInstanceType_M5_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_8xlarge = GameServerGroupInstanceType' "m5.8xlarge"

pattern GameServerGroupInstanceType_M5_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_large = GameServerGroupInstanceType' "m5.large"

pattern GameServerGroupInstanceType_M5_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5_xlarge = GameServerGroupInstanceType' "m5.xlarge"

pattern GameServerGroupInstanceType_M5a_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_12xlarge = GameServerGroupInstanceType' "m5a.12xlarge"

pattern GameServerGroupInstanceType_M5a_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_16xlarge = GameServerGroupInstanceType' "m5a.16xlarge"

pattern GameServerGroupInstanceType_M5a_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_24xlarge = GameServerGroupInstanceType' "m5a.24xlarge"

pattern GameServerGroupInstanceType_M5a_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_2xlarge = GameServerGroupInstanceType' "m5a.2xlarge"

pattern GameServerGroupInstanceType_M5a_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_4xlarge = GameServerGroupInstanceType' "m5a.4xlarge"

pattern GameServerGroupInstanceType_M5a_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_8xlarge = GameServerGroupInstanceType' "m5a.8xlarge"

pattern GameServerGroupInstanceType_M5a_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_large = GameServerGroupInstanceType' "m5a.large"

pattern GameServerGroupInstanceType_M5a_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_M5a_xlarge = GameServerGroupInstanceType' "m5a.xlarge"

pattern GameServerGroupInstanceType_R4_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R4_16xlarge = GameServerGroupInstanceType' "r4.16xlarge"

pattern GameServerGroupInstanceType_R4_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R4_2xlarge = GameServerGroupInstanceType' "r4.2xlarge"

pattern GameServerGroupInstanceType_R4_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R4_4xlarge = GameServerGroupInstanceType' "r4.4xlarge"

pattern GameServerGroupInstanceType_R4_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R4_8xlarge = GameServerGroupInstanceType' "r4.8xlarge"

pattern GameServerGroupInstanceType_R4_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R4_large = GameServerGroupInstanceType' "r4.large"

pattern GameServerGroupInstanceType_R4_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R4_xlarge = GameServerGroupInstanceType' "r4.xlarge"

pattern GameServerGroupInstanceType_R5_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_12xlarge = GameServerGroupInstanceType' "r5.12xlarge"

pattern GameServerGroupInstanceType_R5_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_16xlarge = GameServerGroupInstanceType' "r5.16xlarge"

pattern GameServerGroupInstanceType_R5_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_24xlarge = GameServerGroupInstanceType' "r5.24xlarge"

pattern GameServerGroupInstanceType_R5_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_2xlarge = GameServerGroupInstanceType' "r5.2xlarge"

pattern GameServerGroupInstanceType_R5_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_4xlarge = GameServerGroupInstanceType' "r5.4xlarge"

pattern GameServerGroupInstanceType_R5_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_8xlarge = GameServerGroupInstanceType' "r5.8xlarge"

pattern GameServerGroupInstanceType_R5_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_large = GameServerGroupInstanceType' "r5.large"

pattern GameServerGroupInstanceType_R5_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5_xlarge = GameServerGroupInstanceType' "r5.xlarge"

pattern GameServerGroupInstanceType_R5a_12xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_12xlarge = GameServerGroupInstanceType' "r5a.12xlarge"

pattern GameServerGroupInstanceType_R5a_16xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_16xlarge = GameServerGroupInstanceType' "r5a.16xlarge"

pattern GameServerGroupInstanceType_R5a_24xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_24xlarge = GameServerGroupInstanceType' "r5a.24xlarge"

pattern GameServerGroupInstanceType_R5a_2xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_2xlarge = GameServerGroupInstanceType' "r5a.2xlarge"

pattern GameServerGroupInstanceType_R5a_4xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_4xlarge = GameServerGroupInstanceType' "r5a.4xlarge"

pattern GameServerGroupInstanceType_R5a_8xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_8xlarge = GameServerGroupInstanceType' "r5a.8xlarge"

pattern GameServerGroupInstanceType_R5a_large :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_large = GameServerGroupInstanceType' "r5a.large"

pattern GameServerGroupInstanceType_R5a_xlarge :: GameServerGroupInstanceType
pattern GameServerGroupInstanceType_R5a_xlarge = GameServerGroupInstanceType' "r5a.xlarge"

{-# COMPLETE
  GameServerGroupInstanceType_C4_2xlarge,
  GameServerGroupInstanceType_C4_4xlarge,
  GameServerGroupInstanceType_C4_8xlarge,
  GameServerGroupInstanceType_C4_large,
  GameServerGroupInstanceType_C4_xlarge,
  GameServerGroupInstanceType_C5_12xlarge,
  GameServerGroupInstanceType_C5_18xlarge,
  GameServerGroupInstanceType_C5_24xlarge,
  GameServerGroupInstanceType_C5_2xlarge,
  GameServerGroupInstanceType_C5_4xlarge,
  GameServerGroupInstanceType_C5_9xlarge,
  GameServerGroupInstanceType_C5_large,
  GameServerGroupInstanceType_C5_xlarge,
  GameServerGroupInstanceType_C5a_12xlarge,
  GameServerGroupInstanceType_C5a_16xlarge,
  GameServerGroupInstanceType_C5a_24xlarge,
  GameServerGroupInstanceType_C5a_2xlarge,
  GameServerGroupInstanceType_C5a_4xlarge,
  GameServerGroupInstanceType_C5a_8xlarge,
  GameServerGroupInstanceType_C5a_large,
  GameServerGroupInstanceType_C5a_xlarge,
  GameServerGroupInstanceType_M4_10xlarge,
  GameServerGroupInstanceType_M4_2xlarge,
  GameServerGroupInstanceType_M4_4xlarge,
  GameServerGroupInstanceType_M4_large,
  GameServerGroupInstanceType_M4_xlarge,
  GameServerGroupInstanceType_M5_12xlarge,
  GameServerGroupInstanceType_M5_16xlarge,
  GameServerGroupInstanceType_M5_24xlarge,
  GameServerGroupInstanceType_M5_2xlarge,
  GameServerGroupInstanceType_M5_4xlarge,
  GameServerGroupInstanceType_M5_8xlarge,
  GameServerGroupInstanceType_M5_large,
  GameServerGroupInstanceType_M5_xlarge,
  GameServerGroupInstanceType_M5a_12xlarge,
  GameServerGroupInstanceType_M5a_16xlarge,
  GameServerGroupInstanceType_M5a_24xlarge,
  GameServerGroupInstanceType_M5a_2xlarge,
  GameServerGroupInstanceType_M5a_4xlarge,
  GameServerGroupInstanceType_M5a_8xlarge,
  GameServerGroupInstanceType_M5a_large,
  GameServerGroupInstanceType_M5a_xlarge,
  GameServerGroupInstanceType_R4_16xlarge,
  GameServerGroupInstanceType_R4_2xlarge,
  GameServerGroupInstanceType_R4_4xlarge,
  GameServerGroupInstanceType_R4_8xlarge,
  GameServerGroupInstanceType_R4_large,
  GameServerGroupInstanceType_R4_xlarge,
  GameServerGroupInstanceType_R5_12xlarge,
  GameServerGroupInstanceType_R5_16xlarge,
  GameServerGroupInstanceType_R5_24xlarge,
  GameServerGroupInstanceType_R5_2xlarge,
  GameServerGroupInstanceType_R5_4xlarge,
  GameServerGroupInstanceType_R5_8xlarge,
  GameServerGroupInstanceType_R5_large,
  GameServerGroupInstanceType_R5_xlarge,
  GameServerGroupInstanceType_R5a_12xlarge,
  GameServerGroupInstanceType_R5a_16xlarge,
  GameServerGroupInstanceType_R5a_24xlarge,
  GameServerGroupInstanceType_R5a_2xlarge,
  GameServerGroupInstanceType_R5a_4xlarge,
  GameServerGroupInstanceType_R5a_8xlarge,
  GameServerGroupInstanceType_R5a_large,
  GameServerGroupInstanceType_R5a_xlarge,
  GameServerGroupInstanceType'
  #-}
