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
-- Module      : Network.AWS.IoTWireless.Types.BatteryLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTWireless.Types.BatteryLevel
  ( BatteryLevel
      ( ..,
        BatteryLevel_Critical,
        BatteryLevel_Low,
        BatteryLevel_Normal
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Sidewalk device battery level.
newtype BatteryLevel = BatteryLevel'
  { fromBatteryLevel ::
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

pattern BatteryLevel_Critical :: BatteryLevel
pattern BatteryLevel_Critical = BatteryLevel' "critical"

pattern BatteryLevel_Low :: BatteryLevel
pattern BatteryLevel_Low = BatteryLevel' "low"

pattern BatteryLevel_Normal :: BatteryLevel
pattern BatteryLevel_Normal = BatteryLevel' "normal"

{-# COMPLETE
  BatteryLevel_Critical,
  BatteryLevel_Low,
  BatteryLevel_Normal,
  BatteryLevel'
  #-}
