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
-- Module      : Amazonka.IoTWireless.Types.BatteryLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.BatteryLevel
  ( BatteryLevel
      ( ..,
        BatteryLevel_Critical,
        BatteryLevel_Low,
        BatteryLevel_Normal
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk device battery level.
newtype BatteryLevel = BatteryLevel'
  { fromBatteryLevel ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
