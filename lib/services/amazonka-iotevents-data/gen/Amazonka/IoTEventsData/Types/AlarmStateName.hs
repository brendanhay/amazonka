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
-- Module      : Amazonka.IoTEventsData.Types.AlarmStateName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.AlarmStateName
  ( AlarmStateName
      ( ..,
        AlarmStateName_ACKNOWLEDGED,
        AlarmStateName_ACTIVE,
        AlarmStateName_DISABLED,
        AlarmStateName_LATCHED,
        AlarmStateName_NORMAL,
        AlarmStateName_SNOOZE_DISABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlarmStateName = AlarmStateName'
  { fromAlarmStateName ::
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

pattern AlarmStateName_ACKNOWLEDGED :: AlarmStateName
pattern AlarmStateName_ACKNOWLEDGED = AlarmStateName' "ACKNOWLEDGED"

pattern AlarmStateName_ACTIVE :: AlarmStateName
pattern AlarmStateName_ACTIVE = AlarmStateName' "ACTIVE"

pattern AlarmStateName_DISABLED :: AlarmStateName
pattern AlarmStateName_DISABLED = AlarmStateName' "DISABLED"

pattern AlarmStateName_LATCHED :: AlarmStateName
pattern AlarmStateName_LATCHED = AlarmStateName' "LATCHED"

pattern AlarmStateName_NORMAL :: AlarmStateName
pattern AlarmStateName_NORMAL = AlarmStateName' "NORMAL"

pattern AlarmStateName_SNOOZE_DISABLED :: AlarmStateName
pattern AlarmStateName_SNOOZE_DISABLED = AlarmStateName' "SNOOZE_DISABLED"

{-# COMPLETE
  AlarmStateName_ACKNOWLEDGED,
  AlarmStateName_ACTIVE,
  AlarmStateName_DISABLED,
  AlarmStateName_LATCHED,
  AlarmStateName_NORMAL,
  AlarmStateName_SNOOZE_DISABLED,
  AlarmStateName'
  #-}
