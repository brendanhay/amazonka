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
-- Module      : Amazonka.ResilienceHub.Types.AlarmType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AlarmType
  ( AlarmType
      ( ..,
        AlarmType_Canary,
        AlarmType_Composite,
        AlarmType_Event,
        AlarmType_Logs,
        AlarmType_Metric
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AlarmType = AlarmType'
  { fromAlarmType ::
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

pattern AlarmType_Canary :: AlarmType
pattern AlarmType_Canary = AlarmType' "Canary"

pattern AlarmType_Composite :: AlarmType
pattern AlarmType_Composite = AlarmType' "Composite"

pattern AlarmType_Event :: AlarmType
pattern AlarmType_Event = AlarmType' "Event"

pattern AlarmType_Logs :: AlarmType
pattern AlarmType_Logs = AlarmType' "Logs"

pattern AlarmType_Metric :: AlarmType
pattern AlarmType_Metric = AlarmType' "Metric"

{-# COMPLETE
  AlarmType_Canary,
  AlarmType_Composite,
  AlarmType_Event,
  AlarmType_Logs,
  AlarmType_Metric,
  AlarmType'
  #-}
