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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlarmType = AlarmType'
  { fromAlarmType ::
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
