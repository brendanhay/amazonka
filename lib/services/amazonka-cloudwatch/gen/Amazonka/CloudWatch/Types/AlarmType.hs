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
-- Module      : Amazonka.CloudWatch.Types.AlarmType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.AlarmType
  ( AlarmType
      ( ..,
        AlarmType_CompositeAlarm,
        AlarmType_MetricAlarm
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

pattern AlarmType_CompositeAlarm :: AlarmType
pattern AlarmType_CompositeAlarm = AlarmType' "CompositeAlarm"

pattern AlarmType_MetricAlarm :: AlarmType
pattern AlarmType_MetricAlarm = AlarmType' "MetricAlarm"

{-# COMPLETE
  AlarmType_CompositeAlarm,
  AlarmType_MetricAlarm,
  AlarmType'
  #-}
