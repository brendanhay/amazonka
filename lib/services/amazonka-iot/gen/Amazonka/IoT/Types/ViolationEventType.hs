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
-- Module      : Amazonka.IoT.Types.ViolationEventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ViolationEventType
  ( ViolationEventType
      ( ..,
        ViolationEventType_Alarm_cleared,
        ViolationEventType_Alarm_invalidated,
        ViolationEventType_In_alarm
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ViolationEventType = ViolationEventType'
  { fromViolationEventType ::
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

pattern ViolationEventType_Alarm_cleared :: ViolationEventType
pattern ViolationEventType_Alarm_cleared = ViolationEventType' "alarm-cleared"

pattern ViolationEventType_Alarm_invalidated :: ViolationEventType
pattern ViolationEventType_Alarm_invalidated = ViolationEventType' "alarm-invalidated"

pattern ViolationEventType_In_alarm :: ViolationEventType
pattern ViolationEventType_In_alarm = ViolationEventType' "in-alarm"

{-# COMPLETE
  ViolationEventType_Alarm_cleared,
  ViolationEventType_Alarm_invalidated,
  ViolationEventType_In_alarm,
  ViolationEventType'
  #-}
