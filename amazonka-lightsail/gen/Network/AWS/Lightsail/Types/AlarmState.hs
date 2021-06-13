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
-- Module      : Network.AWS.Lightsail.Types.AlarmState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AlarmState
  ( AlarmState
      ( ..,
        AlarmState_ALARM,
        AlarmState_INSUFFICIENT_DATA,
        AlarmState_OK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AlarmState = AlarmState'
  { fromAlarmState ::
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

pattern AlarmState_ALARM :: AlarmState
pattern AlarmState_ALARM = AlarmState' "ALARM"

pattern AlarmState_INSUFFICIENT_DATA :: AlarmState
pattern AlarmState_INSUFFICIENT_DATA = AlarmState' "INSUFFICIENT_DATA"

pattern AlarmState_OK :: AlarmState
pattern AlarmState_OK = AlarmState' "OK"

{-# COMPLETE
  AlarmState_ALARM,
  AlarmState_INSUFFICIENT_DATA,
  AlarmState_OK,
  AlarmState'
  #-}
