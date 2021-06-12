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
-- Module      : Network.AWS.IoT.Types.ViolationEventType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ViolationEventType
  ( ViolationEventType
      ( ..,
        ViolationEventType_Alarm_cleared,
        ViolationEventType_Alarm_invalidated,
        ViolationEventType_In_alarm
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ViolationEventType = ViolationEventType'
  { fromViolationEventType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
