{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype ViolationEventType = ViolationEventType'
  { fromViolationEventType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
