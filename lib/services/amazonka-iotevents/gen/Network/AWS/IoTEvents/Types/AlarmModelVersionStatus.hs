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
-- Module      : Network.AWS.IoTEvents.Types.AlarmModelVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.AlarmModelVersionStatus
  ( AlarmModelVersionStatus
      ( ..,
        AlarmModelVersionStatus_ACTIVATING,
        AlarmModelVersionStatus_ACTIVE,
        AlarmModelVersionStatus_FAILED,
        AlarmModelVersionStatus_INACTIVE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AlarmModelVersionStatus = AlarmModelVersionStatus'
  { fromAlarmModelVersionStatus ::
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

pattern AlarmModelVersionStatus_ACTIVATING :: AlarmModelVersionStatus
pattern AlarmModelVersionStatus_ACTIVATING = AlarmModelVersionStatus' "ACTIVATING"

pattern AlarmModelVersionStatus_ACTIVE :: AlarmModelVersionStatus
pattern AlarmModelVersionStatus_ACTIVE = AlarmModelVersionStatus' "ACTIVE"

pattern AlarmModelVersionStatus_FAILED :: AlarmModelVersionStatus
pattern AlarmModelVersionStatus_FAILED = AlarmModelVersionStatus' "FAILED"

pattern AlarmModelVersionStatus_INACTIVE :: AlarmModelVersionStatus
pattern AlarmModelVersionStatus_INACTIVE = AlarmModelVersionStatus' "INACTIVE"

{-# COMPLETE
  AlarmModelVersionStatus_ACTIVATING,
  AlarmModelVersionStatus_ACTIVE,
  AlarmModelVersionStatus_FAILED,
  AlarmModelVersionStatus_INACTIVE,
  AlarmModelVersionStatus'
  #-}
