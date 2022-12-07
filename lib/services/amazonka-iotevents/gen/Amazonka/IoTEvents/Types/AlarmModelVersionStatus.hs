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
-- Module      : Amazonka.IoTEvents.Types.AlarmModelVersionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.AlarmModelVersionStatus
  ( AlarmModelVersionStatus
      ( ..,
        AlarmModelVersionStatus_ACTIVATING,
        AlarmModelVersionStatus_ACTIVE,
        AlarmModelVersionStatus_FAILED,
        AlarmModelVersionStatus_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AlarmModelVersionStatus = AlarmModelVersionStatus'
  { fromAlarmModelVersionStatus ::
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
