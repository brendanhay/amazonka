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
-- Module      : Amazonka.AlexaBusiness.Types.EndOfMeetingReminderType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.EndOfMeetingReminderType
  ( EndOfMeetingReminderType
      ( ..,
        EndOfMeetingReminderType_ANNOUNCEMENT_TIME_CHECK,
        EndOfMeetingReminderType_ANNOUNCEMENT_VARIABLE_TIME_LEFT,
        EndOfMeetingReminderType_CHIME,
        EndOfMeetingReminderType_KNOCK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EndOfMeetingReminderType = EndOfMeetingReminderType'
  { fromEndOfMeetingReminderType ::
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

pattern EndOfMeetingReminderType_ANNOUNCEMENT_TIME_CHECK :: EndOfMeetingReminderType
pattern EndOfMeetingReminderType_ANNOUNCEMENT_TIME_CHECK = EndOfMeetingReminderType' "ANNOUNCEMENT_TIME_CHECK"

pattern EndOfMeetingReminderType_ANNOUNCEMENT_VARIABLE_TIME_LEFT :: EndOfMeetingReminderType
pattern EndOfMeetingReminderType_ANNOUNCEMENT_VARIABLE_TIME_LEFT = EndOfMeetingReminderType' "ANNOUNCEMENT_VARIABLE_TIME_LEFT"

pattern EndOfMeetingReminderType_CHIME :: EndOfMeetingReminderType
pattern EndOfMeetingReminderType_CHIME = EndOfMeetingReminderType' "CHIME"

pattern EndOfMeetingReminderType_KNOCK :: EndOfMeetingReminderType
pattern EndOfMeetingReminderType_KNOCK = EndOfMeetingReminderType' "KNOCK"

{-# COMPLETE
  EndOfMeetingReminderType_ANNOUNCEMENT_TIME_CHECK,
  EndOfMeetingReminderType_ANNOUNCEMENT_VARIABLE_TIME_LEFT,
  EndOfMeetingReminderType_CHIME,
  EndOfMeetingReminderType_KNOCK,
  EndOfMeetingReminderType'
  #-}
