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
-- Module      : Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.EndOfMeetingReminderType
  ( EndOfMeetingReminderType
      ( ..,
        EndOfMeetingReminderType_ANNOUNCEMENT_TIME_CHECK,
        EndOfMeetingReminderType_ANNOUNCEMENT_VARIABLE_TIME_LEFT,
        EndOfMeetingReminderType_CHIME,
        EndOfMeetingReminderType_KNOCK
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype EndOfMeetingReminderType = EndOfMeetingReminderType'
  { fromEndOfMeetingReminderType ::
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
