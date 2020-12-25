{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.CurrentMetricName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.CurrentMetricName
  ( CurrentMetricName
      ( CurrentMetricName',
        CurrentMetricNameAgentsOnline,
        CurrentMetricNameAgentsAvailable,
        CurrentMetricNameAgentsOnCall,
        CurrentMetricNameAgentsNonProductive,
        CurrentMetricNameAgentsAfterContactWork,
        CurrentMetricNameAgentsError,
        CurrentMetricNameAgentsStaffed,
        CurrentMetricNameContactsInQueue,
        CurrentMetricNameOldestContactAge,
        CurrentMetricNameContactsScheduled,
        CurrentMetricNameAgentsOnContact,
        CurrentMetricNameSlotsActive,
        CurrentMetricNameSlotsAvailable,
        fromCurrentMetricName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The current metric names.
newtype CurrentMetricName = CurrentMetricName'
  { fromCurrentMetricName ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern CurrentMetricNameAgentsOnline :: CurrentMetricName
pattern CurrentMetricNameAgentsOnline = CurrentMetricName' "AGENTS_ONLINE"

pattern CurrentMetricNameAgentsAvailable :: CurrentMetricName
pattern CurrentMetricNameAgentsAvailable = CurrentMetricName' "AGENTS_AVAILABLE"

pattern CurrentMetricNameAgentsOnCall :: CurrentMetricName
pattern CurrentMetricNameAgentsOnCall = CurrentMetricName' "AGENTS_ON_CALL"

pattern CurrentMetricNameAgentsNonProductive :: CurrentMetricName
pattern CurrentMetricNameAgentsNonProductive = CurrentMetricName' "AGENTS_NON_PRODUCTIVE"

pattern CurrentMetricNameAgentsAfterContactWork :: CurrentMetricName
pattern CurrentMetricNameAgentsAfterContactWork = CurrentMetricName' "AGENTS_AFTER_CONTACT_WORK"

pattern CurrentMetricNameAgentsError :: CurrentMetricName
pattern CurrentMetricNameAgentsError = CurrentMetricName' "AGENTS_ERROR"

pattern CurrentMetricNameAgentsStaffed :: CurrentMetricName
pattern CurrentMetricNameAgentsStaffed = CurrentMetricName' "AGENTS_STAFFED"

pattern CurrentMetricNameContactsInQueue :: CurrentMetricName
pattern CurrentMetricNameContactsInQueue = CurrentMetricName' "CONTACTS_IN_QUEUE"

pattern CurrentMetricNameOldestContactAge :: CurrentMetricName
pattern CurrentMetricNameOldestContactAge = CurrentMetricName' "OLDEST_CONTACT_AGE"

pattern CurrentMetricNameContactsScheduled :: CurrentMetricName
pattern CurrentMetricNameContactsScheduled = CurrentMetricName' "CONTACTS_SCHEDULED"

pattern CurrentMetricNameAgentsOnContact :: CurrentMetricName
pattern CurrentMetricNameAgentsOnContact = CurrentMetricName' "AGENTS_ON_CONTACT"

pattern CurrentMetricNameSlotsActive :: CurrentMetricName
pattern CurrentMetricNameSlotsActive = CurrentMetricName' "SLOTS_ACTIVE"

pattern CurrentMetricNameSlotsAvailable :: CurrentMetricName
pattern CurrentMetricNameSlotsAvailable = CurrentMetricName' "SLOTS_AVAILABLE"

{-# COMPLETE
  CurrentMetricNameAgentsOnline,
  CurrentMetricNameAgentsAvailable,
  CurrentMetricNameAgentsOnCall,
  CurrentMetricNameAgentsNonProductive,
  CurrentMetricNameAgentsAfterContactWork,
  CurrentMetricNameAgentsError,
  CurrentMetricNameAgentsStaffed,
  CurrentMetricNameContactsInQueue,
  CurrentMetricNameOldestContactAge,
  CurrentMetricNameContactsScheduled,
  CurrentMetricNameAgentsOnContact,
  CurrentMetricNameSlotsActive,
  CurrentMetricNameSlotsAvailable,
  CurrentMetricName'
  #-}
