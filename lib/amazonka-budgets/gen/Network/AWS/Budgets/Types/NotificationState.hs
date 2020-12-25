{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.NotificationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.NotificationState
  ( NotificationState
      ( NotificationState',
        NotificationStateOK,
        NotificationStateAlarm,
        fromNotificationState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype NotificationState = NotificationState'
  { fromNotificationState ::
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

pattern NotificationStateOK :: NotificationState
pattern NotificationStateOK = NotificationState' "OK"

pattern NotificationStateAlarm :: NotificationState
pattern NotificationStateAlarm = NotificationState' "ALARM"

{-# COMPLETE
  NotificationStateOK,
  NotificationStateAlarm,
  NotificationState'
  #-}
