{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.NotificationEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.NotificationEvent
  ( NotificationEvent
      ( NotificationEvent',
        NEAll,
        NECancelled,
        NEFailed,
        NEInProgress,
        NESuccess,
        NETimedOut
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype NotificationEvent = NotificationEvent' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern NEAll :: NotificationEvent
pattern NEAll = NotificationEvent' "All"

pattern NECancelled :: NotificationEvent
pattern NECancelled = NotificationEvent' "Cancelled"

pattern NEFailed :: NotificationEvent
pattern NEFailed = NotificationEvent' "Failed"

pattern NEInProgress :: NotificationEvent
pattern NEInProgress = NotificationEvent' "InProgress"

pattern NESuccess :: NotificationEvent
pattern NESuccess = NotificationEvent' "Success"

pattern NETimedOut :: NotificationEvent
pattern NETimedOut = NotificationEvent' "TimedOut"

{-# COMPLETE
  NEAll,
  NECancelled,
  NEFailed,
  NEInProgress,
  NESuccess,
  NETimedOut,
  NotificationEvent'
  #-}
