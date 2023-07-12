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
-- Module      : Amazonka.SSM.Types.NotificationEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.NotificationEvent
  ( NotificationEvent
      ( ..,
        NotificationEvent_All,
        NotificationEvent_Cancelled,
        NotificationEvent_Failed,
        NotificationEvent_InProgress,
        NotificationEvent_Success,
        NotificationEvent_TimedOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotificationEvent = NotificationEvent'
  { fromNotificationEvent ::
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

pattern NotificationEvent_All :: NotificationEvent
pattern NotificationEvent_All = NotificationEvent' "All"

pattern NotificationEvent_Cancelled :: NotificationEvent
pattern NotificationEvent_Cancelled = NotificationEvent' "Cancelled"

pattern NotificationEvent_Failed :: NotificationEvent
pattern NotificationEvent_Failed = NotificationEvent' "Failed"

pattern NotificationEvent_InProgress :: NotificationEvent
pattern NotificationEvent_InProgress = NotificationEvent' "InProgress"

pattern NotificationEvent_Success :: NotificationEvent
pattern NotificationEvent_Success = NotificationEvent' "Success"

pattern NotificationEvent_TimedOut :: NotificationEvent
pattern NotificationEvent_TimedOut = NotificationEvent' "TimedOut"

{-# COMPLETE
  NotificationEvent_All,
  NotificationEvent_Cancelled,
  NotificationEvent_Failed,
  NotificationEvent_InProgress,
  NotificationEvent_Success,
  NotificationEvent_TimedOut,
  NotificationEvent'
  #-}
