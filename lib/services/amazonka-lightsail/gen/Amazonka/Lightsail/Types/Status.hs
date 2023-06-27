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
-- Module      : Amazonka.Lightsail.Types.Status
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Status
  ( Status
      ( ..,
        Status_FailedInstanceCreation,
        Status_FailedStartingGUISession,
        Status_FailedStoppingGUISession,
        Status_NotStarted,
        Status_SettingUpInstance,
        Status_StartExpired,
        Status_Started,
        Status_Starting,
        Status_Stopped,
        Status_Stopping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Status = Status' {fromStatus :: Data.Text}
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

pattern Status_FailedInstanceCreation :: Status
pattern Status_FailedInstanceCreation = Status' "failedInstanceCreation"

pattern Status_FailedStartingGUISession :: Status
pattern Status_FailedStartingGUISession = Status' "failedStartingGUISession"

pattern Status_FailedStoppingGUISession :: Status
pattern Status_FailedStoppingGUISession = Status' "failedStoppingGUISession"

pattern Status_NotStarted :: Status
pattern Status_NotStarted = Status' "notStarted"

pattern Status_SettingUpInstance :: Status
pattern Status_SettingUpInstance = Status' "settingUpInstance"

pattern Status_StartExpired :: Status
pattern Status_StartExpired = Status' "startExpired"

pattern Status_Started :: Status
pattern Status_Started = Status' "started"

pattern Status_Starting :: Status
pattern Status_Starting = Status' "starting"

pattern Status_Stopped :: Status
pattern Status_Stopped = Status' "stopped"

pattern Status_Stopping :: Status
pattern Status_Stopping = Status' "stopping"

{-# COMPLETE
  Status_FailedInstanceCreation,
  Status_FailedStartingGUISession,
  Status_FailedStoppingGUISession,
  Status_NotStarted,
  Status_SettingUpInstance,
  Status_StartExpired,
  Status_Started,
  Status_Starting,
  Status_Stopped,
  Status_Stopping,
  Status'
  #-}
