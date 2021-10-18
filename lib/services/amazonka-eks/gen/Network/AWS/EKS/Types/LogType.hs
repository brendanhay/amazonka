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
-- Module      : Network.AWS.EKS.Types.LogType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.LogType
  ( LogType
      ( ..,
        LogType_Api,
        LogType_Audit,
        LogType_Authenticator,
        LogType_ControllerManager,
        LogType_Scheduler
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype LogType = LogType' {fromLogType :: Core.Text}
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

pattern LogType_Api :: LogType
pattern LogType_Api = LogType' "api"

pattern LogType_Audit :: LogType
pattern LogType_Audit = LogType' "audit"

pattern LogType_Authenticator :: LogType
pattern LogType_Authenticator = LogType' "authenticator"

pattern LogType_ControllerManager :: LogType
pattern LogType_ControllerManager = LogType' "controllerManager"

pattern LogType_Scheduler :: LogType
pattern LogType_Scheduler = LogType' "scheduler"

{-# COMPLETE
  LogType_Api,
  LogType_Audit,
  LogType_Authenticator,
  LogType_ControllerManager,
  LogType_Scheduler,
  LogType'
  #-}
