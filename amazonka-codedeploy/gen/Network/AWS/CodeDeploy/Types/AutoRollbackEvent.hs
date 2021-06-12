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
-- Module      : Network.AWS.CodeDeploy.Types.AutoRollbackEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AutoRollbackEvent
  ( AutoRollbackEvent
      ( ..,
        AutoRollbackEvent_DEPLOYMENT_FAILURE,
        AutoRollbackEvent_DEPLOYMENT_STOP_ON_ALARM,
        AutoRollbackEvent_DEPLOYMENT_STOP_ON_REQUEST
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AutoRollbackEvent = AutoRollbackEvent'
  { fromAutoRollbackEvent ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern AutoRollbackEvent_DEPLOYMENT_FAILURE :: AutoRollbackEvent
pattern AutoRollbackEvent_DEPLOYMENT_FAILURE = AutoRollbackEvent' "DEPLOYMENT_FAILURE"

pattern AutoRollbackEvent_DEPLOYMENT_STOP_ON_ALARM :: AutoRollbackEvent
pattern AutoRollbackEvent_DEPLOYMENT_STOP_ON_ALARM = AutoRollbackEvent' "DEPLOYMENT_STOP_ON_ALARM"

pattern AutoRollbackEvent_DEPLOYMENT_STOP_ON_REQUEST :: AutoRollbackEvent
pattern AutoRollbackEvent_DEPLOYMENT_STOP_ON_REQUEST = AutoRollbackEvent' "DEPLOYMENT_STOP_ON_REQUEST"

{-# COMPLETE
  AutoRollbackEvent_DEPLOYMENT_FAILURE,
  AutoRollbackEvent_DEPLOYMENT_STOP_ON_ALARM,
  AutoRollbackEvent_DEPLOYMENT_STOP_ON_REQUEST,
  AutoRollbackEvent'
  #-}
