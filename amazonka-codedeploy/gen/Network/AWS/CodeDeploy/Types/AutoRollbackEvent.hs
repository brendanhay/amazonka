{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype AutoRollbackEvent = AutoRollbackEvent'
  { fromAutoRollbackEvent ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
