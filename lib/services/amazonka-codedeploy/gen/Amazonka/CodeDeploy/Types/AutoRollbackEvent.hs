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
-- Module      : Amazonka.CodeDeploy.Types.AutoRollbackEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.AutoRollbackEvent
  ( AutoRollbackEvent
      ( ..,
        AutoRollbackEvent_DEPLOYMENT_FAILURE,
        AutoRollbackEvent_DEPLOYMENT_STOP_ON_ALARM,
        AutoRollbackEvent_DEPLOYMENT_STOP_ON_REQUEST
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AutoRollbackEvent = AutoRollbackEvent'
  { fromAutoRollbackEvent ::
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
