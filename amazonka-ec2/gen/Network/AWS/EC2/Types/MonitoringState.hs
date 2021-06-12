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
-- Module      : Network.AWS.EC2.Types.MonitoringState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.MonitoringState
  ( MonitoringState
      ( ..,
        MonitoringState_Disabled,
        MonitoringState_Disabling,
        MonitoringState_Enabled,
        MonitoringState_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype MonitoringState = MonitoringState'
  { fromMonitoringState ::
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

pattern MonitoringState_Disabled :: MonitoringState
pattern MonitoringState_Disabled = MonitoringState' "disabled"

pattern MonitoringState_Disabling :: MonitoringState
pattern MonitoringState_Disabling = MonitoringState' "disabling"

pattern MonitoringState_Enabled :: MonitoringState
pattern MonitoringState_Enabled = MonitoringState' "enabled"

pattern MonitoringState_Pending :: MonitoringState
pattern MonitoringState_Pending = MonitoringState' "pending"

{-# COMPLETE
  MonitoringState_Disabled,
  MonitoringState_Disabling,
  MonitoringState_Enabled,
  MonitoringState_Pending,
  MonitoringState'
  #-}
