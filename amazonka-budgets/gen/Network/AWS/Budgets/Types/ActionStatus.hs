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
-- Module      : Network.AWS.Budgets.Types.ActionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionStatus
  ( ActionStatus
      ( ..,
        ActionStatus_EXECUTION_FAILURE,
        ActionStatus_EXECUTION_IN_PROGRESS,
        ActionStatus_EXECUTION_SUCCESS,
        ActionStatus_PENDING,
        ActionStatus_RESET_FAILURE,
        ActionStatus_RESET_IN_PROGRESS,
        ActionStatus_REVERSE_FAILURE,
        ActionStatus_REVERSE_IN_PROGRESS,
        ActionStatus_REVERSE_SUCCESS,
        ActionStatus_STANDBY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionStatus = ActionStatus'
  { fromActionStatus ::
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

pattern ActionStatus_EXECUTION_FAILURE :: ActionStatus
pattern ActionStatus_EXECUTION_FAILURE = ActionStatus' "EXECUTION_FAILURE"

pattern ActionStatus_EXECUTION_IN_PROGRESS :: ActionStatus
pattern ActionStatus_EXECUTION_IN_PROGRESS = ActionStatus' "EXECUTION_IN_PROGRESS"

pattern ActionStatus_EXECUTION_SUCCESS :: ActionStatus
pattern ActionStatus_EXECUTION_SUCCESS = ActionStatus' "EXECUTION_SUCCESS"

pattern ActionStatus_PENDING :: ActionStatus
pattern ActionStatus_PENDING = ActionStatus' "PENDING"

pattern ActionStatus_RESET_FAILURE :: ActionStatus
pattern ActionStatus_RESET_FAILURE = ActionStatus' "RESET_FAILURE"

pattern ActionStatus_RESET_IN_PROGRESS :: ActionStatus
pattern ActionStatus_RESET_IN_PROGRESS = ActionStatus' "RESET_IN_PROGRESS"

pattern ActionStatus_REVERSE_FAILURE :: ActionStatus
pattern ActionStatus_REVERSE_FAILURE = ActionStatus' "REVERSE_FAILURE"

pattern ActionStatus_REVERSE_IN_PROGRESS :: ActionStatus
pattern ActionStatus_REVERSE_IN_PROGRESS = ActionStatus' "REVERSE_IN_PROGRESS"

pattern ActionStatus_REVERSE_SUCCESS :: ActionStatus
pattern ActionStatus_REVERSE_SUCCESS = ActionStatus' "REVERSE_SUCCESS"

pattern ActionStatus_STANDBY :: ActionStatus
pattern ActionStatus_STANDBY = ActionStatus' "STANDBY"

{-# COMPLETE
  ActionStatus_EXECUTION_FAILURE,
  ActionStatus_EXECUTION_IN_PROGRESS,
  ActionStatus_EXECUTION_SUCCESS,
  ActionStatus_PENDING,
  ActionStatus_RESET_FAILURE,
  ActionStatus_RESET_IN_PROGRESS,
  ActionStatus_REVERSE_FAILURE,
  ActionStatus_REVERSE_IN_PROGRESS,
  ActionStatus_REVERSE_SUCCESS,
  ActionStatus_STANDBY,
  ActionStatus'
  #-}
