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

import qualified Network.AWS.Prelude as Prelude

newtype ActionStatus = ActionStatus'
  { fromActionStatus ::
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
