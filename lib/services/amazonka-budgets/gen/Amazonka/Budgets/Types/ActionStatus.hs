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
-- Module      : Amazonka.Budgets.Types.ActionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.ActionStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionStatus = ActionStatus'
  { fromActionStatus ::
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
