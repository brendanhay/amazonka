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
-- Module      : Network.AWS.Budgets.Types.ExecutionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ExecutionType
  ( ExecutionType
      ( ..,
        ExecutionType_APPROVE_BUDGET_ACTION,
        ExecutionType_RESET_BUDGET_ACTION,
        ExecutionType_RETRY_BUDGET_ACTION,
        ExecutionType_REVERSE_BUDGET_ACTION
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ExecutionType = ExecutionType'
  { fromExecutionType ::
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

pattern ExecutionType_APPROVE_BUDGET_ACTION :: ExecutionType
pattern ExecutionType_APPROVE_BUDGET_ACTION = ExecutionType' "APPROVE_BUDGET_ACTION"

pattern ExecutionType_RESET_BUDGET_ACTION :: ExecutionType
pattern ExecutionType_RESET_BUDGET_ACTION = ExecutionType' "RESET_BUDGET_ACTION"

pattern ExecutionType_RETRY_BUDGET_ACTION :: ExecutionType
pattern ExecutionType_RETRY_BUDGET_ACTION = ExecutionType' "RETRY_BUDGET_ACTION"

pattern ExecutionType_REVERSE_BUDGET_ACTION :: ExecutionType
pattern ExecutionType_REVERSE_BUDGET_ACTION = ExecutionType' "REVERSE_BUDGET_ACTION"

{-# COMPLETE
  ExecutionType_APPROVE_BUDGET_ACTION,
  ExecutionType_RESET_BUDGET_ACTION,
  ExecutionType_RETRY_BUDGET_ACTION,
  ExecutionType_REVERSE_BUDGET_ACTION,
  ExecutionType'
  #-}
