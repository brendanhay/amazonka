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
-- Module      : Amazonka.Budgets.Types.ExecutionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.ExecutionType
  ( ExecutionType
      ( ..,
        ExecutionType_APPROVE_BUDGET_ACTION,
        ExecutionType_RESET_BUDGET_ACTION,
        ExecutionType_RETRY_BUDGET_ACTION,
        ExecutionType_REVERSE_BUDGET_ACTION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ExecutionType = ExecutionType'
  { fromExecutionType ::
      Core.Text
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
