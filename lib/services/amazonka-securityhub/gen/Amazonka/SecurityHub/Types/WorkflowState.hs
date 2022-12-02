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
-- Module      : Amazonka.SecurityHub.Types.WorkflowState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.WorkflowState
  ( WorkflowState
      ( ..,
        WorkflowState_ASSIGNED,
        WorkflowState_DEFERRED,
        WorkflowState_IN_PROGRESS,
        WorkflowState_NEW,
        WorkflowState_RESOLVED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowState = WorkflowState'
  { fromWorkflowState ::
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

pattern WorkflowState_ASSIGNED :: WorkflowState
pattern WorkflowState_ASSIGNED = WorkflowState' "ASSIGNED"

pattern WorkflowState_DEFERRED :: WorkflowState
pattern WorkflowState_DEFERRED = WorkflowState' "DEFERRED"

pattern WorkflowState_IN_PROGRESS :: WorkflowState
pattern WorkflowState_IN_PROGRESS = WorkflowState' "IN_PROGRESS"

pattern WorkflowState_NEW :: WorkflowState
pattern WorkflowState_NEW = WorkflowState' "NEW"

pattern WorkflowState_RESOLVED :: WorkflowState
pattern WorkflowState_RESOLVED = WorkflowState' "RESOLVED"

{-# COMPLETE
  WorkflowState_ASSIGNED,
  WorkflowState_DEFERRED,
  WorkflowState_IN_PROGRESS,
  WorkflowState_NEW,
  WorkflowState_RESOLVED,
  WorkflowState'
  #-}
