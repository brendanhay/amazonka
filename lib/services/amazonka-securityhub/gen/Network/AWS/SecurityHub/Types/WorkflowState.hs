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
-- Module      : Network.AWS.SecurityHub.Types.WorkflowState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.WorkflowState
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype WorkflowState = WorkflowState'
  { fromWorkflowState ::
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
