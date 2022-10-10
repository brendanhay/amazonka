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
-- Module      : Amazonka.Transfer.Types.WorkflowStepType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.WorkflowStepType
  ( WorkflowStepType
      ( ..,
        WorkflowStepType_COPY,
        WorkflowStepType_CUSTOM,
        WorkflowStepType_DELETE,
        WorkflowStepType_TAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype WorkflowStepType = WorkflowStepType'
  { fromWorkflowStepType ::
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

pattern WorkflowStepType_COPY :: WorkflowStepType
pattern WorkflowStepType_COPY = WorkflowStepType' "COPY"

pattern WorkflowStepType_CUSTOM :: WorkflowStepType
pattern WorkflowStepType_CUSTOM = WorkflowStepType' "CUSTOM"

pattern WorkflowStepType_DELETE :: WorkflowStepType
pattern WorkflowStepType_DELETE = WorkflowStepType' "DELETE"

pattern WorkflowStepType_TAG :: WorkflowStepType
pattern WorkflowStepType_TAG = WorkflowStepType' "TAG"

{-# COMPLETE
  WorkflowStepType_COPY,
  WorkflowStepType_CUSTOM,
  WorkflowStepType_DELETE,
  WorkflowStepType_TAG,
  WorkflowStepType'
  #-}
