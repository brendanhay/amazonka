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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.WorkflowStepType
  ( WorkflowStepType
      ( ..,
        WorkflowStepType_COPY,
        WorkflowStepType_CUSTOM,
        WorkflowStepType_DECRYPT,
        WorkflowStepType_DELETE,
        WorkflowStepType_TAG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowStepType = WorkflowStepType'
  { fromWorkflowStepType ::
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

pattern WorkflowStepType_COPY :: WorkflowStepType
pattern WorkflowStepType_COPY = WorkflowStepType' "COPY"

pattern WorkflowStepType_CUSTOM :: WorkflowStepType
pattern WorkflowStepType_CUSTOM = WorkflowStepType' "CUSTOM"

pattern WorkflowStepType_DECRYPT :: WorkflowStepType
pattern WorkflowStepType_DECRYPT = WorkflowStepType' "DECRYPT"

pattern WorkflowStepType_DELETE :: WorkflowStepType
pattern WorkflowStepType_DELETE = WorkflowStepType' "DELETE"

pattern WorkflowStepType_TAG :: WorkflowStepType
pattern WorkflowStepType_TAG = WorkflowStepType' "TAG"

{-# COMPLETE
  WorkflowStepType_COPY,
  WorkflowStepType_CUSTOM,
  WorkflowStepType_DECRYPT,
  WorkflowStepType_DELETE,
  WorkflowStepType_TAG,
  WorkflowStepType'
  #-}
