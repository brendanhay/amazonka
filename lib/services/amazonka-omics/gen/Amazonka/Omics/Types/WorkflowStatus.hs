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
-- Module      : Amazonka.Omics.Types.WorkflowStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.WorkflowStatus
  ( WorkflowStatus
      ( ..,
        WorkflowStatus_ACTIVE,
        WorkflowStatus_CREATING,
        WorkflowStatus_DELETED,
        WorkflowStatus_FAILED,
        WorkflowStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorkflowStatus = WorkflowStatus'
  { fromWorkflowStatus ::
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

pattern WorkflowStatus_ACTIVE :: WorkflowStatus
pattern WorkflowStatus_ACTIVE = WorkflowStatus' "ACTIVE"

pattern WorkflowStatus_CREATING :: WorkflowStatus
pattern WorkflowStatus_CREATING = WorkflowStatus' "CREATING"

pattern WorkflowStatus_DELETED :: WorkflowStatus
pattern WorkflowStatus_DELETED = WorkflowStatus' "DELETED"

pattern WorkflowStatus_FAILED :: WorkflowStatus
pattern WorkflowStatus_FAILED = WorkflowStatus' "FAILED"

pattern WorkflowStatus_UPDATING :: WorkflowStatus
pattern WorkflowStatus_UPDATING = WorkflowStatus' "UPDATING"

{-# COMPLETE
  WorkflowStatus_ACTIVE,
  WorkflowStatus_CREATING,
  WorkflowStatus_DELETED,
  WorkflowStatus_FAILED,
  WorkflowStatus_UPDATING,
  WorkflowStatus'
  #-}
