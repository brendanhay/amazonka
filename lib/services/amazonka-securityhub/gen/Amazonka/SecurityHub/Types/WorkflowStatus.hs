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
-- Module      : Amazonka.SecurityHub.Types.WorkflowStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.WorkflowStatus
  ( WorkflowStatus
      ( ..,
        WorkflowStatus_NEW,
        WorkflowStatus_NOTIFIED,
        WorkflowStatus_RESOLVED,
        WorkflowStatus_SUPPRESSED
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

pattern WorkflowStatus_NEW :: WorkflowStatus
pattern WorkflowStatus_NEW = WorkflowStatus' "NEW"

pattern WorkflowStatus_NOTIFIED :: WorkflowStatus
pattern WorkflowStatus_NOTIFIED = WorkflowStatus' "NOTIFIED"

pattern WorkflowStatus_RESOLVED :: WorkflowStatus
pattern WorkflowStatus_RESOLVED = WorkflowStatus' "RESOLVED"

pattern WorkflowStatus_SUPPRESSED :: WorkflowStatus
pattern WorkflowStatus_SUPPRESSED = WorkflowStatus' "SUPPRESSED"

{-# COMPLETE
  WorkflowStatus_NEW,
  WorkflowStatus_NOTIFIED,
  WorkflowStatus_RESOLVED,
  WorkflowStatus_SUPPRESSED,
  WorkflowStatus'
  #-}
