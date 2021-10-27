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
-- Module      : Network.AWS.SecurityHub.Types.WorkflowStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.WorkflowStatus
  ( WorkflowStatus
      ( ..,
        WorkflowStatus_NEW,
        WorkflowStatus_NOTIFIED,
        WorkflowStatus_RESOLVED,
        WorkflowStatus_SUPPRESSED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype WorkflowStatus = WorkflowStatus'
  { fromWorkflowStatus ::
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
