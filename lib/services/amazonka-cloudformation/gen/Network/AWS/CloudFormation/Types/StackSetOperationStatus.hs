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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationStatus
  ( StackSetOperationStatus
      ( ..,
        StackSetOperationStatus_FAILED,
        StackSetOperationStatus_QUEUED,
        StackSetOperationStatus_RUNNING,
        StackSetOperationStatus_STOPPED,
        StackSetOperationStatus_STOPPING,
        StackSetOperationStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StackSetOperationStatus = StackSetOperationStatus'
  { fromStackSetOperationStatus ::
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

pattern StackSetOperationStatus_FAILED :: StackSetOperationStatus
pattern StackSetOperationStatus_FAILED = StackSetOperationStatus' "FAILED"

pattern StackSetOperationStatus_QUEUED :: StackSetOperationStatus
pattern StackSetOperationStatus_QUEUED = StackSetOperationStatus' "QUEUED"

pattern StackSetOperationStatus_RUNNING :: StackSetOperationStatus
pattern StackSetOperationStatus_RUNNING = StackSetOperationStatus' "RUNNING"

pattern StackSetOperationStatus_STOPPED :: StackSetOperationStatus
pattern StackSetOperationStatus_STOPPED = StackSetOperationStatus' "STOPPED"

pattern StackSetOperationStatus_STOPPING :: StackSetOperationStatus
pattern StackSetOperationStatus_STOPPING = StackSetOperationStatus' "STOPPING"

pattern StackSetOperationStatus_SUCCEEDED :: StackSetOperationStatus
pattern StackSetOperationStatus_SUCCEEDED = StackSetOperationStatus' "SUCCEEDED"

{-# COMPLETE
  StackSetOperationStatus_FAILED,
  StackSetOperationStatus_QUEUED,
  StackSetOperationStatus_RUNNING,
  StackSetOperationStatus_STOPPED,
  StackSetOperationStatus_STOPPING,
  StackSetOperationStatus_SUCCEEDED,
  StackSetOperationStatus'
  #-}
