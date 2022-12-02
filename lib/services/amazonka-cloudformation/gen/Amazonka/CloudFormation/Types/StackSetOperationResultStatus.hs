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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationResultStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationResultStatus
  ( StackSetOperationResultStatus
      ( ..,
        StackSetOperationResultStatus_CANCELLED,
        StackSetOperationResultStatus_FAILED,
        StackSetOperationResultStatus_PENDING,
        StackSetOperationResultStatus_RUNNING,
        StackSetOperationResultStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackSetOperationResultStatus = StackSetOperationResultStatus'
  { fromStackSetOperationResultStatus ::
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

pattern StackSetOperationResultStatus_CANCELLED :: StackSetOperationResultStatus
pattern StackSetOperationResultStatus_CANCELLED = StackSetOperationResultStatus' "CANCELLED"

pattern StackSetOperationResultStatus_FAILED :: StackSetOperationResultStatus
pattern StackSetOperationResultStatus_FAILED = StackSetOperationResultStatus' "FAILED"

pattern StackSetOperationResultStatus_PENDING :: StackSetOperationResultStatus
pattern StackSetOperationResultStatus_PENDING = StackSetOperationResultStatus' "PENDING"

pattern StackSetOperationResultStatus_RUNNING :: StackSetOperationResultStatus
pattern StackSetOperationResultStatus_RUNNING = StackSetOperationResultStatus' "RUNNING"

pattern StackSetOperationResultStatus_SUCCEEDED :: StackSetOperationResultStatus
pattern StackSetOperationResultStatus_SUCCEEDED = StackSetOperationResultStatus' "SUCCEEDED"

{-# COMPLETE
  StackSetOperationResultStatus_CANCELLED,
  StackSetOperationResultStatus_FAILED,
  StackSetOperationResultStatus_PENDING,
  StackSetOperationResultStatus_RUNNING,
  StackSetOperationResultStatus_SUCCEEDED,
  StackSetOperationResultStatus'
  #-}
