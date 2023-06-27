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
-- Module      : Amazonka.GreengrassV2.Types.EffectiveDeploymentExecutionStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.EffectiveDeploymentExecutionStatus
  ( EffectiveDeploymentExecutionStatus
      ( ..,
        EffectiveDeploymentExecutionStatus_CANCELED,
        EffectiveDeploymentExecutionStatus_COMPLETED,
        EffectiveDeploymentExecutionStatus_FAILED,
        EffectiveDeploymentExecutionStatus_IN_PROGRESS,
        EffectiveDeploymentExecutionStatus_QUEUED,
        EffectiveDeploymentExecutionStatus_REJECTED,
        EffectiveDeploymentExecutionStatus_SUCCEEDED,
        EffectiveDeploymentExecutionStatus_TIMED_OUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EffectiveDeploymentExecutionStatus = EffectiveDeploymentExecutionStatus'
  { fromEffectiveDeploymentExecutionStatus ::
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

pattern EffectiveDeploymentExecutionStatus_CANCELED :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_CANCELED = EffectiveDeploymentExecutionStatus' "CANCELED"

pattern EffectiveDeploymentExecutionStatus_COMPLETED :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_COMPLETED = EffectiveDeploymentExecutionStatus' "COMPLETED"

pattern EffectiveDeploymentExecutionStatus_FAILED :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_FAILED = EffectiveDeploymentExecutionStatus' "FAILED"

pattern EffectiveDeploymentExecutionStatus_IN_PROGRESS :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_IN_PROGRESS = EffectiveDeploymentExecutionStatus' "IN_PROGRESS"

pattern EffectiveDeploymentExecutionStatus_QUEUED :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_QUEUED = EffectiveDeploymentExecutionStatus' "QUEUED"

pattern EffectiveDeploymentExecutionStatus_REJECTED :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_REJECTED = EffectiveDeploymentExecutionStatus' "REJECTED"

pattern EffectiveDeploymentExecutionStatus_SUCCEEDED :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_SUCCEEDED = EffectiveDeploymentExecutionStatus' "SUCCEEDED"

pattern EffectiveDeploymentExecutionStatus_TIMED_OUT :: EffectiveDeploymentExecutionStatus
pattern EffectiveDeploymentExecutionStatus_TIMED_OUT = EffectiveDeploymentExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  EffectiveDeploymentExecutionStatus_CANCELED,
  EffectiveDeploymentExecutionStatus_COMPLETED,
  EffectiveDeploymentExecutionStatus_FAILED,
  EffectiveDeploymentExecutionStatus_IN_PROGRESS,
  EffectiveDeploymentExecutionStatus_QUEUED,
  EffectiveDeploymentExecutionStatus_REJECTED,
  EffectiveDeploymentExecutionStatus_SUCCEEDED,
  EffectiveDeploymentExecutionStatus_TIMED_OUT,
  EffectiveDeploymentExecutionStatus'
  #-}
