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
-- Module      : Amazonka.KendraRanking.Types.RescoreExecutionPlanStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Types.RescoreExecutionPlanStatus
  ( RescoreExecutionPlanStatus
      ( ..,
        RescoreExecutionPlanStatus_ACTIVE,
        RescoreExecutionPlanStatus_CREATING,
        RescoreExecutionPlanStatus_DELETING,
        RescoreExecutionPlanStatus_FAILED,
        RescoreExecutionPlanStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RescoreExecutionPlanStatus = RescoreExecutionPlanStatus'
  { fromRescoreExecutionPlanStatus ::
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

pattern RescoreExecutionPlanStatus_ACTIVE :: RescoreExecutionPlanStatus
pattern RescoreExecutionPlanStatus_ACTIVE = RescoreExecutionPlanStatus' "ACTIVE"

pattern RescoreExecutionPlanStatus_CREATING :: RescoreExecutionPlanStatus
pattern RescoreExecutionPlanStatus_CREATING = RescoreExecutionPlanStatus' "CREATING"

pattern RescoreExecutionPlanStatus_DELETING :: RescoreExecutionPlanStatus
pattern RescoreExecutionPlanStatus_DELETING = RescoreExecutionPlanStatus' "DELETING"

pattern RescoreExecutionPlanStatus_FAILED :: RescoreExecutionPlanStatus
pattern RescoreExecutionPlanStatus_FAILED = RescoreExecutionPlanStatus' "FAILED"

pattern RescoreExecutionPlanStatus_UPDATING :: RescoreExecutionPlanStatus
pattern RescoreExecutionPlanStatus_UPDATING = RescoreExecutionPlanStatus' "UPDATING"

{-# COMPLETE
  RescoreExecutionPlanStatus_ACTIVE,
  RescoreExecutionPlanStatus_CREATING,
  RescoreExecutionPlanStatus_DELETING,
  RescoreExecutionPlanStatus_FAILED,
  RescoreExecutionPlanStatus_UPDATING,
  RescoreExecutionPlanStatus'
  #-}
