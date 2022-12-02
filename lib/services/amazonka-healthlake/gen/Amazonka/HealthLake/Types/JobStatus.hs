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
-- Module      : Amazonka.HealthLake.Types.JobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.JobStatus
  ( JobStatus
      ( ..,
        JobStatus_COMPLETED,
        JobStatus_COMPLETED_WITH_ERRORS,
        JobStatus_FAILED,
        JobStatus_IN_PROGRESS,
        JobStatus_SUBMITTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobStatus = JobStatus'
  { fromJobStatus ::
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

pattern JobStatus_COMPLETED :: JobStatus
pattern JobStatus_COMPLETED = JobStatus' "COMPLETED"

pattern JobStatus_COMPLETED_WITH_ERRORS :: JobStatus
pattern JobStatus_COMPLETED_WITH_ERRORS = JobStatus' "COMPLETED_WITH_ERRORS"

pattern JobStatus_FAILED :: JobStatus
pattern JobStatus_FAILED = JobStatus' "FAILED"

pattern JobStatus_IN_PROGRESS :: JobStatus
pattern JobStatus_IN_PROGRESS = JobStatus' "IN_PROGRESS"

pattern JobStatus_SUBMITTED :: JobStatus
pattern JobStatus_SUBMITTED = JobStatus' "SUBMITTED"

{-# COMPLETE
  JobStatus_COMPLETED,
  JobStatus_COMPLETED_WITH_ERRORS,
  JobStatus_FAILED,
  JobStatus_IN_PROGRESS,
  JobStatus_SUBMITTED,
  JobStatus'
  #-}
