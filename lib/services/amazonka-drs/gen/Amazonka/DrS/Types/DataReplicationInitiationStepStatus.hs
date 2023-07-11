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
-- Module      : Amazonka.DrS.Types.DataReplicationInitiationStepStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.DataReplicationInitiationStepStatus
  ( DataReplicationInitiationStepStatus
      ( ..,
        DataReplicationInitiationStepStatus_FAILED,
        DataReplicationInitiationStepStatus_IN_PROGRESS,
        DataReplicationInitiationStepStatus_NOT_STARTED,
        DataReplicationInitiationStepStatus_SKIPPED,
        DataReplicationInitiationStepStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataReplicationInitiationStepStatus = DataReplicationInitiationStepStatus'
  { fromDataReplicationInitiationStepStatus ::
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

pattern DataReplicationInitiationStepStatus_FAILED :: DataReplicationInitiationStepStatus
pattern DataReplicationInitiationStepStatus_FAILED = DataReplicationInitiationStepStatus' "FAILED"

pattern DataReplicationInitiationStepStatus_IN_PROGRESS :: DataReplicationInitiationStepStatus
pattern DataReplicationInitiationStepStatus_IN_PROGRESS = DataReplicationInitiationStepStatus' "IN_PROGRESS"

pattern DataReplicationInitiationStepStatus_NOT_STARTED :: DataReplicationInitiationStepStatus
pattern DataReplicationInitiationStepStatus_NOT_STARTED = DataReplicationInitiationStepStatus' "NOT_STARTED"

pattern DataReplicationInitiationStepStatus_SKIPPED :: DataReplicationInitiationStepStatus
pattern DataReplicationInitiationStepStatus_SKIPPED = DataReplicationInitiationStepStatus' "SKIPPED"

pattern DataReplicationInitiationStepStatus_SUCCEEDED :: DataReplicationInitiationStepStatus
pattern DataReplicationInitiationStepStatus_SUCCEEDED = DataReplicationInitiationStepStatus' "SUCCEEDED"

{-# COMPLETE
  DataReplicationInitiationStepStatus_FAILED,
  DataReplicationInitiationStepStatus_IN_PROGRESS,
  DataReplicationInitiationStepStatus_NOT_STARTED,
  DataReplicationInitiationStepStatus_SKIPPED,
  DataReplicationInitiationStepStatus_SUCCEEDED,
  DataReplicationInitiationStepStatus'
  #-}
