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
-- Module      : Amazonka.Evidently.Types.ExperimentStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentStatus
  ( ExperimentStatus
      ( ..,
        ExperimentStatus_CANCELLED,
        ExperimentStatus_COMPLETED,
        ExperimentStatus_CREATED,
        ExperimentStatus_RUNNING,
        ExperimentStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExperimentStatus = ExperimentStatus'
  { fromExperimentStatus ::
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

pattern ExperimentStatus_CANCELLED :: ExperimentStatus
pattern ExperimentStatus_CANCELLED = ExperimentStatus' "CANCELLED"

pattern ExperimentStatus_COMPLETED :: ExperimentStatus
pattern ExperimentStatus_COMPLETED = ExperimentStatus' "COMPLETED"

pattern ExperimentStatus_CREATED :: ExperimentStatus
pattern ExperimentStatus_CREATED = ExperimentStatus' "CREATED"

pattern ExperimentStatus_RUNNING :: ExperimentStatus
pattern ExperimentStatus_RUNNING = ExperimentStatus' "RUNNING"

pattern ExperimentStatus_UPDATING :: ExperimentStatus
pattern ExperimentStatus_UPDATING = ExperimentStatus' "UPDATING"

{-# COMPLETE
  ExperimentStatus_CANCELLED,
  ExperimentStatus_COMPLETED,
  ExperimentStatus_CREATED,
  ExperimentStatus_RUNNING,
  ExperimentStatus_UPDATING,
  ExperimentStatus'
  #-}
