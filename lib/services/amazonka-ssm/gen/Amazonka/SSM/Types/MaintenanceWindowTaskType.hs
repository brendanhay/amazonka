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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTaskType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTaskType
  ( MaintenanceWindowTaskType
      ( ..,
        MaintenanceWindowTaskType_AUTOMATION,
        MaintenanceWindowTaskType_LAMBDA,
        MaintenanceWindowTaskType_RUN_COMMAND,
        MaintenanceWindowTaskType_STEP_FUNCTIONS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType'
  { fromMaintenanceWindowTaskType ::
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

pattern MaintenanceWindowTaskType_AUTOMATION :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskType_AUTOMATION = MaintenanceWindowTaskType' "AUTOMATION"

pattern MaintenanceWindowTaskType_LAMBDA :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskType_LAMBDA = MaintenanceWindowTaskType' "LAMBDA"

pattern MaintenanceWindowTaskType_RUN_COMMAND :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskType_RUN_COMMAND = MaintenanceWindowTaskType' "RUN_COMMAND"

pattern MaintenanceWindowTaskType_STEP_FUNCTIONS :: MaintenanceWindowTaskType
pattern MaintenanceWindowTaskType_STEP_FUNCTIONS = MaintenanceWindowTaskType' "STEP_FUNCTIONS"

{-# COMPLETE
  MaintenanceWindowTaskType_AUTOMATION,
  MaintenanceWindowTaskType_LAMBDA,
  MaintenanceWindowTaskType_RUN_COMMAND,
  MaintenanceWindowTaskType_STEP_FUNCTIONS,
  MaintenanceWindowTaskType'
  #-}
