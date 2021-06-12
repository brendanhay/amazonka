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
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskType
  ( MaintenanceWindowTaskType
      ( ..,
        MaintenanceWindowTaskType_AUTOMATION,
        MaintenanceWindowTaskType_LAMBDA,
        MaintenanceWindowTaskType_RUN_COMMAND,
        MaintenanceWindowTaskType_STEP_FUNCTIONS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MaintenanceWindowTaskType = MaintenanceWindowTaskType'
  { fromMaintenanceWindowTaskType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
