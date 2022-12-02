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
-- Module      : Amazonka.SSM.Types.MaintenanceWindowTaskCutoffBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.MaintenanceWindowTaskCutoffBehavior
  ( MaintenanceWindowTaskCutoffBehavior
      ( ..,
        MaintenanceWindowTaskCutoffBehavior_CANCEL_TASK,
        MaintenanceWindowTaskCutoffBehavior_CONTINUE_TASK
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MaintenanceWindowTaskCutoffBehavior = MaintenanceWindowTaskCutoffBehavior'
  { fromMaintenanceWindowTaskCutoffBehavior ::
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

pattern MaintenanceWindowTaskCutoffBehavior_CANCEL_TASK :: MaintenanceWindowTaskCutoffBehavior
pattern MaintenanceWindowTaskCutoffBehavior_CANCEL_TASK = MaintenanceWindowTaskCutoffBehavior' "CANCEL_TASK"

pattern MaintenanceWindowTaskCutoffBehavior_CONTINUE_TASK :: MaintenanceWindowTaskCutoffBehavior
pattern MaintenanceWindowTaskCutoffBehavior_CONTINUE_TASK = MaintenanceWindowTaskCutoffBehavior' "CONTINUE_TASK"

{-# COMPLETE
  MaintenanceWindowTaskCutoffBehavior_CANCEL_TASK,
  MaintenanceWindowTaskCutoffBehavior_CONTINUE_TASK,
  MaintenanceWindowTaskCutoffBehavior'
  #-}
