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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ApplicationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ApplicationStatus
  ( ApplicationStatus
      ( ..,
        ApplicationStatus_AUTOSCALING,
        ApplicationStatus_DELETING,
        ApplicationStatus_FORCE_STOPPING,
        ApplicationStatus_MAINTENANCE,
        ApplicationStatus_READY,
        ApplicationStatus_ROLLED_BACK,
        ApplicationStatus_ROLLING_BACK,
        ApplicationStatus_RUNNING,
        ApplicationStatus_STARTING,
        ApplicationStatus_STOPPING,
        ApplicationStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationStatus = ApplicationStatus'
  { fromApplicationStatus ::
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

pattern ApplicationStatus_AUTOSCALING :: ApplicationStatus
pattern ApplicationStatus_AUTOSCALING = ApplicationStatus' "AUTOSCALING"

pattern ApplicationStatus_DELETING :: ApplicationStatus
pattern ApplicationStatus_DELETING = ApplicationStatus' "DELETING"

pattern ApplicationStatus_FORCE_STOPPING :: ApplicationStatus
pattern ApplicationStatus_FORCE_STOPPING = ApplicationStatus' "FORCE_STOPPING"

pattern ApplicationStatus_MAINTENANCE :: ApplicationStatus
pattern ApplicationStatus_MAINTENANCE = ApplicationStatus' "MAINTENANCE"

pattern ApplicationStatus_READY :: ApplicationStatus
pattern ApplicationStatus_READY = ApplicationStatus' "READY"

pattern ApplicationStatus_ROLLED_BACK :: ApplicationStatus
pattern ApplicationStatus_ROLLED_BACK = ApplicationStatus' "ROLLED_BACK"

pattern ApplicationStatus_ROLLING_BACK :: ApplicationStatus
pattern ApplicationStatus_ROLLING_BACK = ApplicationStatus' "ROLLING_BACK"

pattern ApplicationStatus_RUNNING :: ApplicationStatus
pattern ApplicationStatus_RUNNING = ApplicationStatus' "RUNNING"

pattern ApplicationStatus_STARTING :: ApplicationStatus
pattern ApplicationStatus_STARTING = ApplicationStatus' "STARTING"

pattern ApplicationStatus_STOPPING :: ApplicationStatus
pattern ApplicationStatus_STOPPING = ApplicationStatus' "STOPPING"

pattern ApplicationStatus_UPDATING :: ApplicationStatus
pattern ApplicationStatus_UPDATING = ApplicationStatus' "UPDATING"

{-# COMPLETE
  ApplicationStatus_AUTOSCALING,
  ApplicationStatus_DELETING,
  ApplicationStatus_FORCE_STOPPING,
  ApplicationStatus_MAINTENANCE,
  ApplicationStatus_READY,
  ApplicationStatus_ROLLED_BACK,
  ApplicationStatus_ROLLING_BACK,
  ApplicationStatus_RUNNING,
  ApplicationStatus_STARTING,
  ApplicationStatus_STOPPING,
  ApplicationStatus_UPDATING,
  ApplicationStatus'
  #-}
