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
-- Module      : Amazonka.Rekognition.Types.ProjectVersionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProjectVersionStatus
  ( ProjectVersionStatus
      ( ..,
        ProjectVersionStatus_COPYING_COMPLETED,
        ProjectVersionStatus_COPYING_FAILED,
        ProjectVersionStatus_COPYING_IN_PROGRESS,
        ProjectVersionStatus_DELETING,
        ProjectVersionStatus_FAILED,
        ProjectVersionStatus_RUNNING,
        ProjectVersionStatus_STARTING,
        ProjectVersionStatus_STOPPED,
        ProjectVersionStatus_STOPPING,
        ProjectVersionStatus_TRAINING_COMPLETED,
        ProjectVersionStatus_TRAINING_FAILED,
        ProjectVersionStatus_TRAINING_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProjectVersionStatus = ProjectVersionStatus'
  { fromProjectVersionStatus ::
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

pattern ProjectVersionStatus_COPYING_COMPLETED :: ProjectVersionStatus
pattern ProjectVersionStatus_COPYING_COMPLETED = ProjectVersionStatus' "COPYING_COMPLETED"

pattern ProjectVersionStatus_COPYING_FAILED :: ProjectVersionStatus
pattern ProjectVersionStatus_COPYING_FAILED = ProjectVersionStatus' "COPYING_FAILED"

pattern ProjectVersionStatus_COPYING_IN_PROGRESS :: ProjectVersionStatus
pattern ProjectVersionStatus_COPYING_IN_PROGRESS = ProjectVersionStatus' "COPYING_IN_PROGRESS"

pattern ProjectVersionStatus_DELETING :: ProjectVersionStatus
pattern ProjectVersionStatus_DELETING = ProjectVersionStatus' "DELETING"

pattern ProjectVersionStatus_FAILED :: ProjectVersionStatus
pattern ProjectVersionStatus_FAILED = ProjectVersionStatus' "FAILED"

pattern ProjectVersionStatus_RUNNING :: ProjectVersionStatus
pattern ProjectVersionStatus_RUNNING = ProjectVersionStatus' "RUNNING"

pattern ProjectVersionStatus_STARTING :: ProjectVersionStatus
pattern ProjectVersionStatus_STARTING = ProjectVersionStatus' "STARTING"

pattern ProjectVersionStatus_STOPPED :: ProjectVersionStatus
pattern ProjectVersionStatus_STOPPED = ProjectVersionStatus' "STOPPED"

pattern ProjectVersionStatus_STOPPING :: ProjectVersionStatus
pattern ProjectVersionStatus_STOPPING = ProjectVersionStatus' "STOPPING"

pattern ProjectVersionStatus_TRAINING_COMPLETED :: ProjectVersionStatus
pattern ProjectVersionStatus_TRAINING_COMPLETED = ProjectVersionStatus' "TRAINING_COMPLETED"

pattern ProjectVersionStatus_TRAINING_FAILED :: ProjectVersionStatus
pattern ProjectVersionStatus_TRAINING_FAILED = ProjectVersionStatus' "TRAINING_FAILED"

pattern ProjectVersionStatus_TRAINING_IN_PROGRESS :: ProjectVersionStatus
pattern ProjectVersionStatus_TRAINING_IN_PROGRESS = ProjectVersionStatus' "TRAINING_IN_PROGRESS"

{-# COMPLETE
  ProjectVersionStatus_COPYING_COMPLETED,
  ProjectVersionStatus_COPYING_FAILED,
  ProjectVersionStatus_COPYING_IN_PROGRESS,
  ProjectVersionStatus_DELETING,
  ProjectVersionStatus_FAILED,
  ProjectVersionStatus_RUNNING,
  ProjectVersionStatus_STARTING,
  ProjectVersionStatus_STOPPED,
  ProjectVersionStatus_STOPPING,
  ProjectVersionStatus_TRAINING_COMPLETED,
  ProjectVersionStatus_TRAINING_FAILED,
  ProjectVersionStatus_TRAINING_IN_PROGRESS,
  ProjectVersionStatus'
  #-}
