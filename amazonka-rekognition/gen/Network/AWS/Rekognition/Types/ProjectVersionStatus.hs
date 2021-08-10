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
-- Module      : Network.AWS.Rekognition.Types.ProjectVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectVersionStatus
  ( ProjectVersionStatus
      ( ..,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ProjectVersionStatus = ProjectVersionStatus'
  { fromProjectVersionStatus ::
      Core.Text
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
