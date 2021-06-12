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
-- Module      : Network.AWS.SageMaker.Types.TrainingJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobStatus
  ( TrainingJobStatus
      ( ..,
        TrainingJobStatus_Completed,
        TrainingJobStatus_Failed,
        TrainingJobStatus_InProgress,
        TrainingJobStatus_Stopped,
        TrainingJobStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TrainingJobStatus = TrainingJobStatus'
  { fromTrainingJobStatus ::
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

pattern TrainingJobStatus_Completed :: TrainingJobStatus
pattern TrainingJobStatus_Completed = TrainingJobStatus' "Completed"

pattern TrainingJobStatus_Failed :: TrainingJobStatus
pattern TrainingJobStatus_Failed = TrainingJobStatus' "Failed"

pattern TrainingJobStatus_InProgress :: TrainingJobStatus
pattern TrainingJobStatus_InProgress = TrainingJobStatus' "InProgress"

pattern TrainingJobStatus_Stopped :: TrainingJobStatus
pattern TrainingJobStatus_Stopped = TrainingJobStatus' "Stopped"

pattern TrainingJobStatus_Stopping :: TrainingJobStatus
pattern TrainingJobStatus_Stopping = TrainingJobStatus' "Stopping"

{-# COMPLETE
  TrainingJobStatus_Completed,
  TrainingJobStatus_Failed,
  TrainingJobStatus_InProgress,
  TrainingJobStatus_Stopped,
  TrainingJobStatus_Stopping,
  TrainingJobStatus'
  #-}
