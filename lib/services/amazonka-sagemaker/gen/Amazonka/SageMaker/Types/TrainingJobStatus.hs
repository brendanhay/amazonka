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
-- Module      : Amazonka.SageMaker.Types.TrainingJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJobStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TrainingJobStatus = TrainingJobStatus'
  { fromTrainingJobStatus ::
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
