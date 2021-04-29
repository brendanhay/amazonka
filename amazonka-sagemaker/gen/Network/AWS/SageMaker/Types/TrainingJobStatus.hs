{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype TrainingJobStatus = TrainingJobStatus'
  { fromTrainingJobStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
