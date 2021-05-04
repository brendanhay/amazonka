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
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStatus
  ( ProcessingJobStatus
      ( ..,
        ProcessingJobStatus_Completed,
        ProcessingJobStatus_Failed,
        ProcessingJobStatus_InProgress,
        ProcessingJobStatus_Stopped,
        ProcessingJobStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ProcessingJobStatus = ProcessingJobStatus'
  { fromProcessingJobStatus ::
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

pattern ProcessingJobStatus_Completed :: ProcessingJobStatus
pattern ProcessingJobStatus_Completed = ProcessingJobStatus' "Completed"

pattern ProcessingJobStatus_Failed :: ProcessingJobStatus
pattern ProcessingJobStatus_Failed = ProcessingJobStatus' "Failed"

pattern ProcessingJobStatus_InProgress :: ProcessingJobStatus
pattern ProcessingJobStatus_InProgress = ProcessingJobStatus' "InProgress"

pattern ProcessingJobStatus_Stopped :: ProcessingJobStatus
pattern ProcessingJobStatus_Stopped = ProcessingJobStatus' "Stopped"

pattern ProcessingJobStatus_Stopping :: ProcessingJobStatus
pattern ProcessingJobStatus_Stopping = ProcessingJobStatus' "Stopping"

{-# COMPLETE
  ProcessingJobStatus_Completed,
  ProcessingJobStatus_Failed,
  ProcessingJobStatus_InProgress,
  ProcessingJobStatus_Stopped,
  ProcessingJobStatus_Stopping,
  ProcessingJobStatus'
  #-}
