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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobStatus
  ( LabelingJobStatus
      ( ..,
        LabelingJobStatus_Completed,
        LabelingJobStatus_Failed,
        LabelingJobStatus_InProgress,
        LabelingJobStatus_Initializing,
        LabelingJobStatus_Stopped,
        LabelingJobStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype LabelingJobStatus = LabelingJobStatus'
  { fromLabelingJobStatus ::
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

pattern LabelingJobStatus_Completed :: LabelingJobStatus
pattern LabelingJobStatus_Completed = LabelingJobStatus' "Completed"

pattern LabelingJobStatus_Failed :: LabelingJobStatus
pattern LabelingJobStatus_Failed = LabelingJobStatus' "Failed"

pattern LabelingJobStatus_InProgress :: LabelingJobStatus
pattern LabelingJobStatus_InProgress = LabelingJobStatus' "InProgress"

pattern LabelingJobStatus_Initializing :: LabelingJobStatus
pattern LabelingJobStatus_Initializing = LabelingJobStatus' "Initializing"

pattern LabelingJobStatus_Stopped :: LabelingJobStatus
pattern LabelingJobStatus_Stopped = LabelingJobStatus' "Stopped"

pattern LabelingJobStatus_Stopping :: LabelingJobStatus
pattern LabelingJobStatus_Stopping = LabelingJobStatus' "Stopping"

{-# COMPLETE
  LabelingJobStatus_Completed,
  LabelingJobStatus_Failed,
  LabelingJobStatus_InProgress,
  LabelingJobStatus_Initializing,
  LabelingJobStatus_Stopped,
  LabelingJobStatus_Stopping,
  LabelingJobStatus'
  #-}
