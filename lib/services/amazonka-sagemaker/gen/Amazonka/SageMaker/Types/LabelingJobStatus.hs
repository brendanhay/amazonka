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
-- Module      : Amazonka.SageMaker.Types.LabelingJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LabelingJobStatus = LabelingJobStatus'
  { fromLabelingJobStatus ::
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
