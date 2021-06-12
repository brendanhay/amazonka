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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobStatus
  ( HyperParameterTuningJobStatus
      ( ..,
        HyperParameterTuningJobStatus_Completed,
        HyperParameterTuningJobStatus_Failed,
        HyperParameterTuningJobStatus_InProgress,
        HyperParameterTuningJobStatus_Stopped,
        HyperParameterTuningJobStatus_Stopping
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype HyperParameterTuningJobStatus = HyperParameterTuningJobStatus'
  { fromHyperParameterTuningJobStatus ::
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

pattern HyperParameterTuningJobStatus_Completed :: HyperParameterTuningJobStatus
pattern HyperParameterTuningJobStatus_Completed = HyperParameterTuningJobStatus' "Completed"

pattern HyperParameterTuningJobStatus_Failed :: HyperParameterTuningJobStatus
pattern HyperParameterTuningJobStatus_Failed = HyperParameterTuningJobStatus' "Failed"

pattern HyperParameterTuningJobStatus_InProgress :: HyperParameterTuningJobStatus
pattern HyperParameterTuningJobStatus_InProgress = HyperParameterTuningJobStatus' "InProgress"

pattern HyperParameterTuningJobStatus_Stopped :: HyperParameterTuningJobStatus
pattern HyperParameterTuningJobStatus_Stopped = HyperParameterTuningJobStatus' "Stopped"

pattern HyperParameterTuningJobStatus_Stopping :: HyperParameterTuningJobStatus
pattern HyperParameterTuningJobStatus_Stopping = HyperParameterTuningJobStatus' "Stopping"

{-# COMPLETE
  HyperParameterTuningJobStatus_Completed,
  HyperParameterTuningJobStatus_Failed,
  HyperParameterTuningJobStatus_InProgress,
  HyperParameterTuningJobStatus_Stopped,
  HyperParameterTuningJobStatus_Stopping,
  HyperParameterTuningJobStatus'
  #-}
