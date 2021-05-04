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

import qualified Network.AWS.Prelude as Prelude

newtype HyperParameterTuningJobStatus = HyperParameterTuningJobStatus'
  { fromHyperParameterTuningJobStatus ::
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
