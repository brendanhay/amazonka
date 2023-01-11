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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HyperParameterTuningJobStatus = HyperParameterTuningJobStatus'
  { fromHyperParameterTuningJobStatus ::
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
