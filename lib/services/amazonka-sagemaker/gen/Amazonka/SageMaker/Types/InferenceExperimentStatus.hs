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
-- Module      : Amazonka.SageMaker.Types.InferenceExperimentStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceExperimentStatus
  ( InferenceExperimentStatus
      ( ..,
        InferenceExperimentStatus_Cancelled,
        InferenceExperimentStatus_Completed,
        InferenceExperimentStatus_Created,
        InferenceExperimentStatus_Creating,
        InferenceExperimentStatus_Running,
        InferenceExperimentStatus_Starting,
        InferenceExperimentStatus_Stopping,
        InferenceExperimentStatus_Updating
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InferenceExperimentStatus = InferenceExperimentStatus'
  { fromInferenceExperimentStatus ::
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

pattern InferenceExperimentStatus_Cancelled :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Cancelled = InferenceExperimentStatus' "Cancelled"

pattern InferenceExperimentStatus_Completed :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Completed = InferenceExperimentStatus' "Completed"

pattern InferenceExperimentStatus_Created :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Created = InferenceExperimentStatus' "Created"

pattern InferenceExperimentStatus_Creating :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Creating = InferenceExperimentStatus' "Creating"

pattern InferenceExperimentStatus_Running :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Running = InferenceExperimentStatus' "Running"

pattern InferenceExperimentStatus_Starting :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Starting = InferenceExperimentStatus' "Starting"

pattern InferenceExperimentStatus_Stopping :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Stopping = InferenceExperimentStatus' "Stopping"

pattern InferenceExperimentStatus_Updating :: InferenceExperimentStatus
pattern InferenceExperimentStatus_Updating = InferenceExperimentStatus' "Updating"

{-# COMPLETE
  InferenceExperimentStatus_Cancelled,
  InferenceExperimentStatus_Completed,
  InferenceExperimentStatus_Created,
  InferenceExperimentStatus_Creating,
  InferenceExperimentStatus_Running,
  InferenceExperimentStatus_Starting,
  InferenceExperimentStatus_Stopping,
  InferenceExperimentStatus_Updating,
  InferenceExperimentStatus'
  #-}
