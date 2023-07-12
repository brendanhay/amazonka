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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceSchedulerStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceSchedulerStatus
  ( InferenceSchedulerStatus
      ( ..,
        InferenceSchedulerStatus_PENDING,
        InferenceSchedulerStatus_RUNNING,
        InferenceSchedulerStatus_STOPPED,
        InferenceSchedulerStatus_STOPPING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InferenceSchedulerStatus = InferenceSchedulerStatus'
  { fromInferenceSchedulerStatus ::
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

pattern InferenceSchedulerStatus_PENDING :: InferenceSchedulerStatus
pattern InferenceSchedulerStatus_PENDING = InferenceSchedulerStatus' "PENDING"

pattern InferenceSchedulerStatus_RUNNING :: InferenceSchedulerStatus
pattern InferenceSchedulerStatus_RUNNING = InferenceSchedulerStatus' "RUNNING"

pattern InferenceSchedulerStatus_STOPPED :: InferenceSchedulerStatus
pattern InferenceSchedulerStatus_STOPPED = InferenceSchedulerStatus' "STOPPED"

pattern InferenceSchedulerStatus_STOPPING :: InferenceSchedulerStatus
pattern InferenceSchedulerStatus_STOPPING = InferenceSchedulerStatus' "STOPPING"

{-# COMPLETE
  InferenceSchedulerStatus_PENDING,
  InferenceSchedulerStatus_RUNNING,
  InferenceSchedulerStatus_STOPPED,
  InferenceSchedulerStatus_STOPPING,
  InferenceSchedulerStatus'
  #-}
